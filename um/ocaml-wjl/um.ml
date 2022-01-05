exception Error of string (* internal error *)
exception Fail of string (* machine failure *)

type word = int32
let word_to_int = Int32.to_int
let int_to_word = Int32.of_int

type freelist = word Queue.t * word Stream.t (* queue is for free'd slots *)

let freelist_next (queue, stream) =
    try Queue.take queue with Queue.Empty ->
        try Stream.next stream with Stream.Failure ->
            raise (Fail "out of memory")

let freelist_cons (queue, stream) id =
    Queue.add id queue

type state = { memory: (word, word array) Hashtbl.t;
               freelist: freelist;
               mutable ip: int;
               registers: word array; (* length = 8 *) }

let initial_state () =
    let memory = Hashtbl.create 15 in
    let () = Hashtbl.add memory 0l [| |] in (* XXX program? *)
    { memory = memory;
      freelist = (Queue.create (),
                  Stream.from (fun n -> Some (int_to_word (n + 1))));
      ip = 0;
      registers = Array.make 8 0l; }

type regs = { a: int; b: int; c: int }

type instruction =
    | Icmov of regs
    | Iarray_index of regs
    | Iarray_update of regs
    | Iadd of regs
    | Imul of regs
    | Idiv of regs
    | Inand of regs
    | Ihalt
    | Iarray_alloc of int * int (* registers B, C *)
    | Iarray_free of int (* register C *)
    | Ioutput of int (* register C *)
    | Iinput of int (* register C *)
    | Iload_program of int * int  (* registers B, C *)
    | Iload_immediate of int * word (* register A, value *)

(* word_to_bits : word -> bool list *)
let word_to_bits word =
    let rec w2b_aux word acc =
        if word = 0l then
            acc
        else
            let bit = Int32.logand word 1l = 1l in
            w2b_aux (Int32.shift_right_logical word 1) (bit :: acc)
    in
    w2b_aux word []

let pad bits length =
    let rec pad_aux bits n =
        if n = 0 then
            bits
        else
            pad_aux (false :: bits) (n - 1)
    in
    let current_length = List.length bits in
    let n = length - current_length in
    if n < 0 then
        raise (Error "can't pad to a smaller size")
    else
        pad_aux bits (length - current_length)

(* bits_to_int : bool list -> int *)
let bits_to_int bits =
    let () = if List.length bits > 31 then raise (Error "bits_to_int") in
    let rec b2i_aux bits acc =
        match bits with
            | [] -> acc
            | b::bs -> b2i_aux bs ((if b then 1 else 0) + (acc lsl 1))
    in
    b2i_aux bits 0

let bits_to_word bits =
    let () = if List.length bits > 32 then raise (Error "bits_to_word") in
    let (+) = Int32.add in
    let (lsl) = Int32.shift_left in
    let rec b2i_aux bits acc =
        match bits with
            | [] -> acc
            | b::bs -> b2i_aux bs ((if b then 1l else 0l) + (acc lsl 1))
    in
    b2i_aux bits 0l

let rec split n list =
    if n = 0 then
        ([], list)
    else
        match list with
            | [] -> raise (Error "can't split from empty list")
            | x::xs ->
                let (taken, rest) = split (n-1) xs in
                (x :: taken, rest)

let rec split_right n list =
    split (List.length list - n) list

let regs bits =
    let (_, regsbits) = split_right 9 bits in
    let (abits, rest) = split 3 regsbits in
    let (bbits, rest) = split 3 rest in
    let (cbits, rest) = split 3 rest in (* XXX check that rest = []? *)
    { a = bits_to_int abits; b = bits_to_int bbits; c = bits_to_int cbits }

(* decode : word -> instruction *)
let decode word =
    let bits = word_to_bits word in
    let bits = pad bits 32 in
    let (instr, rest) = split 4 bits in
    match bits_to_int instr with
        (* standard operators *)
        | 0 -> Icmov (regs rest)
        | 1 -> Iarray_index (regs rest)
        | 2 -> Iarray_update (regs rest)
        | 3 -> Iadd (regs rest)
        | 4 -> Imul (regs rest)
        | 5 -> Idiv (regs rest)
        | 6 -> Inand (regs rest)
        (* other operators *)
        | 7 -> Ihalt
        | 8 -> let rs = regs rest in Iarray_alloc (rs.b, rs.c)
        | 9 -> Iarray_free (regs rest).c
        | 10 -> Ioutput (regs rest).c
        | 11 -> Iinput (regs rest).c
        | 12 -> let rs = regs rest in Iload_program (rs.b, rs.c)
        (* special operators *)
        | 13 -> let (regbits, valbits) = split 3 rest in
                Iload_immediate (bits_to_int regbits, bits_to_word valbits)
        | n -> raise (Fail ("invalid instruction number, " ^ string_of_int n))

let step state =
    let program = try Hashtbl.find state.memory 0l
                  with Not_found -> raise (Error "array 0 not found!")
    in
    let instr_word = try program.(state.ip)
                     with Invalid_argument "index out of bounds" ->
                        raise (Fail "execution finger out of bounds")
    in
    let () = state.ip <- state.ip + 1
    in
    let lookup_reg r = state.registers.(r) in
    let update_reg r v = state.registers.(r) <- v in
    let instr = decode instr_word in
    match instr with
        | Icmov regs ->
            if lookup_reg regs.c = 0l then
                ()
            else
                update_reg regs.a (lookup_reg regs.b)
        | Iarray_index regs ->
            let array =  try Hashtbl.find state.memory (lookup_reg regs.b)
                         with Not_found ->
                            raise (Fail "invalid array in Iarray_index")
            in
            let value = try array.(word_to_int (lookup_reg regs.c))
                        with Invalid_argument "index out of bounds" ->
                            raise (Fail "index out of bounds in Iarray_index")
            in
            update_reg regs.a value
        | Iarray_update regs ->
            let array =  try Hashtbl.find state.memory (lookup_reg regs.a)
                         with Not_found ->
                            raise (Fail "invalid array in Iarray_update")
            in
            let value = lookup_reg regs.c
            in
            (try array.(word_to_int (lookup_reg regs.b)) <- value
             with Invalid_argument "index out of bounds" ->
                raise (Fail "index out of bounds in Iarray_update"))
        | Iadd regs ->
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            update_reg regs.a (Int32.add b c)
        | Imul regs ->
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            update_reg regs.a (Int32.mul b c)
        | Idiv regs ->
            (* need to fake signed 32-bit division via conversion to 64-bit *)
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            let () = if c = 0l then raise (Fail "division by zero") in
            let b' = Int64.logand 0xffffffffL (Int64.of_int32 b) in
            let c' = Int64.logand 0xffffffffL (Int64.of_int32 c) in
            let out' = Int64.div b' c' in
            let out = Int64.to_int32 out' in
            update_reg regs.a out
        | Inand regs ->
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            update_reg regs.a (Int32.lognot (Int32.logand b c))
        | Ihalt -> exit 0
        | Iarray_alloc (b, c) ->
            let cap = word_to_int (lookup_reg c) in (* XXX truncation? *)
            let array = Array.make cap 0l in
            let id = freelist_next state.freelist in
            Hashtbl.replace state.memory id array;
            update_reg b id
        | Iarray_free c ->
            let id = lookup_reg c in
            let () = if id = 0l
                     then raise (Fail "program attempted to free array 0")
            in
            Hashtbl.remove state.memory id;
            freelist_cons state.freelist id
        | Ioutput c ->
            let char_word = lookup_reg c in
            let char = try Char.chr (word_to_int char_word)
                       with Invalid_argument "Char.chr" ->
                        raise (Fail "invalid char number in Ioutput")
            in
            print_char char;
            flush stdout
        | Iinput c ->
            let char = input_char stdin in
            let char_word = int_to_word (Char.code char) in
            update_reg c char_word
        | Iload_program (b, c) ->
            let id = lookup_reg b in
            let array =  try Hashtbl.find state.memory id
                         with Not_found ->
                            raise (Fail "invalid array in Iload_program")
            in
            (* optimization: if it's 0, it's already loaded *)
            let array = if id = 0l then array else Array.copy array
            in
            Hashtbl.replace state.memory 0l array;
            state.ip <- word_to_int (lookup_reg c) (* XXX truncation? *)
        | Iload_immediate (a, value) ->
            update_reg a value

(* raises End_of_file *)
let input_word in_chan =
    let b1 = input_byte in_chan in
    let b2 = input_byte in_chan in
    let b3 = input_byte in_chan in
    let b4 = input_byte in_chan in
    let word = int_to_word b1 in
    let word = Int32.shift_left word 8 in
    let word = Int32.logor word (int_to_word b2) in
    let word = Int32.shift_left word 8 in
    let word = Int32.logor word (int_to_word b3) in
    let word = Int32.shift_left word 8 in
    let word = Int32.logor word (int_to_word b4) in
    word

let slurp_words in_chan =
    let rec slurp_aux wordsacc =
        match try Some (input_word in_chan) with End_of_file -> None with
            Some word -> slurp_aux (word :: wordsacc)
          | None -> Array.of_list (List.rev wordsacc)
    in
    slurp_aux []

let load_program_from_file state file =
    let in_chan = open_in_bin file in
    let array = slurp_words in_chan in
    let () = close_in in_chan in
    Hashtbl.replace state.memory 0l array

let run file =
    let state = initial_state () in
    let () = load_program_from_file state file in
    let rec loop () = step state; loop () in
    loop ()

let () = run "../test-binaries/internal-challenge.um"
