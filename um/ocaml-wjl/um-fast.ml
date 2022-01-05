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

(*
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
*)

let reg_a_mask = 0b111000000_l
let reg_b_mask = 0b000111000_l
let reg_c_mask = 0b000000111_l

let reg_a word =
    word_to_int (Int32.shift_right_logical (Int32.logand word reg_a_mask) 6)
let reg_b word =
    word_to_int (Int32.shift_right_logical (Int32.logand word reg_b_mask) 3)
let reg_c word =
    word_to_int (Int32.logand word reg_c_mask)

let regs word = { a = reg_a word; b = reg_b word; c = reg_c word }

(*
(* decode : word -> instruction *)
let decode word =
    let instr = Int32.to_int (Int32.shift_right_logical word 28) in
    match instr with
        (* standard operators *)
        | 0 -> Icmov (regs word)
        | 1 -> Iarray_index (regs word)
        | 2 -> Iarray_update (regs word)
        | 3 -> Iadd (regs word)
        | 4 -> Imul (regs word)
        | 5 -> Idiv (regs word)
        | 6 -> Inand (regs word)
        (* other operators *)
        | 7 -> Ihalt
        | 8 -> let rs = regs word in Iarray_alloc (rs.b, rs.c)
        | 9 -> Iarray_free (regs word).c
        | 10 -> Ioutput (regs word).c
        | 11 -> Iinput (regs word).c
        | 12 -> let rs = regs word in Iload_program (rs.b, rs.c)
        (* special operators *)
        | 13 -> let reg_mask = 0b00001110_00000000_00000000_00000000_l in
                let val_mask = 0b00000001_11111111_11111111_11111111_l in
                let regword =
                    Int32.shift_right_logical (Int32.logand word reg_mask) 25
                in
                let valword = Int32.logand word val_mask in
                Iload_immediate (word_to_int regword, valword)
        | n -> raise (Fail ("invalid instruction number, " ^ string_of_int n))
*)

let step state =
    let program = try Hashtbl.find state.memory 0l
                  with Not_found -> raise (Error "array 0 not found!")
    in
    let word =    try program.(state.ip)
                     with Invalid_argument "index out of bounds" ->
                        raise (Fail "execution finger out of bounds")
    in
    let () = state.ip <- state.ip + 1
    in
    let lookup_reg r = state.registers.(r) in
    let update_reg r v = state.registers.(r) <- v in
    (* extract instruction word and dispatch *)
    let instr = word_to_int (Int32.shift_right_logical word 28) in
    match instr with
        | 0 ->
            let regs = regs word in
            if lookup_reg regs.c = 0l then
                ()
            else
                update_reg regs.a (lookup_reg regs.b)
        | 1 ->
            let regs = regs word in
            let array =  try Hashtbl.find state.memory (lookup_reg regs.b)
                         with Not_found ->
                            raise (Fail "invalid array in Iarray_index")
            in
            let value = try array.(word_to_int (lookup_reg regs.c))
                        with Invalid_argument "index out of bounds" ->
                            raise (Fail "index out of bounds in Iarray_index")
            in
            update_reg regs.a value
        | 2 ->
            let regs = regs word in
            let array =  try Hashtbl.find state.memory (lookup_reg regs.a)
                         with Not_found ->
                            raise (Fail "invalid array in Iarray_update")
            in
            let value = lookup_reg regs.c
            in
            (try array.(word_to_int (lookup_reg regs.b)) <- value
             with Invalid_argument "index out of bounds" ->
                raise (Fail "index out of bounds in Iarray_update"))
        | 3 ->
            let regs = regs word in
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            update_reg regs.a (Int32.add b c)
        | 4 ->
            let regs = regs word in
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            update_reg regs.a (Int32.mul b c)
        | 5 ->
            let regs = regs word in
            (* need to fake signed 32-bit division via conversion to 64-bit *)
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            let () = if c = 0l then raise (Fail "division by zero") in
            let b' = Int64.logand 0xffffffffL (Int64.of_int32 b) in
            let c' = Int64.logand 0xffffffffL (Int64.of_int32 c) in
            let out' = Int64.div b' c' in
            let out = Int64.to_int32 out' in
            update_reg regs.a out
        | 6 ->
            let regs = regs word in
            let b = lookup_reg regs.b in
            let c = lookup_reg regs.c in
            update_reg regs.a (Int32.lognot (Int32.logand b c))
        | 7 -> exit 0
        | 8 ->
            let b = reg_b word in
            let c = reg_c word in
            let cap = word_to_int (lookup_reg c) in (* XXX truncation? *)
            let array = Array.make cap 0l in
            let id = freelist_next state.freelist in
            Hashtbl.replace state.memory id array;
            update_reg b id
        | 9 ->
            let c = reg_c word in
            let id = lookup_reg c in
            let () = if id = 0l
                     then raise (Fail "program attempted to free array 0")
            in
            Hashtbl.remove state.memory id;
            freelist_cons state.freelist id
        | 10 ->
            let c = reg_c word in
            let char_word = lookup_reg c in
            let char = try Char.chr (word_to_int char_word)
                       with Invalid_argument "Char.chr" ->
                        raise (Fail "invalid char number in Ioutput")
            in
            print_char char;
            flush stdout
        | 11 ->
            let c = reg_c word in
            let char = input_char stdin in
            let char_word = int_to_word (Char.code char) in
            update_reg c char_word
        | 12 ->
            let b = reg_b word in
            let c = reg_c word in
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
        | 13 ->
            let reg_mask = 0b00001110_00000000_00000000_00000000_l in
            let val_mask = 0b00000001_11111111_11111111_11111111_l in
            let regword =
                Int32.shift_right_logical (Int32.logand word reg_mask) 25
            in
            let valword = Int32.logand word val_mask in
            let a = word_to_int regword in
            update_reg a valword
        | n -> raise (Fail ("invalid instruction number, " ^ string_of_int n))

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

let () = Arg.parse [] (fun s -> run s) "XXX usage"

(* let () = run "../test-binaries/internal-challenge.um" *)
