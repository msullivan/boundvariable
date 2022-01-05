
(* Universal Machine X.
   "General Machine Language"

   XXX TO-DO: update this description
   (for now see um-spec-internal.txt)

*)

structure UMX =
struct

  structure W = Word32
  structure A = Array
  structure AS = ArraySlice

  val paranoid = Params.flag false
    (SOME ("-paranoid",
           "Some expensive paranoid checking, like double-frees")) "paranoid"

  val verbose = Params.flag false
    (SOME ("-verbose", 
           "Print lots of debugging info")) "verbose"

  val instcounts = Params.flag false
    (SOME ("-instcounts",
           "Show count of instructions executed")) "instcounts"

  val alloccounts = Params.flag false
    (SOME ("-alloccounts",
           "Show count of (small) allocation sizes")) "alloccounts"

  val compliant = Params.flag false
    (SOME ("-compliant", 
           "Exit on non-standard instructions")) "compliant"

  val debug = Params.flag false
    (SOME ("-debug", 
           "Process debugging instructions")) "debug"

  val singlestep = Params.flag false
    (SOME ("-singlestep",
           "Execute by single stepping")) "singlestep"

  val watchlist = Params.paramacc nil
                     (SOME ("-w",
                            "watchlist (list of addrs)", #",")) "watchlist"

  val watchvals = Params.paramacc nil
                     (SOME ("-v",
                            "watchlist (list of values)", #",")) "watchvals"

  val gcinfo = Params.flag false
    (SOME ("-gcinfo",
           "Print GC-related info")) "gcinfo"
  (* GC info is anything that has the following bit set *)
  val gc_info_bit = W.<< (0w1, 0w12)

  fun vdo f = if !verbose then f () else ()

  structure GA = GrowArray

  exception UM of string

  fun printe s = TextIO.output(TextIO.stdErr, s)

  (* let's say eight registers ABCDEFGH *)
  type reg = int
    
  datatype inst =
    (* A <- B  unless C = 0 *)
    CMOV of reg * reg * reg
    (* replace code with array in A,
       goto value in B *)
  | LOADPROG of reg * reg
  | LITERAL of reg * W.word (* limited to 24 bits? *)
    (* just use loadprog 0, reg *)
(*  | GOTO of reg *)
  | HALT
  | READ of reg
  | WRITE of reg
  (* A <- id of new array with C words *)
  | ALLOC of reg * reg
  (* free register *)
  | FREE of reg
  (* A <- B[C] *)
  | ASUB of reg * reg * reg
  (* A[B] <- C *)
  | UPD of reg * reg * reg
  | ADD of reg * reg * reg
    (* two's complement, so SUB a, b, c
       is: (a = b - c)
       (needs one temporary =/= a)
       
       NAND a, c, c   (* a = ~c *)
       ADD a, b, a
       LITERAL t, 1
       ADD a, t, a

       *)
  | MUL of reg * reg * reg
  | DIV of reg * reg * reg
    (* easy to code up other bitwise ops
       from this *)
  | NAND of reg * reg * reg

  (* DEBUG INSTRUCTIONS - NOT PART OF UM SPECIFICATION *)
  (* stop execution and print the state of the machine *)
  | BREAK
  (* print information to stderr *)
  | INFO of W.word * reg
  (* save time stamp counter to the given register *)
  | RDTSC of reg

  val ALLOC_START = 1000

  datatype watchwhat = ARR of W.word | VAL of W.word

  val wl = ref (NONE : watchwhat list option)
  fun watched a =
    (case !wl of
       NONE => 
         (wl := SOME (map (fn s =>
                           case W.fromString s of
                             NONE => (print ("Bad watchaddr: " ^ s ^ "\n");
                                      raise UM "bad watchaddr")
                           | SOME w => ARR w) (!watchlist) @
                      map (fn s =>
                           case W.fromString s of
                             NONE => (print ("bad watchval: " ^ s ^ "\n");
                                      raise UM "bad watchval")
                           | SOME w => VAL w) (!watchvals));
          watched a)
     | SOME l =>
         List.exists (fn w => w = a) l)

  exception Watch of W.word
  fun watcharr w =
    if watched (ARR w)
    then raise Watch w
    else ()

  fun watchval w =
    if watched (VAL w)
    then raise Watch w
    else ()

  (* invt: mem[0] always has something in it *)
  type mach = { mem : (W.word A.array) GA.growarray,
                (* indices in mem that have been freed *)
                freelist : int list ref,
                (* length 8 *)
                regs : (W.word A.array),
                (* instruction pointer *)
                ip : W.word ref,
                (* time stamp counter *)
                tsc : W.word ref}

  fun triple w =
    (W.toInt (W.andb(W.>>(w, 0w6), 0w7)),
     W.toInt (W.andb(W.>>(w, 0w3), 0w7)),
     W.toInt (W.andb(w, 0w7)))

  fun double w =
    let val (a, b, c) = triple w
    in
      (b, c)
    end

  fun single w = #3 (triple w)

  (* XXX should check for illegal instructions
     in paranoid mode, 
     at some extra cost *)
  (* decode a word into an instruction *)
  fun decode w =
    (case W.>>(w, 0w28) of
       0w0 => 
       (case W.andb(W.>>(w, 0w24), 0w15) of
          0w0 => CMOV (triple w)
        | 0w8 => CMOV (triple w)

        (* debug instructions follow *)
        | 0w1 => if !compliant
                 then raise UM "non-standard instruction: BREAK"
                 else BREAK
        | 0w2 => if !compliant
                 then raise UM "non-standard instruction: INFO"
                 else INFO (W.andb (W.>>(w, 0w9), 0w32767), single w)
        | 0w3 => if !compliant
                 then raise UM "non-standard instruction: RDTSC"
                 else RDTSC (single w)
        | x => raise UM ("unknown debug instruction: " ^ (W.toString x)))

     | 0w1 => ASUB (triple w)
     | 0w2 => UPD  (triple w)
     | 0w3 => ADD  (triple w)
     | 0w4 => MUL  (triple w)
     | 0w5 => DIV  (triple w)
     | 0w6 => NAND (triple w)
     | 0w7 => HALT
     | 0w8 => ALLOC (double w)
     | 0w9 => FREE  (single w)
     | 0w10 => WRITE (single w)
     | 0w11 => READ  (single w)
     | 0w12 => LOADPROG (double w)
     | 0w13 => LITERAL (W.toInt (W.andb(0w7, W.>>(w, 0w25))),
                        W.andb (w, 0wx2000000 - 0w1))
     | _ => raise UM "bad instruction")
        
  (* give array of instructions *)
  fun newmach code =
    let
      val mem = GA.empty ()
    in
      GA.append mem code;
      (* start allocations at a higher number, 
         so that they are not confused with other
         interesting things. *)
      GA.update mem ALLOC_START (A.fromList [0wxDEADBEEF]);

      { mem = mem,
        freelist = ref nil,
        ip = ref 0w0,
        tsc = ref 0w0,
        regs = A.fromList [0w0, 0w0, 0w0, 0w0,
                           0w0, 0w0, 0w0, 0w0] } : mach
    end

  fun copy_array a =
    A.tabulate(A.length a, fn x => A.sub(a, x))

  fun rs 0 = "aa"
    | rs 1 = "bb"
    | rs 2 = "cc"
    | rs 3 = "dd"
    | rs 4 = "ee"
    | rs 5 = "ff"
    | rs 6 = "gg"
    | rs 7 = "hh"
    | rs _ = "XXX-BAD-REG-XXX"

  fun intos i =
    let
      val r = rs
    in
      case i of
        CMOV (ra, rb, rc) => "cmov " ^ r ra ^ " <- " ^ r rb ^ " (?" ^ r rc ^ ")"
      | LOADPROG (ra, rb) => "loadprog " ^ r ra ^ " @ " ^ r rb
      | LITERAL (ra, w) => "literal " ^ r ra ^ " <- " ^ Word32.fmt StringCvt.HEX w
      | HALT => "halt"
      | WRITE ra => "write " ^ r ra
      | READ ra => "read " ^ r ra ^ " <- "
      | ALLOC (ra, rb) => "alloc " ^ r ra ^ " <- (" ^ r rb ^ " words)"
      | FREE ra => "free " ^ r ra
      | ASUB (ra, rb, rc) => "asub " ^ r ra ^ " <- " ^ r rb ^ "[" ^ r rc ^ "]"
      | UPD  (ra, rb, rc) => "upd " ^ r ra ^ "[" ^ r rb ^ "] <- " ^ r rc
      | ADD (ra, rb, rc) => "add " ^ r ra ^ " <- " ^ r rb ^ " + " ^ r rc
      | MUL (ra, rb, rc) => "mul " ^ r ra ^ " <- " ^ r rb ^ " * " ^ r rc
      | DIV (ra, rb, rc) => "div " ^ r ra ^ " <- " ^ r rb ^ " / " ^ r rc
      | NAND (ra, rb, rc) => "nand " ^ r ra ^ " <- " ^ r rb ^ " ~& " ^ r rc
      | BREAK => "break"
      | INFO (w, ra) => "info " ^ (Word32.fmt StringCvt.HEX w) ^ " " ^ r ra
      | RDTSC (ra) => "rdtsc " ^ r ra
    end


  exception Halt


  (* fetch the current instruction from memory *)
  fun fetchinst mem ip =
    (
      decode (A.sub (GA.sub mem 0,
                     W.toInt (! ip))) )
    handle Overflow =>
      let in
        print ("BAD IP: " ^ W.toString (! ip) ^ "\n");
        raise Halt
      end

  (* debugging parameters *)
  (* how much of the text should we print out? 
    XXX make this configurable? *)
  val text_prefix_length = ref 140

local

nonfix div mul

val cmov_don't = ref 0
val cmov_do = ref 0
val loadprog_zero = ref 0
val loadprog_nonzero = ref 0
val literal = ref 0
val halt = ref 0
val read = ref 0
val write = ref 0
val alloc = ref 0
val free = ref 0
val asub_zero = ref 0
val asub_nonzero = ref 0
val upd_zero = ref 0
val upd_nonzero = ref 0
val add = ref 0
val nand = ref 0  
val mul = ref 0
val div = ref 0
val nonstandard = ref 0

val MAX_ALLOCCOUNT_SIZE = 0w29 : W.word
val allocs = A.array (W.toInt MAX_ALLOCCOUNT_SIZE, 0)

  fun ++ r = r := !r + 1

in

  fun account (inst, reg) =
    if !instcounts
    then
      (case inst of
         CMOV (dest, src, test) =>
           if reg test <> 0w0
           then ++ cmov_don't
           else ++ cmov_do
       | LOADPROG (a, id) =>
           if reg a = 0w0
           then ++ loadprog_zero
           else ++ loadprog_nonzero
       | LITERAL _ => ++ literal
       | HALT => ++ halt (* always will be 1, duh *)
       | READ r => ++ read
       | WRITE r => ++ write
       | ALLOC (dst, sz) => 
         let val () = (if !alloccounts then
                         let val idx = W.toInt (W.min (W.- (MAX_ALLOCCOUNT_SIZE, 0w1), reg sz))
                         in
                           A.update (allocs, idx, A.sub (allocs, idx) + 1)
                         end
                       else ())
         in
           ++ alloc
         end
       | FREE r => ++ free
       | ASUB (dst, a, off) => 
             if reg a = 0w0
             then ++ asub_zero
             else ++ asub_nonzero
       | UPD (a, off, src) => 
             if reg a = 0w0
             then ++ upd_zero
             else ++ upd_nonzero
       | ADD (dst, s1, s2) => ++ add
       | NAND (dst, s1, s2) => ++ nand
       | MUL (dst, s1, s2) => ++ mul
       | DIV (dst, s1, s2) => ++ div
       | BREAK => ++ nonstandard
       | INFO (w, r) => ++ nonstandard
       | RDTSC (r) => ++ nonstandard)
    else ()

  fun astats () =
    let in
      A.appi (fn (i, c) => print ((Int.toString i) ^ " " ^ (Int.toString c) ^ "\n")) allocs
    end

  fun istats () =
    let in
      print ("-----------------------------------------\n");
      print ("cmov_don't:   " ^ (Int.toString (!cmov_don't)) ^ "\n");
      print ("cmov_do:      " ^ (Int.toString (!cmov_do)) ^ "\n");
      print ("goto_zero:    " ^ (Int.toString (!loadprog_zero)) ^ "\n");
      print ("goto_nonzero: " ^ (Int.toString (!loadprog_nonzero)) ^ "\n");
      print ("literal:      " ^ (Int.toString (!literal)) ^ "\n");
      print ("halt:         " ^ (Int.toString (!halt)) ^ "\n");
      print ("read:         " ^ (Int.toString (!read)) ^ "\n");
      print ("write:        " ^ (Int.toString (!write)) ^ "\n");
      print ("alloc:        " ^ (Int.toString (!alloc)) ^ "\n");
      print ("free:         " ^ (Int.toString (!free)) ^ "\n");
      print ("asub_zero:    " ^ (Int.toString (!asub_zero)) ^ "\n");
      print ("asub_nonzero: " ^ (Int.toString (!asub_nonzero)) ^ "\n");
      print ("upd_zero:     " ^ (Int.toString (!upd_zero)) ^ "\n");
      print ("upd_nonzero:  " ^ (Int.toString (!upd_nonzero)) ^ "\n");
      print ("add:          " ^ (Int.toString (!add)) ^ "\n");
      print ("nand:         " ^ (Int.toString (!nand)) ^ "\n");
      print ("mul:          " ^ (Int.toString (!mul)) ^ "\n");
      print ("div:          " ^ (Int.toString (!div)) ^ "\n");
      print ("nonstandard:  " ^ (Int.toString (!nonstandard)) ^ "\n");
      print ("-----------------------------------------\n");
      if (!alloccounts) then astats () else ()
    end

end

val bins = ref (Array.array(0, 0w0) : W.word Array.array)
val syms = ref (Vector.fromList [] : string Vector.vector)

exception NotArray

  fun break (mach as { mem, freelist, ip, tsc, regs }) = 
      let fun wordToString w = StringCvt.padLeft #"0" 8 (W.toString w)
          fun intToString i = StringCvt.padLeft #"0" 8 (Int.fmt StringCvt.HEX i)
          fun print_reg (i, w) = 
              (if i = 4 then print "         " else ();
               printe ("  " ^ (rs i) ^ "=" ^ (wordToString w));
               if (i mod 4) = 3 then print "\n" else ())
          fun print_object addr obj = 
              (printe ((intToString addr) ^ "=  ");
               AS.appi (fn (i, w) => 
                          (if i = 0 then printe (intToString i ^ ":")
                           else if (i mod 4) = 0 then
                             printe ("           " ^ (intToString i ^ ":"))
                           else ();
                           printe ("  " ^ (wordToString w));
                           if (i mod 4) = 3 then print "\n" else ()))
                      obj;
               printe "\n")
               
          fun print_heap mem =
              Util.for (ALLOC_START + 1) ((GA.length mem) - 1)
              (fn i =>
                  if not (List.exists (fn x => x = i) (!freelist))
                  then
                    print_object i (AS.full (GA.sub mem i))
                  else ())

          val i = fetchinst mem ip

          (* print current label from syms *)
          val () =
            (let 
               val b = Array.sub(!bins, W.toInt (!ip))
               val s = Vector.sub(!syms, W.toInt b)
             in 
               printe (W.toString (!ip) ^ " in " ^ s ^ ":\n")
             end handle _ => printe (W.toString (!ip) ^ " in ???:\n"))

          val () = printe "  regs ->"
          val () = A.appi print_reg regs
          val () = printe ("    ip ->  " ^ (intos i) ^ "\n")
          fun read_cmd () = 
              let val () = printe ": "
              in
                case TextIO.inputLine TextIO.stdIn
                 of SOME cmd => cmd
                  | NONE => read_cmd ()
              end

            (* print an address (like stack slot) from the zero page *)
            fun printzero i =
                let
                  val v = A.sub (GA.sub mem 0, W.toInt i)
                  val vs = 
                    (case v of 
                       0w0 => "" (* null *)
                     | _ =>
                         (let 
                            val vi = W.toInt v
                            val m = GA.sub mem vi
                          in
                            A.foldli (fn (_, w, b) =>
                                      (b ^ " " ^ Word32.toString w)) " ->" m
                          end handle _ => " -> ??"))
                in
                  print (W.toString i ^ " : " ^
                         W.toString v ^ vs ^ "\n")
                end


          fun process_cmd "?\n" = 
              (printe ("Enter one of the following commands or "
                       ^ "blank to repeat the previous command.\n"); 
               printe "           h = print heap\n";
               printe "           p = print text prefix\n";
               printe "           f = print full text (not implemented)\n";
               printe "           v = print beginning of stack\n";
               printe "           c = continue execution\n";
               printe " l <hexaddr> = show a specific slot in the 0 page\n";
               printe " z <hexaddr> = print a few words after a label in the 0 page\n";
               printe "s or <enter> = execute one step\n";
               printe "           q = terminate execution\n";
               printe "           ? = print this message\n";
               process_cmd (read_cmd ()))
            | process_cmd "h\n" =
              (print_heap mem;
               process_cmd (read_cmd ()))
            | process_cmd "p\n" =
              (print_object 0 (AS.slice (GA.sub mem 0, 0, 
                                         SOME (!text_prefix_length)));
               process_cmd (read_cmd ()))
            | process_cmd "s\n" =
              singlestep := true
            | process_cmd "c\n" =
              singlestep := false
            | process_cmd "q\n" =
              raise Halt
            | process_cmd "v\n" =
                (* these constants are from a specific program, sigh *)
              (Util.for 0x89 0x9c (printzero o W.fromInt);
               process_cmd (read_cmd ()))
            | process_cmd "\n" =
              singlestep := true
            | process_cmd s =
              (case String.fields Char.isSpace s of
                 ["l", addr, ""] =>
                   (case (W.fromString addr) of
                      NONE => printe "bad addr?"
                    | SOME w => printzero w;
                    process_cmd(read_cmd()))
               | ["z", lab, ""] => 
                     (* number of this symbol.. *)
                      (case Vector.findi (fn (_, sym) => sym = lab) (!syms) of
                         SOME (id, _) =>
                           (* find first place where this appears *)
                           (case Array.findi (fn (_, b) => b = (W.fromInt id)) (!bins) of
                              SOME (i, _) =>
                                (printe (lab ^ "...\n");
                                 Util.for i (i + 9) (printzero o W.fromInt);
                                 process_cmd (read_cmd()))
                            | NONE => printe "couldn't find a bin with that label\n")
                       | NONE => printe "can't find that label\n")
               | cmd => 
                      (printe ("Unrecognized command: " ^ StringUtil.delimit ", "
                               (map (fn x => "[" ^ x ^ "]") cmd) ^ "\n");
                       printe "[lzvhpfcsq?]";
                       process_cmd (read_cmd ())))
      in
        process_cmd (read_cmd ())
      end

  and runinst (mach as { mem, freelist, ip, tsc, regs }) inst =
    let
      (* XXX check it isn't in freelist *)
      fun arr a = GA.sub mem (W.toInt a)

      fun reg x = A.sub(regs, x)
      fun setreg x v = A.update(regs, x, v)
    in
      account (inst, reg);

      (* check if any watch vals are in registers? *)
      app (watchval o reg) [0, 1, 2, 3, 4, 5, 6, 7]
      handle Watch w =>
        let in
          print ("value watch " ^ W.toString w ^ "\n");
          if !debug then break mach
          else raise Halt
        end;

      (case inst of
         CMOV (dest, src, test) =>
           let in
             vdo (fn () =>
                  printe ("cmov " ^ rs dest ^ " <- " ^ W.toString (reg src) 
                          ^ " (if " ^ W.toString (reg test) ^ " <> 0)"));
             
             if reg test <> 0w0
             then setreg dest (reg src)
             else ()
           end
       | LOADPROG (a, id) =>
           (((case reg a of
              (* special case 0, which is very common *)
              0w0 => ip := (reg id) - 0w1
            | _ => 
                let
                  val newcode = copy_array (arr (reg a))
                in

                  vdo (fn () =>
                       printe ("loadprog " ^ W.toString (reg a) ^ " : " ^
                               W.toString (reg id)));
                  GA.update mem 0 newcode;
                  (* one less, because it will increment *)
                  ip := (reg id) - 0w1;
                  watcharr (reg a)
                end)) handle Subscript =>
               let in
                 print "loadprog out of bounds.\n";
                 if !debug then break mach
                 else raise Halt
               end | Overflow =>
                let in
                  print "loadprog overflow\n";
                  if !debug then break mach
                  else raise Halt
                end | Watch w =>
                let in
                  print ("loadprog watch " ^ W.toString w ^ "\n");
                  if !debug then break mach
                  else raise Halt
                end)

       | LITERAL (r, w) => 
           let in
             vdo (fn () =>
                  printe ("literal " ^ rs r ^ " <- " ^ W.toString w));
             setreg r w
           end
       | HALT =>
           let in
             vdo (fn () => printe "HALT.");
             raise Halt
           end
       | READ r =>
           (vdo (fn () => printe "read");
            case TextIO.input1 TextIO.stdIn of
              NONE => setreg r 0wxFFFFFFFF
            | SOME c => setreg r (W.fromInt (ord c)))
       | WRITE r => 
           let in
             vdo (fn () => printe "write");
             TextIO.output1 (TextIO.stdOut, 
                             chr (W.toInt (W.andb (0w255, reg r))));
             TextIO.flushOut TextIO.stdOut
           end
       | ALLOC (dst, sz) => 
           (let
             val a = A.array(W.toInt (reg sz), 0w0)
             val next = 
               (case !freelist of
                  nil => GA.length mem
                | h :: t => 
                    let in
                      freelist := t;
                      h
                    end)
           in
             vdo (fn () => printe ("alloc " ^ rs dst ^ " <- " ^
                                   "new(" ^ W.toString (reg sz) ^
                                   ") @ " ^ Int.toString next ^ "\n"));
             GA.update mem next a;
             setreg dst (W.fromInt next);
             watcharr (W.fromInt next)
           end handle Overflow =>
           let in
             print "Alloc overflow\n";
             if !debug then break mach
             else raise Halt
           end | Watch w =>
                let in
                  print ("alloc watch " ^ W.toString w ^ "\n");
                  if !debug then break mach
                  else raise Halt
                end)


       | FREE r => 
           (* XXX should check it has been allocated *)
           (* PERF should actually remove it *)
           (let 
              val freeme = Word32.toInt (reg r)
            in
              vdo (fn () => printe "free");
              
              if !paranoid andalso List.exists (fn x => x = freeme) (!freelist)
              then 
                let in
                  print ("Freed " ^ Int.toString freeme ^
                         " which has already been freed.\n");
                  raise Halt
                    (* XXX debugger *)
                end
              else ();
              
              freelist := freeme :: !freelist;
              watcharr (reg r)
            end handle Overflow =>
           let in
             print "Free overflow\n";
             if !debug then break mach
             else raise Halt
           end | Watch w =>
                let in
                  print ("free watch " ^ W.toString w ^ "\n");
                  if !debug then break mach
                  else raise Halt
                end)

       | ASUB (dst, a, off) => 
           (let 
              val which = reg a
            in
              vdo (fn () => printe ("asub " ^ rs dst ^ " <- " ^
                                    W.toString (reg a) ^ "[" ^
                                    W.toString (reg off) ^ "]"));
              setreg dst (A.sub((arr (reg a)) handle Subscript => raise NotArray, 
                                W.toInt (reg off)));
              watcharr which
            end handle Subscript =>
              let in
                print "Array subscript out of bounds.\n";
                if !debug then break mach
                else raise Halt
              end | NotArray =>
                let in
                  print "Tried to subscript non-object.\n";
                  if !debug then break mach
                  else raise Halt
                end | Overflow =>
                let in
                  print "Array subscript overflow\n";
                  if !debug then break mach
                  else raise Halt
                end | Watch w =>
                let in
                  print ("array subscript watch " ^ W.toString w ^ "\n");
                  if !debug then break mach
                  else raise Halt
                end)

       | UPD (a, off, src) => 
           (let 
              val which = reg a
            in
             vdo (fn () => printe ("upd " ^
                                   W.toString (reg a) ^ "[" ^
                                   W.toString (reg off) ^ "] <- " ^
                                   rs src ^ ("=" ^ W.toString (reg src))));
             A.update((arr (reg a)) handle Subscript => raise NotArray, W.toInt (reg off), reg src);
             watcharr which
           end handle Subscript =>
              let in
                print "Array update out of bounds.\n";
                if !debug then break mach
                else raise Halt
              end | NotArray =>
                let in
                  print "Tried to update non-object.\n";
                  if !debug then break mach
                  else raise Halt
                end | Overflow =>
                let in
                  print "Array update overflow\n";
                  if !debug then break mach
                  else raise Halt
                end | Watch w =>
                let in
                  print ("array update watch " ^ W.toString w ^ "\n");
                  if !debug then break mach
                  else raise Halt
                end)

       | ADD (dst, s1, s2) => 
           let in
             vdo (fn () => printe ("add " ^
                                   rs dst ^ " <- " ^
                                   rs s1 ^ "(=" ^ (W.toString (reg s1)) ^ ") + " ^
                                   rs s2 ^ "(=" ^ (W.toString (reg s2)) ^ ")"));
             setreg dst (W.+ (reg s1, reg s2))
           end
       | NAND (dst, s1, s2) => 
           let in
             vdo (fn () => printe "nand");
             setreg dst (W.notb (W.andb (reg s1, reg s2)))
           end
       | MUL (dst, s1, s2) => 
           let in
             vdo (fn () => printe "mul");
             setreg dst (W.* (reg s1, reg s2))
           end
       | DIV (dst, s1, s2) => 
           let in
             vdo (fn () => printe "div");            
             (* XXX check div0 *)
             setreg dst (W.div (reg s1, reg s2))
           end
       | BREAK => ()
       | INFO (w, r) =>
           if !gcinfo andalso (W.> (W.andb (w, gc_info_bit), 0w0)) then
             printe ("GC>>> " ^ (W.toString w) ^ ": " ^ (W.toString (reg r)) ^ "\n")
           else ()
       | RDTSC (r) =>
           if !debug then setreg r (!tsc)
           else ()
             );

      (* always increment the instruction pointer *)
      ip := !ip + 0w1;

      (* increment the time stamp counter *)
      tsc := W.+(!tsc, 0w1);

      (* check to see if we should break: 
        i.e. if debug is true AND
                (we are at a BREAK instruction OR
                 singlestep is set to true) *)
      (* XXX perhaps a cleaner way to do this branch *)
      (case (!debug, inst, !singlestep)
        of (false, _, _) => ()
         | (true, BREAK, _) => break mach
         | (true, _, false) => ()
         | (true, _, true) => break mach)
    end

  fun runprog a =
    let 
      val mach = newmach a
    in
      while true do
        let
          val i = (decode (A.sub (GA.sub (#mem mach) 0,
                                  W.toInt (! (#ip mach)))))
            handle Subscript =>
              let in
                printe ("Instruction pointer (" ^ W.toString (! (#ip mach)) ^ 
                        ") outside code\n");
                raise Halt
              end
                 | Overflow => 
              let in
                printe ("Instruction pointer ( " ^ W.toString (! (#ip mach)) ^
                        ") outside code\n");
                raise Halt
              end

                   
        in
          vdo (fn () =>
               printe ("\nIP: " ^ (StringUtil.pad ~5 (W.toString (! (#ip mach)))) ^ " "));
          vdo (fn () =>
               Array.appi (fn (_, r) =>
                           (* XXX only showing 16 bits *)
                           printe (StringUtil.pad ~4 (W.toString r) ^ " ")) (#regs mach));
          vdo (fn () =>
               Util.for 23 28
               (fn i =>
                printe (W.toString (W.fromInt i) ^ ":" ^
                        StringUtil.pad ~4 (W.toString 
                                           (A.sub (GA.sub (#mem mach) 0, i))) ^ " ")));
          
          runinst mach i
        end
    end handle Halt => 
      let in
        printe "\n\n== Halted. ==\n";
        if !instcounts
        then istats ()
        else ()
      end


  fun runfromfile file =
    let
      val (base, _) = FSUtil.splitext file

      val f = BinIO.openIn file

      val () = if !singlestep then debug := true else ()

      val prog = GA.empty ()

      fun e w = Word32.fromInt (Word8.toInt w)
      fun go prog f =
        case (BinIO.input1 f,
              BinIO.input1 f,
              BinIO.input1 f,
              BinIO.input1 f) of
          (SOME a, SOME b, SOME c, SOME d) => 
          (GA.append prog (W.orb(W.<<(e a, 0w24),
                                 W.orb(W.<<(e b, 0w16),
                                       W.orb(W.<<(e c, 0w8),
                                             e d))));
           go prog f)
        | (NONE, NONE, NONE, NONE) => ()
        | _ => raise UM "file is not multiple of 4 bytes"

      val () = go prog f
      val () = BinIO.closeIn f

      (* now read bin file, if it exists *)
      val () =
        bins := 
        (let
            val fb = BinIO.openIn (base ^ ".bins")
            val bins = GA.empty ()
          in
            go bins fb;
            BinIO.closeIn fb;
            GA.finalize bins
          end handle _ => (printe ("couldn't load " ^
                                   base ^ ".bins...\n");
                           Array.array(0, 0w0)))

      val () =
        syms := 
        Vector.fromList
        (let
           val fs = StringUtil.readfile (base ^ ".sym")
         in
           String.fields Char.isSpace fs
         end handle _ => (printe ("couldn't load " ^
                                  base ^ ".sym...\n");
                          nil))
    in
      runprog (GA.finalize prog)
    end handle UM s => printe ("\n\n== Error: " ^ s ^ " ==\n")

end

val _ = 
  case Params.docommandline () of
    [f] => UMX.runfromfile f
  | _ => 
      let in
        UMX.printe "universal machine x\nusage:\num file\n\n";
        print (Params.usage ())
      end
  
