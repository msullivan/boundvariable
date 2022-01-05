
(* stack-based virtual machine with "weird"
   instructions. The idea here is to be easy to
   implement and "reasonably" efficient, but also
   to serve as a target for us to easily generate
   impenetrably obfuscated code via compilation.

   The machine works as follows. We have two data
   structures at runtime. One is a stack of data,
   which we call the Tower. The other is a ring
   buffer of stacks. The Ring contains stacks. It
   has a concept of a current "position," which we
   call the cursor.

   The Ring has four operations:
      1. read <n>, which copies the top <n> elements
         of the stack to the right of the cursor
         and puts them on the top of the Tower.
      2. write <n>, which copies the top <n> elements
         of the Tower and adds a new element to the
         Ring, such that it is to the right of the
         cursor.
      3. rotate <n>, which rotates the cursor to the
         right (so that the element that would be
         the next stack read is now the last one)
         <n> times
      4. delete, which removes the stack to the
         right of the cursor from the Ring.

   The only "type" of datum is the unsigned 32-bit
   integer. Data can be interpreted as instructions
   as well.

   The machine begins with the Ring empty and the Tower
   initialized with the sequence of 32-bit words from
   the input program (the last word becomes the top of
   the Tower). The machine steps by removing the top
   word from the tower and interpreting it as an 
   instruction. If the word does not decode to a valid 
   instruction, it is simply discarded.

   Ideas:
    Observe that the ring can be used as data storage
    in a variety of ways. For instance, one can build
    a map from words to data by storing the word
    at the top of the stack in the Ring, the size of
    the data after that, and then the data itself.
    Accesses are done by searching the Ring.
    (This is essentially "malloc.")

    It should be the case that an initial inefficient
    implementation of this machine is really easy,
    and suffices to begin working on the contest. But
    later phases might deliberately exercise certain 
    features of the VM, slowing down implementations
    that take shortcuts. (For example: the rotate
    instruction could be given very large 32-bit
    numbers for those implementations that do not
    do it via modular arithmetic.)

   Notes:
    This VM does serve as a light reference to the 
    programming language GML used in the raytracer 
    ICFP contest.

*)

structure VM =
struct

  structure W = Word32

  datatype primop = PLUS

  datatype inst =
    
    (* let's limit I/O to these two *)
    READCHAR
  | PUTCHAR
    
  | HALT
  | OP_ADD

  | READ
  | WRITE
  | ROTATE
  | DELETE
    (* pop the argument below, call it n.
       then push the register onto the top of the
       tower. 
       then flip the next n elements (including
       the just pushed register) at the top of
       the stack. (if 0, flip the entire stack) *)
  | PUSH_AND_FLIP
    (* pop the top of the stack, call it c.
       pop the top of the stack, call it n.

       if c is nonzero, remove the next <n>
       elements of the stack. *)
  | CSKIP

 fun wtoi 0w1 = SOME READCHAR
   | wtoi 0w2 = SOME PUTCHAR
   | wtoi 0w3 = SOME HALT
   | wtoi 0w4 = SOME CSKIP
   | wtoi 0w5 = SOME READ
   | wtoi 0w6 = SOME WRITE
   | wtoi 0w7 = SOME ROTATE
   | wtoi 0w8 = SOME DELETE
   | wtoi 0w9 = SOME PUSH_AND_FLIP
   | wtoi 0w10 = SOME OP_ADD

   (* XXX ops *)
   | wtoi (_ : W.word) = NONE

 type mach = { tower : W.word list ref,
               ring : W.word list list ref,
               reg : W.word ref }

 exception Stuck of string
 exception Halt
 fun pop (ref nil) = raise Stuck "empty stack"
   | pop (t as ref (h::rest)) = (t := rest; h)

 fun chop (w : W.word) l =
   let
     (* val () = print ("Chopping: " ^ W.toString w ^ "\n") *)
     fun r 0w0 acc l = (rev acc, l)
       | r w acc (h::t) = r (w - 0w1) (h :: acc) t
       | r _ _ _ = raise Stuck "bad chop"
   in
     r w nil l
   end

 fun step { tower, ring, reg } =
   case wtoi (pop tower) of
     NONE => () (* ignore *)
   | SOME i => 
       (case i of
          READCHAR =>
            (case TextIO.input1 TextIO.stdIn of
               NONE => reg := 0wxFFFFFFFF
             | SOME c => reg := (W.fromInt (ord c)))
        | PUTCHAR => TextIO.output1 (TextIO.stdOut, 
                                     chr (W.toInt (W.andb (0w255, pop tower))))
        | HALT => raise Halt

        | PUSH_AND_FLIP => 
            (case pop tower of
               0w0 => tower := rev (!reg :: !tower)
             | w => let 
                      val () = tower := !reg :: !tower
                      val (top, bot) = chop w (!tower)
                    in tower := rev top @ bot
                    end)
        | WRITE =>
               let val (top, _) = chop (pop tower) (!tower)
               in
                 ring := top :: !ring
               end

        | READ => 
               (let val (top, _) = chop (pop tower) (hd (!ring))
                in tower := top @ !tower
                end handle Empty => raise Stuck "ring empty for read")

        | ROTATE => 
                  (let val w = pop tower
                       fun rot 0w0 = ()
                         | rot w = (ring := tl (!ring) @ [hd (!ring)];
                                    rot (w - 0w1))
                   in
                     rot w
                   end handle Empty => raise Stuck "ring empty for rotate")

        | OP_ADD => reg := pop tower + pop tower

        | CSKIP => 
                     let 
                       val c = pop tower
                       val n = pop tower
                     in
                       if c <> 0w0 
                       then tower := #2 (chop n (!tower))
                       else ()
                     end

        | _ => raise Stuck "unimplemented")

 fun intos READCHAR = "READCHAR"
   | intos PUTCHAR = "PUTCHAR"
   | intos HALT = "HALT"
   | intos OP_ADD = "OP_ADD"
   | intos READ = "READ"
   | intos WRITE = "WRITE"
   | intos ROTATE = "ROTATE"
   | intos DELETE = "DELETE"
   | intos PUSH_AND_FLIP = "PUSH_AND_FLIP"
   | intos CSKIP = "CSKIP"
(*
   | intos _ = "???"
*)

 fun printm { tower = ref tower, reg = ref reg, ring = ref ring } =
   let
     fun maybeinst w =
       case wtoi w of
         NONE => ""
       | SOME i => intos i

     fun wtos w = StringUtil.pad ~8 (W.toString w)
   in
     print ("Reg: " ^ wtos reg ^ "\n");
     print ("Tower:\n");
     app (fn w =>
          print ("  " ^ wtos w ^ "  " ^
                 maybeinst w ^ "\n")) tower
   end
        
 (* XXX testing *)  
 fun run p =
   let
     val mach = { tower = ref p, 
                  reg = ref 0w0,
                  ring = ref nil }
   in
     (while true do 
        let in
          (* printm mach; *)
          step mach
        end)
        handle Halt => TextIO.output(TextIO.stdErr, "Done.\n")
             | Stuck s => TextIO.output(TextIO.stdErr, "UM stuck: " ^ s ^ "\n")
   end

end
