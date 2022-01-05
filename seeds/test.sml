structure Test =
struct

  open Seeds

  fun iotostring IN = "i"
    | iotostring OUT = "o"

  fun booltostring true = "1"
    | booltostring false = "0"

  fun insttostring (PSH (io, w)) = "psh" ^ (iotostring io) 
                                   ^ "_" ^ (Int.toString w)
    | insttostring (POP io) = "pop" ^ (iotostring io) ^ "  "
    | insttostring (CPY r) = "cpy   "
    | insttostring (DIS r) = "dis   "
    | insttostring (NEG r) = "neg   "
    | insttostring (VOT r) = "vot   "
    | insttostring (RES r) = "res   "
    | insttostring HLT = "hlt   "

  fun regtostring R1 = "1"
    | regtostring R2 = "2"

  fun istacktostring stack = 
      let fun loop 0 st stack = st
            | loop 3 st (b1::b2::b3::b4::stack) = "..." ^ st
            | loop n st nil = loop (n - 1) (st ^ " ") nil
            | loop n st (b::stack) = loop (n - 1) ((booltostring b) ^ st) stack
      in 
        loop 25 "" stack
      end

  fun ostacktostring stack = 
      let fun loop 0 st stack = st
            | loop 3 st (b1::b2::b3::b4::stack) = st ^ "..."
            | loop n st nil = loop (n - 1) (" " ^ st) nil
            | loop n st (b::stack) = loop (n - 1) (st ^ (booltostring b)) stack
      in 
        loop 25 "" stack
      end

  fun printheaders () =
      (print "instr   | istack (old-to-new)       | T R1 R2 | ostack (new-to-old)\n")

  fun printmach ({ istack, ostack, rregs, treg, ireg, code } : mach, i) =
      let val () = case i of 
                     Inst i' => print ((insttostring i') ^ " ")
                   | Rand r => print ("*RND_" ^ (regtostring r) ^ "*")
          val () = print " | "
          val () = print (istacktostring (!istack))
          val () = print " | "
          val () = print ((booltostring (!treg)) ^ "  " ^ 
                          (booltostring (A.sub(rregs, 0))) ^ "  " ^
                          (booltostring (A.sub(rregs, 1))))
          val () = print " | "
          val () = print (ostacktostring (!ostack))
      in
        print "\n"
      end
        
   val testp = [POP IN, CPY R1, RES R1, PSH (OUT, 0),
                POP IN, CPY R1, RES R1, PSH (OUT, 0),
                POP IN, CPY R1, RES R1, PSH (OUT, 0),
                POP IN, CPY R1, RES R1, PSH (OUT, 0)]

(* Try:  Test.runtest Test.testp [true, true, false, true]; *)

   (* XXX Could we compute two XORs in parallel using R1 and R2? *)
   val xor = [POP IN, PSH (OUT, 0), POP IN, PSH (IN, 0), NEG R1,
              RES R1, PSH (IN, 0), POP OUT, PSH (OUT, 0), NEG R1, POP IN, 
              DIS R1, RES R1, (* neg A or neg B *)
              NEG R1, POP IN, PSH (OUT, 0), RES R1, (* neg (neg A or neg B) *)
              PSH (IN, 0), POP OUT, CPY R1, POP OUT, DIS R1,
              RES R1, (* A or B *)
              NEG R1, POP IN, DIS R1, RES R1, NEG R1, RES R1, PSH (OUT, 0)]
             
   val dup2 = [POP IN, PSH (OUT, 0), 
               POP IN, PSH (IN, 0), PSH (IN, 0), PSH (IN, 0),
               POP OUT, PSH (IN, 2), PSH (IN, 1), PSH (IN, 0)];

   val vote = [POP OUT, CPY R1, POP OUT, CPY R2, POP OUT, 
               VOT R1, RES R1, PSH (OUT, 0)]
    
   fun runtest p istack = 
       (printheaders (); 
        print ("        | " ^ (istacktostring istack) ^ " |         |\n");
        run p istack (SOME printmach))

(* Try:  Test.runtest Test.xor [true, true]; *)
(* Or:   Test.runtest (Test.dup2 @ Test.xor @ Test.xor @ Test.xor @ Test.vote) [false, true]; *)
end
