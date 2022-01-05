
structure Balance :> BALANCE
=
struct

  structure W8 = Word8
  structure W8V = Word8Vector
  structure W8A = Word8Array
  structure W31 = Word31

  (* The machine has a 256 byte array of bytes
     (main memory) and an arbitrarily long
     immutable instruction stream. 

     It also has six registers, the source
     registers A, B, C, and D and the destination
     registers X and Y.

     Finally, there is an instruction pointer IP
     and an instruction speed IS.

     *)

  type mach = { mem : Word8Array.array,
                (* length 6: abcdxy *)
                regs : Word8Array.array,
                is : int ref,
                ip : int ref,
                code : Word8Vector.vector }

  fun newmach m r c : mach =
    { mem = m,
      regs = r,
      is = ref 1,
      ip = ref 0,
      code = c }

  fun printmem mem =
      Util.for 0 255
      (fn i =>
       let
         val b = StringUtil.bytetohex (W8.toInt (W8A.sub(mem, i)))
       in
         print b;
         if (i + 1) mod 8 = 0 
         then if (i + 1) mod 16 = 0 
              then print "\n"
              else print "  "
         else print " "
       end)

  fun printreg regs =
      app (fn (a, i) =>
           print (a ^ ": " ^ 
                  StringUtil.bytetohex (W8.toInt (W8A.sub(regs, i)))
                  ^ " ")) [("A", 0), ("B", 1), ("C", 2), ("D", 3),
                           ("X", 4), ("Y", 5)];

  fun printmach ({ mem, regs, is, ip, code } : mach) =
    let
    in
      (* print memory *)
      print " === machine state === \n";
      print "mem:\n";
      printmem mem;
      print "regs: ";
      printreg regs;
      print ("\nis: " ^ Int.toString (!is) ^ " ip: " ^
             Int.toString (!ip) ^ "\n")
    end

  datatype inst =
      (* duality: jumps if not zero,
         so do something special when arg
         is zero *)
      JNZ of int (* new speed *)
    (* dest+1 <- src+1 - src+1, dest <- src + src *)
    | MATH of int * int * int
    (* dest+1 <- src+1 ^ src+1, dest <- src & src *)
    | LOGIC of int * int * int
    (* guaranteed to be 5 bits *)
    | REF of W8.word
    | BAIL

  fun instToString i =
      case i
       of JNZ n =>
          "SCIENCE " ^ Int.toString n
        | MATH (d, s1, s2) =>
          "MATH "
          ^ Int.toString d ^ " "
          ^ Int.toString s1 ^ " "
          ^ Int.toString s2
        | LOGIC (d, s1, s2) =>
          "LOGIC "
          ^ Int.toString d ^ " "
          ^ Int.toString s1 ^ " "
          ^ Int.toString s2
        | REF w =>
          "PHYSICS " ^ Word8.toString w
        | BAIL =>
          "BAIL"

  (* (w << (8-bits)) ~>> (8-bits) *)
  fun signextend bits w =
    W8.toIntX 
    (W8.~>>(W8.<<(w, Word.fromInt (8 - bits)),
            Word.fromInt (8 - bits)))

  (* d:0..1, s1,s2:0..3 *)
  fun dualizeops (d, s1, s2) = 
    ((d + 1) mod 2,
     (s1 + 1) mod 4,
     (s2 + 1) mod 4)

  exception BadInst

  local
      (* w[lo+width-1:lo] *)
      fun ex w lo width =
          W8.toInt (W8.andb(W8.- (W8.<< (0w1, width), 0w1), W8.>>(w, lo)))
      fun threeops w =
          let 
              val d = ex w 0w4 0w1
              val s1 = ex w 0w2 0w2
              val s2 = ex w 0w0 0w2
          (*
           val d  = W8.toInt (W8.andb(0w1, W8.>>(w, 0w4)))
           val s1 = W8.toInt (W8.andb(0w2, W8.>>(w, 0w2)))
           val s2 = W8.toInt (W8.andb(0w2, w))
           *)
          in
              (* primary ops *)
              (d, s1, s2)
          end
  in
  (* decode an instruction *)
  fun getinst w =
    case W8.andb(0w7, W8.>>(w, 0w5)) of
      (* JNZ *)
      0w0 => JNZ (signextend 5 w)
    (* ADD/SUB *)
    | 0w1 => MATH (threeops w)
    (* AND/XOR *)
    | 0w2 => LOGIC (threeops w)
    (* REF/ROT *)
    | 0w3 => REF (W8.andb(w, 0w31))
    | 0w7 => BAIL
    | _ => raise BadInst
  end

  exception Compile of string
  local
      val || = W8.orb
      fun << (v, n) = (W8.<< (W8.fromInt v, W31.fromInt n))
      infix || <<
      (* n[7:5] *)
      fun inst n =
          (n << 5)
      (* i[7:5] | d[4] | s1[3:2] | s2[1:0] *)
      fun threeops i d s1 s2 =
          (inst i) || (d << 4) || (s1 << 2) || (s2 << 0)

      fun assertWidth n w = (W31.~>> (W31.fromInt n, W31.fromInt w)) = 0w0
      (*fun assertImm imm = assertWidth imm 23 orelse raise Compile "imm"*)
      fun assertImm imm = assertWidth imm 5 orelse raise Compile "imm"
      fun assertImm' imm = assertWidth (Word8.toInt imm) 5 orelse raise Compile "imm'"
      fun assertDst d = assertWidth d 1 orelse raise Compile "dst"
      fun assertSrc s = assertWidth s 2 orelse raise Compile "src"
      fun assertOps (d, s1, s2) = assertDst d andalso assertSrc s1 andalso assertSrc s2
      fun compile' i =
          case i
           of JNZ imm =>
              let val _ = assertImm imm
              in (inst 0) || (imm << 0)
              end
            | MATH (d, s1, s2) =>
              let val _ = assertOps (d, s1, s2)
              in threeops 1 d s1 s2
              end
            | LOGIC (d, s1, s2) =>
              let val _ = assertOps (d, s1, s2)
              in threeops 2 d s1 s2
              end
            | REF imm =>
              let val _ = assertImm' imm
              in (inst 3) || (W8.<< (imm, 0w0))
              end
            | BAIL =>
              (inst 7)
  in
  fun compile is =
      List.map compile' is
  end

  exception Done
  exception Bail

  fun step (mach as { mem, regs, is, ip, code } : mach) =
    let
      val inst = getinst (W8V.sub(code, !ip))
      val () = print ("\n" ^ W8.toString (W8V.sub(code,!ip)) ^ "\n")
      val () = print (instToString inst ^ "\n\n")
        

      fun readreg n = W8A.sub(regs, n)
      fun readsrc n = W8A.sub(mem, W8.toInt (readreg n))
        
      fun setreg  n w = W8A.update(regs, n, w)
      fun setdest n w = W8A.update(mem, W8.toInt (readreg n), w)
    in
      (* do inst *)
      (case inst of
         JNZ d =>
         if readsrc 0 = 0w0
         then ()
         else if d = 0
         then raise Done
         else is := d (* SUSP sign-extend? *)
       | MATH (ops as (d, s1, s2)) =>
           let
             val (d', s1', s2') = dualizeops ops
           in
             (* sub first, then add *)
             setdest (4 + d') (W8.- (readsrc s1', readsrc s2'));
             setdest (4 + d)  (W8.+ (readsrc s1,  readsrc s2))
           end
       | LOGIC (ops as (d, s1, s2)) => 
           let
             val (d', s1', s2') = dualizeops ops
           in
             (* XOR first (maybe shift is more useful??), 
                then AND. *)
             setdest (4 + d') (W8.xorb (readsrc s1', readsrc s2'));
             setdest (4 + d)  (W8.andb (readsrc s1,  readsrc s2))
           end
       | REF w =>
           let
             fun rotregs w =
               let
                 (* always set 'A' bit *)
                 val w = W8.orb(0w32, w)

                 (* is the bit set at index n?
                    (n \in 0..5)
                    we treat 0w32 as index '0', 
                    0w16 as '1', etc. *)
                 fun bitset n =
                   0w0 <>
                   W8.andb(w, 
                           W8.>>(0w32,
                                 Word.fromInt n))
                   
               (* return the index of the next 
                  (with wraparound) bit that
                  is set in the word w 
                  
                  some bit must be set, but
                  this is true because we always
                  set the 'A' bit.
                  *)
                 fun nextbit n =
                   let
                     val n = (n + 1) mod 6
                   in
                     if bitset n 
                     then n
                     else nextbit n
                   end

                 (* for performance, avoid making
                    a new array *)
                 val first = readreg 0

                 (* rotate into dst from the
                    next src, then continue *)
                 fun loop dst =
                   case nextbit dst of
                     0 => setreg dst first
                   | src => (setreg dst (readreg src);
                             loop src)
               in
                 loop 0
               end
             
             (* change reference for A, then rotate. *)
           in
             setreg 0 (W8.fromInt 
                       (W8.toInt (readreg 0) +
                        signextend 5 w));
             rotregs w
           end
       | BAIL => raise Bail
         );
      (* advance IP last *)
      ip := (!ip + !is) mod W8V.length code
    end

  fun run mach =
      let fun run' n = (printmach mach; TextIO.inputLine TextIO.stdIn; 
                        step mach;
                        if n = 1024
                        then ((*print ".";*) run' 1)
                        else run' (n + 1))
      in run' 1
         handle BadInst => print "bad instruction\n"
              | Done => print "reached tightest infinite loop\n"
              | Bail => print "reached BAILance\n"
      end

end

