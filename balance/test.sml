
structure Test =
struct

  structure W8 = Word8
  structure W8V = Word8Vector
  structure W8A = Word8Array

  structure B = Balance
  structure C = Codegen

  local
      (* initial value *)
      val (oA, oB, oC, oD, oX, oY) = (0, 1, 2, 3, 4, 5)
  in
  (* invariant: M[RD] = 0, RC = RY *)
  (* M[2] = M[0] + M[1] *)
  (* {1, 2, 4, 8, 16, 32, 64, 128, 0, ...} *)
  val addM =
      let val m = W8A.tabulate (256, fn _ => 0w0)
      in Util.for 0 7 (fn i => W8A.update (m, i, W8.<< (0w1, Word31.fromInt i)))
       ; m
      end
  (* {0, 1, 2, 3, 4, 5} *)
  val addR =
      W8A.tabulate (6, fn i => W8.fromInt i)
  val add =
      [C.SET (C.rB, oB, 0)
     , C.SET (C.rC, oC, 1)
     , C.SET (C.rD, oD, ~1)
     , C.SET (C.rX, oX, 2)
     , C.SET (C.rY, oY, 1)
     (* (0,1,~1,2,1) *)
     , C.MATH (C.rX, C.rB, C.rC)
     , C.SWAPIN C.rX
     (* M[RY] <- M[RC] - M[RD] = M[RY] - 0 = M[RY]
        M[RX] <- M[RB] + M[RC] = M[0] + M[1] *)
     , C.STOP]

  (* invariant: M[RD] = 0, RC = RY, M[RC] = RX *)
  (* M[i] = M[0] for i = 1..255 *)

  (* M = {a0, i, j, 0, 1, ?, 0, ...} *)
  (* 0 < i < j *)
  (* M[i..j] = a0 *)
  (* i = M[1]
     j = M[2]
     k = 0
     while i > 0
       k++
       i--
       j--
     while j > 0
       M[k] = a
       k++
       j--
   *)
  (* L1': (* (1,2,3,4,5) *)
     B = 0
     C = 1
     D = 4
     X = 1
     (* (0,1,4,1,5) *)
     swap A X
     JNZ L1

     (* (0,1,4,1,5), M[1]=0 *)
     swap A X
     M[X] = M[C] + M[D] (* M[1]++ *)
     swap A X
     JNZ L2'

     (* k++, M[1]--, M[2]-- *)
     L1: (* (k,1,4,1,5), M[X] <> 0 *)
     (* RESET SPEED *)
     JNZ 1
     swap A X
     B = B + 1 (* k++ *)
     M[X] = M[C] - M[D] (* M[1]-- *)
     X = X + 1
     C = C + 1
     (* (k,2,4,2,5) *)
     M[X] = M[C] - M[D] (* M[2]-- *)
     X = X - 1
     C = C - 1
     (* (k,1,4,1,5) *)
     swap A X
     JNZ L1

     (* (k=i,1,4,1,5), M[1]=0 *)
     swap A X
     (* M[1]++ *)
     M[X] = M[C] + M[D]
     swap A X

     L2': (* (k=i,1,4,1,5), M[1]=1 *)
     (* RESET SPEED *)
     JNZ 1
     swap A X
     swap B X
     B = B - 1
     C = C + 1
     (* (0,2,4,k=i,5) *)
     swap A C
     JNZ L2

     L2'': (* (0,2,4,k=i,5), M[2]=0 *)
     swap A C
     Y = Y - 3
     swap B X
     X = X + 5
     (* (k,2,4,5,2) *)
     M[Y] = M[C] + M[D] (* M[2]++ *)
     X = X - 5
     swap B X
     Y = Y + 3
     (* (0,2,4,k,5), M[2]=1 *)
     swap A C
     JNZ L3

     (* M[k] = M[0], k++, M[2]-- *)
     L2: (* (0,2,4,k,5), M[X] <> 0 *)
     JNZ 1
     swap A C
     D = D - 1
     (* (0,2,3,k,2) *)
     M[X] = M[B] + M[D] (* M[k] = a *)
     D = D + 1
     X = X + 1
     swap X B
     X = X + 5
     Y = Y - 3
     (* (k,2,4,5,2) *)
     M[Y] = M[C] - M[D] (* i'-- *)
     Y = Y + 3
     X = X - 5
     swap X B
     (* (0,2,4,k,5) *)
     swap A C
     JNZ L2

     L3: (* (0,2,4,k,5), M[2]=0 *)
     swap A C
     Y = Y - 3
     swap B X
     X = X + 5
     (* (k,2,4,5,2) *)
     M[Y] = M[C] + M[D] (* M[2]++ *)
     X = X - 5
     swap B X
     Y = Y + 3
     (* (0,2,4,k,5), M[2]=1 *)
     swap A C
     JNZ 0
   *)
  (* {a, i, j, 0, 1, 2, 4, 8, 0, ...} *)
  fun loopM a i j =
      let val m = W8A.tabulate (256, fn _ => 0w0)
      in W8A.update (m, 0, W8.fromInt a)
       ; W8A.update (m, 1, W8.fromInt i)
       ; W8A.update (m, 2, W8.fromInt j)
       ; Util.for 0 3 (fn i => W8A.update (m, 4 + i, W8.<< (0w1, Word31.fromInt i)))
       ; m
      end
  (* {0, 1, 2, 3, 4, 5} *)
  val loopR =
      W8A.tabulate (6, fn i => W8.fromInt i)
(*
  val loop =
      [C.LAB "L1'"
     , C.SET (C.rB, oB, 0)
     , C.SET (C.rC, oC, 1)
     , C.SET (C.rD, oD, 4)
     , C.SET (C.rX, oX, 1)
     (* (0,1,4,1,5) *)
     , C.SWAPIN C.rX
     , C.JNZ "L1"

     , C.LAB "L1''"
     , C.SWAPOUT C.rX
     , C.MATH (C.rX, C.rC, C.rD) (* i++ *)
     , C.SWAPIN C.rX
     , C.JNZ "L2'"

     , C.LAB "L1"
     , C.RESET
     , C.SWAPOUT C.rX
     (* (k,1,4,1,5), M[X] <> 0 *)
     , C.INC (C.rB, 1) (* k++ *)
     , C.MATH (C.dX, C.dC, C.dD) (* i-- *)
     , C.INC (C.rX, 1)
     , C.INC (C.rC, 1)
     , C.MATH (C.dX, C.dC, C.dD) (* j-- *)
     , C.DEC (C.rX, 1)
     , C.DEC (C.rC, 1)
     (* (k,1,4,1,5) *)
     , C.SWAPIN C.rX
     , C.JNZ "L1"

     , C.SWAPOUT C.rX
     (* (k=i,1,4,1,5), M[1]=0 *)
     , C.MATH (C.rX, C.rC, C.rD)
     (* (k=i,1,4,1,5), M[1]=1 *)
     , C.SWAPIN C.rX
     (* , C.JNZ "L2'" *)

     , C.LAB "L2'"
     , C.RESET
     , C.SWAPOUT C.rX
     (* (k=i,1,4,1,5), M[1]=1 *)
     , C.TRANSPOSE (C.rX, C.rB)
     , C.DEC (C.rB, 1)
     , C.INC (C.rC, 1)
     , C.SWAPIN C.rC
     , C.JNZ "L2"

     , C.LAB "L2''"
     , C.SWAPOUT C.rC
     (* (0,2,4,k=i,5), M[2]=0 *)
     , C.DEC (C.rY, 3)
     , C.TRANSPOSE (C.rX, C.rB)
     , C.INC (C.rX, 5)
     (* (k,2,4,5,2), M[2]=1 *)
     , C.MATH (C.rY, C.rC, C.rD) (* j++ *)
     , C.DEC (C.rX, 5)
     , C.TRANSPOSE (C.rX, C.rB)
     , C.INC (C.rY, 3)
     (* (0,2,4,k,5), M[2]=1 *)
     , C.SWAPIN C.rC
     , C.JNZ "L3"

     , C.LAB "L2"
     , C.RESET
     , C.SWAPOUT C.rC
     (* (0,2,4,k,5), M[C] <> 0 *)
     , C.DEC (C.rD, 1)
     (* (0,2,3,k,5) *)
     , C.MATH (C.rX, C.rB, C.rD) (* M[k] = a *)
     , C.INC (C.rD, 1)
     , C.INC (C.rX, 1) (* k++ *)
     , C.TRANSPOSE (C.rX, C.rB)
     , C.INC (C.rX, 5)
     , C.DEC (C.rY, 3)
     (* (k+1,2,4,5,2) *)
     , C.MATH (C.dY, C.dC, C.dD) (* j-- *)
     , C.INC (C.rY, 3)
     , C.DEC (C.rX, 5)
     , C.TRANSPOSE (C.rX, C.rB)
     (* (0,2,4,k+1,5) *)
     , C.SWAPIN C.rC
     , C.JNZ "L2"

     (* (0,2,4,k,5), M[2]=0 *)
     , C.LAB "L3"
     , C.SWAPOUT C.rC
     , C.DEC (C.rY, 3)
     , C.TRANSPOSE (C.rX, C.rB)
     , C.INC (C.rX, 5)
     (* (k,2,4,5,2) *)
     , C.MATH (C.rY, C.rC, C.rD)
     , C.DEC (C.rX, 5)
     , C.TRANSPOSE (C.rX, C.rB)
     , C.INC (C.rY, 3)
     (* (0,2,4,k,5), M[2]=1 *)
     , C.SWAPIN C.rC
     , C.STOP]
*)

  val loop =
      [ { this = "L0"
        , main =
          [ C.SET (C.rB, oB, 0)
          , C.SET (C.rC, oC, 1)
          , C.SET (C.rD, oD, 4)
          , C.SET (C.rX, oX, 1) ]
          (* (0,1,4,1,5) *)
        , flow = C.FIRST
                     { outr = C.rB
                     , j = "L1'" }
        }
      , { this = "L1'"
        , main =
          []
        , flow = C.STRAIGHT
                     { inr = C.rB
                     , outr = C.rX
                     , jt = "L1"
                     , leave = 
                       (* i++ *)
                       [ C.MATH (C.rX, C.rC, C.rD) ]
                     , jf = "L2'" }
        }
      , { this = "L1"
        , main =
          (* (k,1,4,1,5), M[X] <> 0 *)
          [ C.INC (C.rB, 1) (* k++ *)
          , C.MATH (C.dX, C.dC, C.dD) (* i-- *)
          , C.INC (C.rX, 1)
          , C.INC (C.rC, 1)
          , C.MATH (C.dX, C.dC, C.dD) (* j-- *)
          , C.DEC (C.rX, 1)
          , C.DEC (C.rC, 1) ]
          (* (k,1,4,1,5) *)
        , flow = C.LOOP
                     { inr = C.rX
                     , outr = C.rX
                     , leave =
                       (* (k=i,1,4,1,5), M[1]=0 *)
                       [ C.MATH (C.rX, C.rC, C.rD) ]
                     (* (k=i,1,4,1,5), M[1]=1 *)
                     , jf = "L2'" }
        }
      , { this = "L2'"
        , main =
          (* (k=i,1,4,1,5), M[1]=1 *)
          [ C.TRANSPOSE (C.rX, C.rB)
          , C.DEC (C.rB, 1)
          , C.INC (C.rC, 1) ]
        , flow = C.STRAIGHT
                     { inr = C.rX
                     , outr = C.rC
                     , jt = "L2"
                     , leave = 
                       (* (0,2,4,k=i,5), M[2]=0 *)
                       [ C.DEC (C.rY, 3)
                       , C.TRANSPOSE (C.rX, C.rB)
                       , C.INC (C.rX, 5)
                       (* (k,2,4,5,2), M[2]=1 *)
                       , C.MATH (C.rY, C.rC, C.rD) (* j++ *)
                       , C.DEC (C.rX, 5)
                       , C.TRANSPOSE (C.rX, C.rB)
                       , C.INC (C.rY, 3) ]
                       (* (0,2,4,k,5), M[2]=1 *)
                     , jf = "L3" }
            }
      , { this = "L2"
        , main =
          (* (0,2,4,k,5), M[2] <> 0 *)
          [ C.DEC (C.rD, 1)
          (* (0,2,3,k,5) *)
          , C.MATH (C.rX, C.rB, C.rD) (* M[k] = a *)
          , C.INC (C.rD, 1)
          , C.INC (C.rX, 1) (* k++ *)
          , C.TRANSPOSE (C.rX, C.rB)
          , C.INC (C.rX, 5)
          , C.DEC (C.rY, 3)
          (* (k+1,2,4,5,2) *)
          , C.MATH (C.dY, C.dC, C.dD) (* j-- *)
          , C.INC (C.rY, 3)
          , C.DEC (C.rX, 5)
          , C.TRANSPOSE (C.rX, C.rB) ]
          (* (0,2,4,k+1,5) *)
        , flow = C.LOOP
                     { inr = C.rC
                     , outr = C.rC
                     , leave =
                       (* (0,2,4,k,5), M[2]=0 *)
                       [ C.DEC (C.rY, 3)
                       , C.TRANSPOSE (C.rX, C.rB)
                       , C.INC (C.rX, 5)
                       (* (k,2,4,5,2) *)
                       , C.MATH (C.rY, C.rC, C.rD)
                       , C.DEC (C.rX, 5)
                       , C.TRANSPOSE (C.rX, C.rB)
                       , C.INC (C.rY, 3) ]
                       (* (0,2,4,k,5), M[2]=1 *)
                     , jf = "L3" }
        }
      , { this = "L3"
        , main = []
        , flow = C.LAST
                     { inr = C.rC
                     , outr = C.rC }
        }
      ]
  end

(*
  local
      (* initial value *)
      val (oA, oB, oC, oD, oX, oY) = (0, 1, 2, 3, 4, 5)
      (* index *)
      val (iA, iB, iC, iD, iX, iY) = (0, 1, 2, 3, 4, 5)
      (* register number *)
      val (rA, rB, rC, rD, rX, rY) = (0, 1, 2, 3, 0, 1)
      (* dual register number *)
      val (dA, dB, dC, dD, dX, dY) = (3, 0, 1, 2, 1, 0)
  in
  (* invariant: M[RD] = 0, RC = RY *)
  (* M[2] = M[0] + M[1] *)
  val add =
      C.set iD oD ~1
      (* RB <- RD + 252 = 255 *)
      @ C.set iB oB 0
      (* RB <- RB - 1 = 0 *)
      @ C.set iC oC 1
      (* RC <- RC - 1 = 1 *)
      @ C.set iY oY 1
      (* RY <- RY - 4 = 1 *)
      @ C.set iX oX 2
      (* RX <- RX - 2 = 2 *)
      @ [B.MATH (rX, rB, rC)
        (* M[RY] <- M[RC] - M[RD] = M[RY] - 0 = M[RY]
           M[RX] <- M[RB] + M[RC] = M[0] + M[1] *)
       , B.JNZ 0]
(*
      C.inc iD (255 - oD)
      (* RB <- RD + 252 = 255 *)
      @ C.inc iB (256 - oB)
      (* RB <- RB - 1 = 0 *)
      @ C.inc iC (257 - oC)
      (* RC <- RC - 1 = 1 *)
      @ C.inc iY (257 - oY)
      (* RY <- RY - 4 = 1 *)
      @ C.inc iX (258 - oX)
      (* RX <- RX - 2 = 2 *)
      @ [B.MATH (rX, rB, rC)
        (* M[RY] <- M[RC] - M[RD] = M[RY] - 0 = M[RY]
           M[RX] <- M[RB] + M[RC] = M[0] + M[1] *)
       , B.JNZ 0]
*)

  (* invariant: M[RD] = 0, RC = RY, M[RC] = RX *)
  (* M[i] = M[0] for i = 1..255 *)

  (* M = {a0, i, j, 0, 1, ?, 0, ...} *)
  (* 0 < i < j *)
  (* M[i..j] = a0 *)
  (* M[1] = i
     M[2] = j
     k = 0
     while M[1] > 0
       k = k + 1
       M[1] = M[1] - 1
       M[2] = M[2] - 1
     while M[2] > 0
       M[k] = M[0]
       k = k + 1
       M[2] = M[2] - 1
   *)
  (* L1': (* (1,2,3,4,5) *)
     B = 0
     C = 1
     D = 4
     X = 1
     (* (0,1,4,1,5) *)
     swap A X
     JNZ L1

     (* (0,1,4,1,5), M[1]=0 *)
     swap A X
     M[X] = M[C] + M[D] (* M[1]++ *)
     swap A X
     JNZ L2'

     (* k++, M[1]--, M[2]-- *)
     L1: (* (k,1,4,1,5), M[X] <> 0 *)
     (* RESET SPEED *)
     JNZ 1
     swap A X
     B = B + 1 (* k++ *)
     M[X] = M[C] - M[D] (* M[1]-- *)
     X = X + 1
     C = C + 1
     (* (k,2,4,2,5) *)
     M[X] = M[C] - M[D] (* M[2]-- *)
     X = X - 1
     C = C - 1
     (* (k,1,4,1,5) *)
     swap A X
     JNZ L1

     (* (k=i,1,4,1,5), M[1]=0 *)
     swap A X
     (* M[1]++ *)
     M[X] = M[C] + M[D]
     swap A X

     L2': (* (k=i,1,4,1,5), M[1]=1 *)
     (* RESET SPEED *)
     JNZ 1
     swap A X
     swap B X
     B = B - 1
     C = C + 1
     (* (0,2,4,k=i,5) *)
     swap A C
     JNZ L2

     (* (0,2,4,k=i,5), M[2]=0 *)
     swap A C
     Y = Y - 3
     swap B X
     X = X + 5
     (* (k,2,4,5,2) *)
     M[Y] = M[C] + M[D] (* M[2]++ *)
     X = X - 5
     swap B X
     Y = Y + 3
     (* (0,2,4,k,5), M[2]=1 *)
     swap A C
     JNZ L3

     (* M[k] = M[0], k++, M[2]-- *)
     L2: (* (0,2,4,k,5), M[X] <> 0 *)
     JNZ 1
     swap A C
     D = D - 1
     (* (0,2,3,k,2) *)
     M[X] = M[B] + M[D] (* M[k] = a *)
     D = D + 1
     swap X B
     X = X + 5
     Y = Y - 3
     (* (k,2,4,5,2) *)
     M[Y] = M[C] - M[D] (* i'-- *)
     Y = Y + 3
     X = X - 5
     swap X B
     (* (0,2,4,k,5) *)
     swap A C
     JNZ L2

     L3: (* (0,2,4,k,5), M[2]=0 *)
     swap A C
     Y = Y - 3
     swap B X
     X = X + 5
     (* (k,2,4,5,2) *)
     M[Y] = M[C] + M[D] (* M[2]++ *)
     X = X - 5
     swap B X
     Y = Y + 3
     (* (0,2,4,k,5), M[2]=1 *)
     swap A C
     JNZ 0
   *)
  val loop =
      (* L1' *)
      C.set iB oB 0
      @ C.set iC oC 1
      @ C.set iD oD 1
      @ C.set iX oX 1
      @ C.swapin iX
      @ [B.JNZ 0] (* L1 *)

      @ C.swapout iX
      @ [B.MATH (rX, rC, rD)]
      @ C.swapin iX
      @ [B.JNZ 0] (* L2' *)

      (* L1 *)
      @ [B.JNZ 1]
      @ C.swapout iX
      @ C.inc iB 1
      @ [B.MATH (dX, dC, dD)]
      @ C.inc iX 1
      @ C.inc iC 1
      @ [B.MATH (dX, dC, dD)]
      @ C.dec iX 1
      @ C.dec iC 1
      @ C.swapin iX
      @ [B.JNZ 0] (* L1 *)

      @ C.swapout iX
      @ [B.MATH (rX, rC, rD)]
      @ C.swapin iX

      (* L2' *)
      @ [B.JNZ 1]
      @ C.swapout iX
      @ C.transpose iB iX (* optimize *)
      @ C.dec iB 1
      @ C.inc iC 1
      @ C.swapin iC
      @ [B.JNZ 0] (* L2 *)

      @ C.swapout iC
      @ C.dec iY 3
      @ C.transpose iB iX
      @ C.inc iX 5
      @ [B.MATH (rY, rC, rD)]
      @ C.dec iX 5
      @ C.transpose iB iX
      @ C.inc iY 3
      @ C.swapin iC
      @ [B.JNZ 0] (* L3 *)

      (* L2 *)
      @ [B.JNZ 1]
      @ C.swapout iC
      @ C.dec iD 1
      @ [B.MATH (rX, rB, rD)]
      @ C.inc iD 1
      @ C.transpose iX iB
      @ C.inc iX 5
      @ C.dec iY 3
      @ [B.MATH (dY, dC, dD)]
      @ C.inc iY 3
      @ C.dec iX 5
      @ C.transpose iX iB
      @ C.swapin iC
      @ [B.JNZ 0] (* L2 *)

      (* L3 *)
      @ C.swapout iC
      @ C.dec iY 3
      @ C.transpose iB iX
      @ C.inc iX 5
      @ [B.MATH (rY, rC, rD)]
      @ C.dec iX 5
      @ C.transpose iB iX
      @ C.inc iY 3
      @ C.swapin iX
      @ [B.JNZ 0] (* HALT *)
  end
*)

  fun testrot w = 
    let 
      val c = B.compile [B.REF w, B.JNZ 0]
      val m = B.newmach addM addR (Word8Vector.fromList c)
    in
      B.printmach m;
      B.run m;
      B.printmach m
    end

  fun teststream m r s =
    let val c = C.compile s
        val b = B.compile c
        val m = B.newmach m r (Word8Vector.fromList b)
    in B.printmach m
     ; B.run m
     ; B.printmach m
    end

  fun teststream' m r s =
    let val c = C.compile_streams s
        val b = B.compile c
        val m = B.newmach m r (Word8Vector.fromList b)
    in B.printmach m
     ; B.run m
     ; B.printmach m
    end

  fun test s =
      case s
       of "add" => teststream addM addR add
        | "loop" => teststream' (loopM 237 65 83) loopR loop
        | _ => raise Match

end


(*
CM.make "codegen.cm"; 
structure B = Balance 
structure C = Codegen 
structure T = Test;
fun prt l = (List.app (print o Word8.toString) l; print "\n");
val prt = print o StringUtil.bytetohex o Word8.toInt
  prt (Balance.compile (Codegen.compile Test.add));
fun prt l = (List.app (print o Word.toString) l; print "\n");
  T.test "add";
  T.test "loop";
*)

structure Show =
struct
  structure B = Balance 
  structure C = Codegen 
  structure T = Test;
  
  val test_show = Balance.compile (Codegen.compile Test.add);
    
  val _ = List.app (fn w => print (StringUtil.bytetohex (Word8.toInt w))) test_show
  val () = print "\n"

end
