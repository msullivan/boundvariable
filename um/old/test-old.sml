
structure Test =
struct

  structure W = Word32
  
  (* look up an instruction (by search; rather than maintain the
     translation and its inverse)  *)

  exception Test of string
  fun lookup i =
    let
      fun l 0w256 = raise Test "no encoding for instruction"
        | l w = if VM.wtoi w = SOME i then w
                else l (w + 0w1)
    in
      l (0w0 : W.word)
    end

(*   val flip = lookup VM.FLIP *)
  val readchar = lookup VM.READCHAR
  val putchar = lookup VM.PUTCHAR
  val halt = lookup VM.HALT
  val read = lookup VM.READ
  val write = lookup VM.WRITE
  val rotate = lookup VM.ROTATE
  val delete = lookup VM.DELETE
  val push_and_flip = lookup VM.PUSH_AND_FLIP
  val op_add = lookup VM.OP_ADD
  val cskip = lookup VM.CSKIP

  (* 'cat' program (identity) *)
  val cat = 
    let
      val r = ref 0w0
      fun l () =
        [
         (* begin by copying the whole
            program into the ring. *)
         write,
         !r, (* length of tail *)
         readchar, (* now reg has char *)

         push_and_flip, 0w3, (* flip add and arg *)
         0w1, op_add, (* making 0 if EOF *)

         push_and_flip, 0w2, (* test for zero *)
         cskip,
         0w1, (* number of instructions to skip *)

         (* if not skipped, then done. *)
         halt,

         (* otherwise decrement again *)
         push_and_flip, 0w3,
         0wxFFFFFFFF, op_add,

         (* and print it *)
         push_and_flip, 0w2,
         putchar,

         (* loop *)
         read,
         !r]

    in
      (* loop everything except init routine *)
      r := W.fromInt (length (l ()) - 2);
      l ()
    end

end
