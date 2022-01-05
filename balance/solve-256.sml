
structure Solve =
struct

  (* for mlton *)
  structure I = Int16
  structure A = Int16Array

  (* This searches for efficient ways of
     doing certain operations.

     For example, one operation is as
     follows: "permute the 6 registers
     in the following way without
     perturbing their contents."
     (there are 6! = 720 permutations.)

     Another, harder operation would be,
     permute the 6 registers and also
     modify them by the following 
     offsets o1, o2, o3, o4, o5, o6.
     (there are 6! * 256^6 =
      202661983231672320 entries!)

     This latter thing is too big to
     compute the entire table for. But it
     may not be unreasonable to solve for
     an optimal solution for particular
     cases if we are able to find a small
     upper bound--ie a sequence that
     always works of like 10 moves (is
     this even plausible?)

     but it would be nice to know that the
     DP problem is tractable as is. Here's
     a simpler one. permute the registers
     a, b, c per the input. Modify the
     offsets of a, b, and c as per the input.
     mess up regs d, x, y as you like.
   
     There are only 3! * 256^3 such operations,
     a mere 100 million table entries (approx.
     400 megabytes?)
*)

  (* we perform the dynamic programming task
     as follows. 
     
     Call the destination
     permutation/offset a+0, b+0, c+0.

     Each other state is of the form
      a+oa, b+ob, c+oc or
      a+oa, c+oc, b+ob or ...

     for some permutation of these three
     values and for some offsets oa, ob,
     oc. We will then compute the shortest
     path from every state permutation to
     this permutation. So, if we want to
     figure out how to subtract one from
     the A register, add two to the B
     register, and not permute anything,
     we look at the path in the a+1,
     b+254, c+0 cell.
     
     To do this, we construct a large
     four-dimensional table. The first
     three axes contain the offsets from
     the target values: oa, ob, and oc.

     The final axis tells us in which
     cells these values currently live. It
     is represented as a number from 0 to
     5:

       0: a+oa, b+ob, c+oc
       1: a+oa, c+oc, b+ob
       2: b+ob, a+oa, c+oc
       3: b+ob, c+oc, a+oa
       4: c+oc, a+oa, b+ob
       5: c+oc, b+ob, a+oa

     We fill in this table with the length
     of the shortest known path (using a
     certain set of operations) from it to
     the 0 cell. For the 0 cell, this length
     is 0. For any other cell:
       
     Choose N, which is the maximum number
     of instructions per "operation" we'll
     try (this is probably 3 = 15 bits =
     32k possibilities, but maybe 4 if
     pruning is possible). Each operation
     must put the register file back into
     a legal configuration (A,B,C can't be
     swapped into the D, X, Y area.). We
     then look at every ROTREF instruction
     sequence of length N. A cell's value
     is thus

    2^(5*N)-1
       min      (|seq_i| + cell[exec(seq_i)])
      i = 0

     Where |seq| is the number of non-zero
     instructions (which are no-ops) in seq.
     
     That is, each cell is initialized to look
     at all of the cells it can reach in N
     instructions, and then to choose the one
     which itself has the shortest path.

     This description is not inductive, because
     the cells that we can reach are not
     "smaller" by any induction measure I can
     think of. One easy thing to implement would
     be to compute in a "dataflow" way. Then
     continuously run update passes over this
     table until it reaches a fixed point. This
     takes time proportional to the length of
     the longest path (I think), but paths should
     not be that long.

     (Faster would be to compute the inverse of
      this operation, but that adds complication...)

     After computing this table of lengths, we can
     then search it easily to find the actual minimal
     instruction sequence.

     *)

  (* Well, that turns out to be really slow!
     What do you know!

     Inverting the operation is not hard.
     For an instruction 0..31, the inverse
     is to simply rotate right:

       ABCDXY    -> YBCAXD
       100101

     .. and then subtract the word from the
     new 0th register.

     (Can we generate a table of transitions? that
      is, of valid instruction sequences and their
      effects? We definitely waste some time
      exploring invalid instruction sequences as
      we search.)

     In the forward direction we probably get
     much faster convergence. (??) We also get
     reasonable answers in every cell after just
     one iteration, if we do it in a sensible
     order.

     We could ensure accurate answers in a single
     pass with a Dijkstra-like work queue. But
     the Dijkstra strategy doesn't work when the
     edge lengths aren't 1, as we have here. (And
     inserting each edge as actually like 32,000
     edges probably won't scale effectively.)
     
     

     *)

  structure W8 = Word8
  exception Bad of string
  
  (* track the positions of the registers
     (as register numbers, 0..5) 
     
     this is written so it's easy to change the set
     of registers that we care about. *)
  type perm = { a : int, b : int, c : int, d : int, x : int, y : int }
    
  fun ptoi {a = 0, b = 1, c = 2, ...} = 0
    | ptoi {a = 0, c = 1, b = 2, ...} = 1
    | ptoi {b = 0, a = 1, c = 2, ...} = 2
    | ptoi {b = 0, c = 1, a = 2, ...} = 3
    | ptoi {c = 0, a = 1, b = 2, ...} = 4
    | ptoi {c = 0, b = 1, a = 2, ...} = 5
    | ptoi (_ : perm) = raise Bad "ptoi"

  fun pok ({a, b, c, ...} : perm) = (a + b + c = 3)

  (* filling in dummy values for d, x, y *)
  fun itop 0 = {a = 0, b = 1, c = 2, d = 3, x = 4, y = 5}
    | itop 1 = {a = 0, c = 1, b = 2, d = 3, x = 4, y = 5}
    | itop 2 = {b = 0, a = 1, c = 2, d = 3, x = 4, y = 5}
    | itop 3 = {b = 0, c = 1, a = 2, d = 3, x = 4, y = 5}
    | itop 4 = {c = 0, a = 1, b = 2, d = 3, x = 4, y = 5}
    | itop 5 = {c = 0, b = 1, a = 2, d = 3, x = 4, y = 5}
    | itop i = raise Bad ("itop " ^ Int.toString i)

  type state = perm * W8.word * W8.word * W8.word

  (* apply a ROTREF instruction to
     transform one state to the next. *)
  fun apply w ({a, b, c, d, x, y}, oa, ob, oc) =
    (* only 32 cases; might as well make it a
       table *)
    let
      val oa' = oa + (if a = 0 then w else 0w0)
      val ob' = ob + (if b = 0 then w else 0w0)
      val oc' = oc + (if c = 0 then w else 0w0)
    in
      case w of 
      (* !!! generated from the below -- don't edit !!! *)
    (* abcdxy *)
    (* X----- *)   0w0 => ({a=a, b=b, c=c, d=d, x=x, y=y}, oa', ob', oc')
    (* X----X *) | 0w1 => ({y=a, b=b, c=c, d=d, x=x, a=y}, oa', ob', oc')
    (* X---X- *) | 0w2 => ({x=a, b=b, c=c, d=d, a=x, y=y}, oa', ob', oc')
    (* X---XX *) | 0w3 => ({x=a, b=b, c=c, d=d, y=x, a=y}, oa', ob', oc')
    (* X--X-- *) | 0w4 => ({d=a, b=b, c=c, a=d, x=x, y=y}, oa', ob', oc')
    (* X--X-X *) | 0w5 => ({d=a, b=b, c=c, y=d, x=x, a=y}, oa', ob', oc')
    (* X--XX- *) | 0w6 => ({d=a, b=b, c=c, x=d, a=x, y=y}, oa', ob', oc')
    (* X--XXX *) | 0w7 => ({d=a, b=b, c=c, x=d, y=x, a=y}, oa', ob', oc')
    (* X-X--- *) | 0w8 => ({c=a, b=b, a=c, d=d, x=x, y=y}, oa', ob', oc')
    (* X-X--X *) | 0w9 => ({c=a, b=b, y=c, d=d, x=x, a=y}, oa', ob', oc')
    (* X-X-X- *) | 0w10 => ({c=a, b=b, x=c, d=d, a=x, y=y}, oa', ob', oc')
    (* X-X-XX *) | 0w11 => ({c=a, b=b, x=c, d=d, y=x, a=y}, oa', ob', oc')
    (* X-XX-- *) | 0w12 => ({c=a, b=b, d=c, a=d, x=x, y=y}, oa', ob', oc')
    (* X-XX-X *) | 0w13 => ({c=a, b=b, d=c, y=d, x=x, a=y}, oa', ob', oc')
    (* X-XXX- *) | 0w14 => ({c=a, b=b, d=c, x=d, a=x, y=y}, oa', ob', oc')
    (* X-XXXX *) | 0w15 => ({c=a, b=b, d=c, x=d, y=x, a=y}, oa', ob', oc')
    (* XX---- *) | 0w16 => ({b=a, a=b, c=c, d=d, x=x, y=y}, oa', ob', oc')
    (* XX---X *) | 0w17 => ({b=a, y=b, c=c, d=d, x=x, a=y}, oa', ob', oc')
    (* XX--X- *) | 0w18 => ({b=a, x=b, c=c, d=d, a=x, y=y}, oa', ob', oc')
    (* XX--XX *) | 0w19 => ({b=a, x=b, c=c, d=d, y=x, a=y}, oa', ob', oc')
    (* XX-X-- *) | 0w20 => ({b=a, d=b, c=c, a=d, x=x, y=y}, oa', ob', oc')
    (* XX-X-X *) | 0w21 => ({b=a, d=b, c=c, y=d, x=x, a=y}, oa', ob', oc')
    (* XX-XX- *) | 0w22 => ({b=a, d=b, c=c, x=d, a=x, y=y}, oa', ob', oc')
    (* XX-XXX *) | 0w23 => ({b=a, d=b, c=c, x=d, y=x, a=y}, oa', ob', oc')
    (* XXX--- *) | 0w24 => ({b=a, c=b, a=c, d=d, x=x, y=y}, oa', ob', oc')
    (* XXX--X *) | 0w25 => ({b=a, c=b, y=c, d=d, x=x, a=y}, oa', ob', oc')
    (* XXX-X- *) | 0w26 => ({b=a, c=b, x=c, d=d, a=x, y=y}, oa', ob', oc')
    (* XXX-XX *) | 0w27 => ({b=a, c=b, x=c, d=d, y=x, a=y}, oa', ob', oc')
    (* XXXX-- *) | 0w28 => ({b=a, c=b, d=c, a=d, x=x, y=y}, oa', ob', oc')
    (* XXXX-X *) | 0w29 => ({b=a, c=b, d=c, y=d, x=x, a=y}, oa', ob', oc')
    (* XXXXX- *) | 0w30 => ({b=a, c=b, d=c, x=d, a=x, y=y}, oa', ob', oc')
    (* XXXXXX *) | 0w31 => ({b=a, c=b, d=c, x=d, y=x, a=y}, oa', ob', oc')
  | w => raise Bad ("apply " ^ W8.toString w)
    end

  val NPERMS = 6
  val NINST = 2
  fun gentable () =
    let
      val sz = NPERMS * 256 * 256 * 256
      val tab = A.array(sz, 30000)

      fun idx (p, oa, ob, oc) = 
        (6 *
         (W8.toInt oa * (256 * 256) +
          W8.toInt ob * (256) +
          W8.toInt oc)) +
        ptoi p

      fun stos ({a, b, c, ...}, oa, ob, oc) =
        Int.toString a ^ Int.toString b ^ Int.toString c ^
        "." ^ W8.toString oa ^ "." ^ W8.toString ob ^ "." ^
        W8.toString oc
        
      fun get s = A.sub(tab, idx s)
      fun set s x = A.update(tab, idx s, x)

      fun makepass () =
        let
          val changed = ref false
        in
          (* different orders may have drastically
             different performance characteristics;
             I don't know how to predict... *)
          (* XXX not searching much *)
          Util.for 0 0 (* (NPERMS - 1) *)
          (fn p =>
           Util.for 0 0 (* 255 *)
           (fn oa =>
            (print "\n!"; (* progress.. *)
            Util.for 0 255
            (fn ob =>
             (print "-";
             Util.for 0 255
             (fn oc =>
              let
                val s = (itop p, 
                         W8.fromInt oa, 
                         W8.fromInt ob,
                         W8.fromInt oc)

                fun is d s' len =
                  let in

                    (* at valid state? *)
                    if pok (#1 s')
                    then
                      let 
                        val old = get s
                        val that = get s'
                        val new = len + that
                      in
                        if new < old
                        then (changed := true;
                              print ("\n" ^ stos s ^ "(" ^ 
                                     I.toString new ^ ") -> " ^
                                     stos s' ^ "(" ^
                                     I.toString that ^ ")\n");
                              set s new)
                        else ()
                      end 
                    else ();

                    (* if depth remains, add more instructions *)
                    if d > 0 then
                    (* try each non-nop instruction, proceed *)
                    Util.for 1 31
                    (fn inst =>
                     let 
                       val w = Word8.fromInt inst
                       val s'' = apply w s'
                     in
                       is (d - 1) s'' (len + 1)
                     end)
                    else ()
                  end
              in
                (* now try every sequence of NINST
                   instructions *)
                is NINST s 0
              end))))));
          !changed
        end

      val n = ref 0
      fun loop () =
        let in
          print ("Pass #" ^ Int.toString (!n) ^ "..\n");
          n := !n + 1;
          if makepass () andalso !n < 20
          then loop ()
          else ()
        end
    in
      (* self is 0 distance *)
      set (itop 0, 0w0, 0w0, 0w0) 0;

      loop ()

      (* XXX do actual path search here *)
    end

  (* generates the above *)
  fun emit_code () =
    let 
      fun p i =
        let 
          val iw = W8.fromInt i

          fun bin 0w0 = nil
            | bin w =
            (if 0w0 = W8.andb(iw, W8.<<(0w1, w - 0w1))
             then false
             else true) :: bin (w - 0w1)

          val bm = bin 0w5

          val bs = implode
            (map (fn true => #"X" | false => #"-") bm)

          val perms = 
            let
              (* assume A is included for this *)
              val bm = true :: bm
              val them = ["a", "b", "c", "d", "x", "y"]
              val marked = ListPair.zip (them, bm)
              val rotme =
                List.mapPartial (fn (a, b) => if b then SOME a
                                              else NONE) marked
              val rotted = tl rotme @ [hd rotme]

              fun replace nil nil = nil
                | replace (hr::tr) ((_, true)::t) = hr :: replace tr t
                | replace r ((a, false)::t) = a :: replace r t
                | replace _ _ = raise Bad "replace"

              val results = replace rotted marked
            in
              ListPair.map (fn (v, l) => l ^ "=" ^ v) (them, results)
            end

        in
          print ("    (* X" ^
                 bs ^ 
                 " *) " ^
                 (if i > 0 then "|" else " ") ^ 
                 " 0w"  ^ 
                 Int.toString i ^
                 " => ({" ^
                 StringUtil.delimit ", " perms ^
                 "}, " ^
                 "oa', ob', oc')\n")
        end
    in
      print ("    (* abcdxy *)\n");
      Util.for 0 31 p
    end

end

val _ = Solve.gentable () handle Solve.Bad s => (print s)
