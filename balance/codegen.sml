structure Codegen :> CODEGEN
=
struct

  structure W8 = Word8
  structure B = Balance
  structure V = Vector
  structure M = SplayMapFn (struct
                              type ord_key = string
                              val compare = String.compare
                            end)

  (* ISA:
     ABCDXY
     012345

     Codegen:
     ABCDXY
     543210 *)

  exception Codegen of string

  type reg = int
  type lab = string
  datatype inst
    = RESET
    | STOP
    | JNZ of lab
    | MATH of reg * reg * reg
    | LOGIC of reg * reg * reg
    | SWAPIN of reg
    | SWAPOUT of reg
    | TRANSPOSE of reg * reg
    | SET of reg * int * int
    | INC of reg * int
    | DEC of reg * int
    | BAIL
    | NOP
  datatype inst'
    = RESET'
    | STOP'
    | JNZ' of lab
    | MATH' of int * int * int
    | LOGIC' of int * int * int
    | REF' of W8.word
    | BAIL'
    | NOP'
  type block = inst list
  type block' = inst' list
  type blockv' = inst' vector
  datatype flow
    = FIRST of { outr : reg, j : lab }
    | LOOP of { inr : reg, outr : reg, leave : block, jf : lab }
    | STRAIGHT of { inr : reg, outr : reg, jt : lab, leave : block, jf : lab }
    | LAST of { inr : reg, outr : reg }
  type stream
    = { this : lab
      , main : block
      , flow : flow }
  type stream'
    = { this : lab
      , code : block }
  type stream''
    = { this : lab
      , code : block' }
  type streamv''
    = { this : lab
      , code : blockv' }

  fun instToString i =
      case i
       of RESET =>
          "RESET"
        | STOP =>
          "STOP"
        | JNZ l =>
          "JNZ " ^ l
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
        | SWAPIN r =>
          "SWAPIN "
          ^ Int.toString r
        | SWAPOUT r =>
          "SWAPOUT "
          ^ Int.toString r
        | TRANSPOSE (r1, r2) =>
          "TRANSPOSE "
          ^ Int.toString r1 ^ " "
          ^ Int.toString r2
        | SET (r, old, new) =>
          "SET "
          ^ Int.toString r ^ " "
          ^ Int.toString old ^ " "
          ^ Int.toString new
        | INC (r, n) =>
          "INC "
          ^ Int.toString r ^ " "
          ^ Int.toString n
        | DEC (r, n) =>
          "DEC "
          ^ Int.toString r ^ " "
          ^ Int.toString n
        | BAIL =>
          "BAIL"
        | NOP =>
          "NOP"
  fun instToString' i =
      case i
       of RESET' =>
          "RESET'"
        | STOP' =>
          "STOP'"
        | JNZ' l =>
          "JNZ' " ^ l
        | MATH' (d, s1, s2) =>
          "MATH' "
          ^ Int.toString d ^ " "
          ^ Int.toString s1 ^ " "
          ^ Int.toString s2
        | LOGIC' (d, s1, s2) =>
          "LOGIC' "
          ^ Int.toString d ^ " "
          ^ Int.toString s1 ^ " "
          ^ Int.toString s2
        | REF' w =>
          "REF' " ^ Word8.toString w
        | BAIL' =>
          "BAIL'"
        | NOP' =>
          "NOP'"

  (* swap contents of registers 5 and r : 0..4 *)
  (* A ... R ... -> R ... A+2^r ... *)
  (* int -> inst' *)
  fun swap r =
      REF' (W8.<< (0w1, Word31.fromInt r))

  (* swap contents of registers 5 and r : 0..4, n times *)
  (* A ... R ... -> R ... A+2^r ... *)
  (* int -> int -> inst' list *)
  fun nswap r n =
      List.tabulate (n, fn _ => swap r)

  (* swap contents of registers r1, r2 : 0..4; subtracts 2^r2 from rA *)
  (* int -> int -> inst' list *)
  fun transpose r1 r2 =
      if r1 = r2
      then []
      else
          let (* 2^n *)
              fun exp n = Word31.toInt (Word31.<< (0w1, Word31.fromInt n))
              (* 2 * (2^8 - 2^r) / 2^r = 2^(9 - r) - 2 *)
              fun diff r = exp (9 - r) - 2
          in (* A ... R1 ... R2 ... *)
              swap r1
              (* R1 ... A+2^r1 ... R2 *)
              :: swap r2
              (* R2 ... A+2^r1 ... R1+2^r2 *)
              :: swap r1
              (* A+2^r1 ... R2+2^r1 ... R1+2^r2 *)
              :: nswap r1 (diff r1)
              (* A ... R2 ... R1+2^r2 *)
              @ nswap r2 (diff r2)
          (* A-2^r2 ... R2 ... R1 *)
          end

  local
      (* increments register r : 0..4 by 2^b; adds (n-1)*2^b to rA *)
      (* int -> int -> inst' list *)
      fun incbit r b n =
          (* A ... R ... B ... *)
          transpose r b
          (* A-2^b ... B ... R ... *)
          @ nswap b (2 * n)
          (* A-2^b+n*2^b ... B ... R+n*2^b ... *)
          @ transpose r b
      (* A-2^b+n*2^b ... R+2^b ... B ... *)
      (* w[n] *)
      fun bit w n =
          Word8.toInt (Word8.andb (0w1, Word8.~>> (Word8.fromInt w, Word31.fromInt n)))
      (* -w[7:n] *)
      fun diff w n =
          Word8.toInt (Word8.~ (Word8.~>> (Word8.fromInt w, Word31.fromInt n)))
  in
  (* increment register r : 0..4 by n : 0..255 *)
  (* int -> int -> inst' list *)
  fun inc r n =
      (* A ... R ... *)
      List.concat (List.tabulate (4, fn b => case bit n b
                                              of 0 => []
                                               | _ => incbit r b 1))
      (* A ... R+n[3:0] ... *)
      @ incbit r 4 (diff n 4)
      (* A ... R+n ... *)
  end

  (* register number *)
  val (rA, rB, rC, rD, rX, rY) = (5, 4, 3, 2, 1, 0)
  (* dual register number *)
  val (dA, dB, dC, dD, dX, dY) = (2, 5, 4, 3, 0, 1)

  local
  (* A..D = 5..2 |-> 0..3 *)
  fun src r = 5 - r
  (* X..Y = 1..0 |-> 0..1 *)
  fun dst r = 1 - r

  (* inst -> inst' list *)
  fun compile' inst =
      case inst
       of RESET => [RESET']
        | STOP => [STOP']
        | JNZ l => [JNZ' l]
        | MATH (d, s1, s2) => [MATH' (dst d, src s1, src s2)]
        | LOGIC (d, s1, s2) => [LOGIC' (dst d, src s1, src s2)]
        | SWAPIN r => [swap r]
        | SWAPOUT r =>
          let fun diff r = Word.toInt (Word.~>> (0w256, Word31.fromInt r))
          in nswap r (2 * diff r - 1)
          end
        | TRANSPOSE (r1, r2) => transpose r1 r2
        | SET (r, old, new) => inc r (256 + new - old)
        | INC (r, n) => inc r n
        | DEC (r, n) => inc r (256 - n)
        | BAIL => [BAIL']
        | NOP => [NOP']

  fun lookup m l = Option.valOf (M.find (m, l))
  (* int -> int M.map -> inst' -> Balance.inst *)
  fun compile'' c m inst' =
    case inst'
     of RESET' => B.JNZ (M.numItems m)
      | STOP' => B.JNZ 0
      | JNZ' l =>
        (*B.JNZ ((M.numItems m + lookup m l - c) mod M.numItems m)*)
        (* use negative jumps *)
        B.JNZ (Word.toInt (Word.andb (Word.fromInt (lookup m l - c), 0wx1f)))
      | MATH' ops => B.MATH ops
      | LOGIC' ops => B.LOGIC ops
      | REF' w => B.REF w
      | BAIL' => B.BAIL
      | NOP' => B.REF 0w0

  fun shadow l = "_" ^ l
  (* stream -> stream' *)
  fun compile_stream { this, main, flow } =
      let val (flowin, flowout) =
              case flow
               of FIRST { outr, j } =>
                  ([], [SWAPIN outr, JNZ (shadow j)])
                | LOOP { inr, outr, leave, jf } =>
                  ([RESET, SWAPOUT inr], [SWAPIN inr, JNZ (shadow this), SWAPOUT inr] @ leave @ [SWAPIN outr, JNZ (shadow jf)])
                | STRAIGHT { inr, outr, jt, leave, jf } =>
                  ([RESET, SWAPOUT inr], [SWAPIN outr, JNZ (shadow jt), SWAPOUT outr] @ leave @ [SWAPIN outr, JNZ (shadow jf)])
                | LAST { inr, outr } =>
                  ([RESET, SWAPOUT inr], [SWAPIN outr, STOP])
      in { this = this, code = flowin @ main @ flowout }
      end
  (* stream' -> stream'' *)
  fun compile_stream' code =
      List.concat (List.map compile' code)
  (* stream -> stream *)
  fun shadow_stream ({ this, ...} : stream') =
      { this = shadow this
      , code = [JNZ this] }
  (* int -> stream'' -> stream'' *)
  fun prepad_stream sz code =
      List.tabulate (sz, fn _ => NOP') @ code
  (* int -> stream'' -> stream'' *)
  fun fill_stream sz code =
      code @ List.tabulate (sz - List.length code, fn _ => RESET')
  (* stream list -> Balance.inst list *)
  fun compile_streams s =
      let (* first and other streams *)
          val (fst, s) =
              case List.partition (fn {flow = FIRST _, ...} : stream => true
                                    | _ => false) s
               of ([fst], s) => (fst, s)
                | _ => raise Codegen "no unique FIRST"
          val fst = compile_stream fst
          val s = List.map compile_stream s
          (* shadow streams *)
          val s' = List.map shadow_stream s
          (* other streams *)
          val s = s' @ s
          (* strip labels *)
          val (labs, fst :: s) =
              ListPair.unzip (List.map (fn {this, code} => (this, code))
                                       (fst :: s))
          (* build map: lab -> int *)
          val (m, _) =
              List.foldl (fn (this, (m, n)) =>
                             (M.insert (m, this, n), n + 1))
                         (M.empty, 0)
                         labs
          (* compile *)
          val fst :: s = List.map compile_stream' (fst :: s)
          (* sizes *)
          val szf = List.length fst - 1
          val sz = List.foldl (fn (s, n) => Int.max (n, List.length s)) 0 s
          (* XXX one extra line of padding with RESET, only needed for positive jumps *)
          val szt = szf + sz + 1
          (* every stream has size szf+sz
           * first stream: fst @ (sz-1 RESETs)
           * other streams: (szf NOPs) @ code @ (sz - |code| RESETs)
           *)
          (* prepend szf NOPs *)
          val s = List.map (prepad_stream szf) s
          (* append RESETs to szt *)
          val s = List.map (fill_stream szt) (fst :: s)
          (* inst' vector vector *)
          val s = V.fromList (List.map V.fromList s)
(*
val maxc = V.length s
val () = Util.for 0 (szt - 1)
         (fn r => (Util.for 0 (maxc - 1)
                            (fn c => print (instToString' (V.sub (V.sub (s, c), r)) ^ "\t"))
                 ; print "\n"))
*)
          (* inst' vector *)
          val s = V.tabulate (V.length s * szt
                                 , fn n => let val (r, c) = (n div V.length s, n mod V.length s)
                                               val i' = V.sub (V.sub (s, c), r)
                                           in compile'' c m i'
                                           end)
(*
val () = Util.for 0 (szt - 1)
         (fn r => (Util.for 0 (maxc - 1)
                            (fn c => print (B.instToString (V.sub (s, maxc * r + c)) ^ "\t"))
                 ; print "\n"))
*)
          (* Balance.inst list *)
          val s = List.tabulate (V.length s, fn i => V.sub (s, i))
      in s
      end
  in
  (* inst list -> Balance.inst list *)
  (* compile straight-line code only *)
  fun compile insts =
      let val insts = List.concat (List.map compile' insts)
          val insts = List.map (compile'' 0 M.empty) insts
      in insts
      end
  val compile_streams = compile_streams
  end

end
