signature CODEGEN
=
sig

  exception Codegen of string

  type reg
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
  type block = inst list
  datatype flow
    = FIRST of { outr : reg, j : lab }
    | LOOP of { inr : reg, outr : reg, leave : block, jf : lab }
    | STRAIGHT of { inr : reg, outr : reg, jt : lab, leave : block, jf : lab }
    | LAST of { inr : reg, outr : reg }
  type stream
    = { this : lab
      , main : block
      , flow : flow }

  val rB : reg
  val rC : reg
  val rD : reg
  val rX : reg
  val rY : reg

  val dB : reg
  val dC : reg
  val dD : reg
  val dX : reg
  val dY : reg

  val compile : inst list -> Balance.inst list
  val compile_streams : stream list -> Balance.inst list

end
