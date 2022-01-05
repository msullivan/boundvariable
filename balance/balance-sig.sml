signature BALANCE
=
sig

  datatype inst =
      JNZ of int
    | MATH of int * int * int
    | LOGIC of int * int * int
    | REF of Word8.word
    | BAIL

  val instToString : inst -> string
  type mach
  val newmach : Word8Array.array -> Word8Array.array -> Word8Vector.vector -> mach
  val printmem : Word8Array.array -> unit
  val printreg : Word8Array.array -> unit
  val printmach : mach -> unit
  val step : mach -> unit
  val run : mach -> unit

  exception Done
  exception Bail

  val compile : inst list -> Word8.word list

end
