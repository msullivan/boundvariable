
structure UMPrint =
struct

  open Conventions
  open UMA
  structure V = Variable

  val msg = Params.param "UML is the programming language of choice"
    (SOME ("-message",
           "message printed in 'umprint'")) "umprintmsg"

  val nonew = Params.flag false
    (SOME ("-nonew", "No newline at end")) "nonew"

  val debug = false

  fun code () =
    let
    in
      emitstring [aa, bb, cc, dd, ee, ff, gg, hh] 
      (!msg ^ (if !nonew then "" else "\n"))
      @ [HALT]
    end


end
