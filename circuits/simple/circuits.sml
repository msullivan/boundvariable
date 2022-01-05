
structure Circuits =
struct

  datatype inface  = N | W
  datatype outface = E | S

  datatype exp =
    Unit
  | Pair of exp * exp
  | Inl of exp
  | Inr of exp
  | Face of inface

  datatype cmd =
    Sink
  | Send1 of exp * outface
  | Send2 of exp * outface * exp * outface
  | Case of exp * outface * outface
  (* send to S, W *)
  | Split of exp 
  | Use of string

  (* the name of the box is only used
     to identify the wires it connects to *)
  type box = string * cmd
  (* box from, box to *)
  type wire = (string * outface) * (string * inface)

  type module =
    string *
    box list *
    wire list *
    (* inputs: north *)
    (string * inface) option *
    (* inputs: west *)
    (string * inface) option *
    (* outputs: east *)
    (string * outface) list

end