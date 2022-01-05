
(* Interface to the Hemlock compiler. *)

signature COMPILE =
sig

  (* where shall I write the one program that I support? *)
    val compile : unit -> string -> OS.Process.status

end
