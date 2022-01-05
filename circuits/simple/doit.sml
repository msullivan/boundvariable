structure DoIt =
struct
  local 
    open MinML
  in
(*
    val prog = [("m", ["x", "y"], 
                 Pair(Var "y", Pair(Inl(Var"x"), Var "y")))]
*)

(*
    val prog = [("m", ["x", "y"],
                 App(Var "food", [Pair(Var "x", Var "x")]))]
*)
(*
    val prog = [("m", ["x"],
                 App(Var "food", [Var "x", Unit, Unit, Unit]))]
*)

(*
    val prog = [("m", ["x"],
                 Split(Var "x", "a", "b", Pair(Var"b", Var"a")))]
    *)

(*
    val prog = [("m", ["x"],
                 Split(Var "x", "a", "b",
                       Case(Var "a", "v", Inr(Var "v"), Inl(Var "v"))))]
*)

    val prog = [("m", ["x"],
                 Case(Var "x", "v", Unit, Inr Unit))]
  end


  val mods = (Compile.compile prog)
    handle (e as Compile.Compile s) => (print ("COMPILE: " ^ s ^ "\n");
                                        raise e)



  val mods = Compile.compile RT.minml_tracer_funs
    handle (e as Compile.Compile s) => (print ("COMPILE: " ^ s ^ "\n");
                                        raise e)


  (* val layme = [Example.plus, Example.mult, Example.main] *)

  val layme = mods
               

  val _ = 
    (SimpleLayout.layprog layme)
    handle SimpleLayout.SimpleLayout s => print ("ERROR: " ^ s ^ "\n")

end
