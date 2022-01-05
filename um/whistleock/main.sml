structure QuietDownNJ = struct end

(*
val outf = Params.param ""
    (SOME ("-o",
           "Name of bytecode output (relative to input file dir)")) "outf"
*)

val _ =
    case Params.docommandline () of
        [outf] => OS.Process.exit(Compile.compile () outf)
      | _ =>
            let in
                print "Usage: whistleock file.uml\n\n";
                print (Params.usage ())
            end
