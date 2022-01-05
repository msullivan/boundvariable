
structure Bench =
struct
(*
  val _ = Control.Print.printDepth := 10000
  val _ = Control.Print.printLength := 10000
*)
  val INVSIZE = 6

  val (pf, (goal, pile)) = Generator.generate 0 (ref 0wxDEADBEEF) 25 INVSIZE (nil, (nil, [(Solver.Noun "EPROMBurner", nil)], Solver.Noun "EPROMBurner"));

  fun bench () =
    let
      val then_ = Time.now ()
      val (p, steps) = Solver.provepuzzle NONE (goal, pile, INVSIZE)
                        	handle Match => (print "solvematch\n"; raise Match)
    in
      case Proof.verify INVSIZE (pile, nil, goal) p
                        	handle Match => (print "verifymatch\n"; raise Match)

      of Proof.Success => print "YES\n"
      | Proof.Failure s => print ("NO : " ^ s ^ "\n");
(*
        ("------------- goal  -------------------------------------------------------------------------",
         goal, 

         "------------- pile  -------------------------------------------------------------------------",
         pile, 

         "------------- proof -------------------------------------------------------------------------",
         p)
*)
        print ("it took: " ^ Time.toString (Time.- (Time.now (), then_)) ^ "\n")

    end
	handle Match => (print "benchmatch\n"; raise Match)
end

val _ = print "self-check:\n"
val _ = Bench.bench ()
