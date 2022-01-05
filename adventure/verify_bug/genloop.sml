
(* Runs the generator and a collection of solvers,
   limiting their time spent trying to solve the
   puzzle. *)

structure GenLoop =
struct

  exception GenLoop of string

  structure GoodArg =
  struct
    val coalesce_missings = true
    val analyze_useless = true
    val prefer_incinerate = false
    val comb_newest = true
    val hashing = true
  end

  structure BadArg =
  struct
    val coalesce_missings = false
    val analyze_useless = false
    val prefer_incinerate = false
    val comb_newest = true
    val hashing = false
  end

  (* best! everything on *)
  structure Good = SolverFn(open GoodArg)
                         
  (* worst! *)   
  structure Bad = SolverFn(open BadArg)
                         
  structure GoodC = SolverFn(open GoodArg
                             val coalesce_missings = not coalesce_missings)
  structure GoodA = SolverFn(open GoodArg
                             val analyze_useless = not analyze_useless)
  structure GoodP = SolverFn(open GoodArg
                             val prefer_incinerate = not prefer_incinerate)
  structure GoodN = SolverFn(open GoodArg
                             val comb_newest = not comb_newest)
  structure GoodH = SolverFn(open GoodArg
                             val hashing = not hashing)
                         
  structure BadC = SolverFn(open BadArg
                            val coalesce_missings = not coalesce_missings)
  structure BadA = SolverFn(open BadArg
                            val analyze_useless = not analyze_useless)
  structure BadP = SolverFn(open BadArg
                            val prefer_incinerate = not prefer_incinerate)
  structure BadN = SolverFn(open BadArg
                            val comb_newest = not comb_newest)
  structure BadNP = SolverFn(open BadArg
                             val comb_newest = not comb_newest
                             val prefer_incinerate = not prefer_incinerate)
  structure BadH = SolverFn(open BadArg
			    val coalesce_missings = true
                            val hashing = not hashing)

  val solvers = [
(*
{ name = "Good",  solve = Good.provepuzzle }, 
{ name = "GoodC", solve = GoodC.provepuzzle },
{ name = "GoodA", solve = GoodA.provepuzzle },
{ name = "GoodP", solve = GoodP.provepuzzle },
{ name = "GoodN", solve = GoodN.provepuzzle },
{ name = "GoodH", solve = GoodH.provepuzzle },
*)
{ name = "BadH",  solve = BadH.provepuzzle },
{ name = "Bad",  solve = Bad.provepuzzle },

(* { name = "BadC", solve = BadC.provepuzzle },  *)
(* { name = "BadA", solve = BadA.provepuzzle },  *)
{ name = "BadP",  solve = BadP.provepuzzle }, 
{ name = "BadN",  solve = BadN.provepuzzle },
{ name = "BadNP", solve = BadH.provepuzzle }
]

  val INVSIZE = 6
  val LENGTH = 80

  val MAXDEPTH = 3000000
  val seed = ref (0wxDEADBEEF : Word32.word)

  fun findbest (bestscore, bestproblem)  =
    let

      val maxdepth = Int.max(bestscore * 2, 1000)

      fun quick_exit l =
        (* if badh failed, just give up *)
         List.exists(fn {name="BadH", steps=NONE, time=_} => true | _ => false) l

      fun eval l =
        let
        in
          case (List.find (fn {name="Bad",  ...} => true | _ => false) l,
                List.find (fn {name="BadP", ...} => true | _ => false) l,
                List.find (fn {name="BadN", ...} => true | _ => false) l,
                List.find (fn {name="BadNP", ...} => true | _ => false) l,
                List.find (fn {name="BadH", ...} => true | _ => false) l) of
            (SOME {name=_, steps=bad, time=_},
             SOME {name=_, steps=badp, time=_},
             SOME {name=_, steps=badn, time=_},
             SOME {name=_, steps=badnp, time=_},
             SOME {name=_, steps=SOME x, time=_}) =>
            let
              val bad = getOpt(bad, maxdepth + 1)
              val badp = getOpt(badp, maxdepth + 1)
              val badn = getOpt(badn, maxdepth + 1)
              val badnp = getOpt(badnp, maxdepth + 1)
            in
              foldl Int.min maxdepth [bad, badp, badn, badnp] - x
            end
              | _ => 0
        end


      val (pf, (goal, pile)) = 
        Generator.generate 0 seed LENGTH INVSIZE 
          (nil, (nil, [(Solver.Noun "Goal", nil)], Solver.Noun "Goal"));

      val results =
        let
          fun trying (nil, done) = SOME (rev done)
            | trying ( {name, solve} :: t, done ) = 
            let 
              val then_ = Time.now ()

              val this =
                let
                  val (p, steps) = solve (SOME maxdepth) (goal, pile, INVSIZE)
                in
                  (case Proof.verify INVSIZE (pile, nil, goal) p
                     of Proof.Success => (* print "YES\n" *) ()
                   | Proof.Failure s => raise GenLoop ("PROVER " ^ name ^ " FAILED! "
                                                       ^ s ^ "\n"));
                     print "+";
                     {name = name,
                      steps = SOME steps,
                      time = Time.- (Time.now (), then_) }
                end handle OUTATIME => (print "-";
                                        {name = name,
                                         steps = NONE,
                                         time = Time.- (Time.now (), then_) })

              val newdone = this :: done
            in
              if quick_exit newdone
              then NONE
              else trying (t, newdone)
            end
        in
          trying (solvers, nil)
        end
            
    in
      (case results of
         NONE => (print "X"; findbest(bestscore, bestproblem))
       | SOME results => 
           let
             val score = eval results
           in
             if score > bestscore
             then
               let in
                 print ("\nNEW BEST! (score " ^ Int.toString score ^ "):\n");
                 print
                 (StringUtil.table 75
                  (["name", "steps", "time"] ::
                   map
                   (fn {name, steps, time} =>
                    [name, 
                     (case steps of 
                        NONE => "-" 
                      | SOME s => Int.toString s),
                        Time.toString time])
                   results));
                 
                 findbest(score, (pf, goal, pile))
               end
             else
               let in
                 print "!";
                 findbest(bestscore, bestproblem)
               end
           end)
    end

  fun loop () = 
    let in
      seed := 0wxDEADBEEF;
      findbest(~1, (nil, Solver.Noun "ILLEGAL", nil))
    end
    
end

val () = GenLoop.loop ()
