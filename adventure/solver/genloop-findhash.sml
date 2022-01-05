
(* Runs the generator and a collection of solvers,
   limiting their time spent trying to solve the
   puzzle. *)

structure GenLoop =
struct

  exception GenLoop of Word32.word * string * string * Proof.item list

  structure GoodArg =
  struct
    val coalesce_missings = true
    val analyze_useless = true
    val prefer_incinerate = false
    val comb_newest = true
    val hashing = true
    val lazy_expand = true
  end

  structure BadArg =
  struct
    val coalesce_missings = false
    val analyze_useless = false
    val prefer_incinerate = false
    val comb_newest = true
    val hashing = false
    val lazy_expand = true
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
{ name = "BadNP", solve = BadNP.provepuzzle }
]

  val INVSIZE = 6
  val LENGTH = 80

  val MINMAXDEPTH = 100000
  val MAXDEPTH = 3000000
  val seed = ref (0wxDEADBEEF : Word32.word)

  val start = Time.now ()

  fun saveit (score, description, (pf, goal, pile)) =
    let
      
      fun toml (pf, goal, pile) =
        let
          fun ptoml (Proof.Noun s) = "(Noun \"" ^ String.toString s ^ "\")"
            | ptoml (Proof.Missing (p, pl)) = "(Missing (" ^ ptoml p ^ ", [" ^
            StringUtil.delimit ", " (map ptoml pl) ^ "]))"

          fun itoml (p, al) = "(" ^ ptoml p ^ ", [" ^
            StringUtil.delimit ", " (map (fn s => "\"" ^ String.toString s ^ "\"") al) ^ "])"

          fun steptoml (Proof.Combine (a, b)) = "Combine(" ^ itoml a ^ ", " ^ itoml b ^ ")"
            | steptoml (Proof.Pickup a) = "Pickup(" ^ itoml a ^ ")"
            | steptoml (Proof.Incinerate a) = "Incinerate(" ^ itoml a ^ ")"

          val goals = ptoml goal
          val piles = "[" ^ StringUtil.delimit ",\n" (map itoml pile) ^ "]"

          val pfs = "[" ^ StringUtil.delimit ",\n" (map steptoml pf) ^ "]"
        in
          "val pf =\n " ^ pfs ^ "\n\n" ^
          "val goal = " ^ goals ^ "\n\n" ^
          "val pile =\n" ^ piles ^ "\n\n" ^
          "(* end *)"
        end

      val s =
        "(*\n" ^
        description ^
        "*)\n\n" ^
        toml (pf, goal, pile)

    in
      StringUtil.writefile ("best-" ^ Int.toString score ^ ".puz") s
    end

  fun findbest (bestscore, bestproblem)  =
    let
      val (pf, (goal, pile)) = 
        Generator.generate 0 seed LENGTH INVSIZE 
          (nil, (nil, [(Solver.Noun "Goal", nil)], Solver.Noun "Goal"));
    in
      tryit (bestscore, bestproblem, pf, goal, pile)
    end

  and tryit (bestscore, bestproblem, pf, goal, pile) =
    let

      val maxdepth = Int.max(bestscore * 2, MINMAXDEPTH)

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

      val startseed = !seed

      fun verifyit () =
          (case Proof.verify INVSIZE (pile, nil, goal) pf of
              Proof.Success => "(verified)"
            | Proof.Failure s => ("(couldn't verify generated proof!: " ^ s ^ ")"))


      val _ = print ":"

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
                   | Proof.Failure s => raise GenLoop (startseed,
                                                       "PROVER " ^ name ^ " FAILED! "
                                                       ^ s ^ "\n", 
                                                       verifyit (),
                                                       pile));
                     print "+";
                     {name = name,
                      steps = SOME steps,
                      time = Time.- (Time.now (), then_) }

                end handle Proof.OUTATIME => (print "-";
                                              {name = name,
                                               steps = NONE,
                                               time = Time.- (Time.now (), then_) })
                         | Proof.Solver s => raise GenLoop (startseed,
                                                            "SOLVER " ^ name ^ " failout: "
                                                            ^ s ^ "\n",
                                                            verifyit (),
                                                            pile)
                  

              val newdone = this :: done
            in
                (*
              if Time.> (Time.- (Time.now (), start), Time.fromSeconds 240)
                 then raise Proof.OUTATIME
                 else ();
                     *)
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
               let 
                 val desc = 
                   "\nNEW BEST! (score " ^ Int.toString score ^ "):\n" ^
                   (StringUtil.table 75
                    (["name", "steps", "time"] ::
                     map
                     (fn {name, steps, time} =>
                      [name, 
                       (case steps of 
                          NONE => "-" 
                        | SOME s => Int.toString s),
                          Time.toString time])
                     results))
               in
                 print desc;
                 
                 saveit(score, desc, (pf, goal, pile));
                 tryit(score, (pf, goal, pile), pf, goal, pile)
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
        (* for bug *)
        (* seed := 0wx12505B17; *)
      findbest(~1, (nil, Solver.Noun "ILLEGAL", nil));
      ()
    end handle GenLoop (seednum, s, vfyok, pile) => 
        (print ("\n\n(" ^ Word32.toString seednum ^ ") GENLOOP: " ^ s ^ "\n");
         print "\n===== pile =====\n";
         print (StringUtil.delimit "\n" (map Proof.itos pile));
         print "\n";
         print "\n===== generated solution: =====\n";
         print (vfyok ^ "\n"))
    
end

val () = GenLoop.loop ()

