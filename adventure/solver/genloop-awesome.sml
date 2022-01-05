
(* Runs the generator and a collection of solvers,
   limiting their time (in steps) spent trying to solve the
   puzzle. *)

structure GenLoop =
struct

  exception GenLoop of (Word32.word * Word32.word) * string * string * Proof.item list

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
    val coalesce_missings = true
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
                         
  structure GoodA = SolverFn(open GoodArg
                             val analyze_useless = not analyze_useless)
  structure GoodP = SolverFn(open GoodArg
                             val prefer_incinerate = not prefer_incinerate)
  structure GoodN = SolverFn(open GoodArg
                             val comb_newest = not comb_newest)
  structure GoodH = SolverFn(open GoodArg
                             val hashing = not hashing)

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
{ name = "Good",  solve = Good.provepuzzle }, 
(* 
{ name = "GoodA", solve = GoodA.provepuzzle },
{ name = "GoodP", solve = GoodP.provepuzzle },
{ name = "GoodN", solve = GoodN.provepuzzle },
{ name = "GoodH", solve = GoodH.provepuzzle },
*)
{ name = "BadH",  solve = BadH.provepuzzle },
{ name = "BadA", solve = BadA.provepuzzle }, 
{ name = "Bad",  solve = Bad.provepuzzle },
{ name = "BadP",  solve = BadP.provepuzzle }, 
{ name = "BadN",  solve = BadN.provepuzzle },
{ name = "BadNP", solve = BadNP.provepuzzle }
]

  val INVSIZE = 6
 (* perhaps a bit long?? *)
  val LENGTH = 60

  val MINMAXDEPTH = 1000000
(*  val MINMAXDEPTH = 999999999 (* XX!!!! *) *)


  val MAXDEPTH = 3000000
  val seed = ref (0wxDEADBEEF : Word32.word,
		  0wxFEED0001 : Word32.word)

  val start = Time.now ()

  val prefer = "Good"

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
      StringUtil.writefile (prefer ^ "-" ^ (case score of
						SOME score => Int.toString score
					      | NONE => "FAIL") ^ ".puz") s
    end

  fun findbest (bestscore, bestproblem)  =
    let
      val startseed = !seed
      val (pf, (goal, pile)) = 
        Generator.generate 0 seed LENGTH INVSIZE 
          (nil, (nil, [(Solver.Noun "Goal", nil)], Solver.Noun "Goal"));
    in
      tryit (bestscore, bestproblem, pf, goal, pile, startseed)
    end

  and tryit (bestscore, bestproblem, pf, goal, pile, startseed) =
    let

      val maxdepth = Int.max(bestscore * 2, MINMAXDEPTH)

      fun quick_exit l =
        (* if preferred solver failed, just give up *)
         List.exists(fn {name=s, steps=NONE, time=_} => s = prefer | _ => false) l
	 orelse
	 (case ListUtil.findpartial (fn {name=s, steps=SOME x, time=_} => if s = prefer
									  then SOME x
									  else NONE | _ => NONE) l of
	     NONE => (* no score for prefer yet *) false
          | SOME best =>
		(* was this score so bad that our current maxdepth won't even admit
		   a large enough disparity? *)
		(maxdepth - best) <= bestscore    orelse
		(* is there any non-preferred prover whose score is already too good? *)
	 	List.exists (fn { name=s, steps=SOME y, time=_} =>
				s <> prefer
				(* gap will be at most this large.. *)
				andalso (y - best <= bestscore) | _ => false) l
	)

      fun eval l =
        let
        in
          case (List.filter (fn {name=s, steps, time} => s <> prefer) l,
                List.find (fn {name=s, steps, time} => s = prefer) l) of
            (others, SOME {name=_, steps=SOME x, time=_}) =>
            let
	      val others = map (fn {steps=s, ... } => getOpt(s, maxdepth + 1)) others
            in
              foldl Int.min maxdepth others - x
            end
              | _ => 0
        end

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
                   | Proof.Failure s => 
				let val err = "PROOF from " ^ name ^ " doesn't verify: " ^ s ^ "\n"
				in
		                 saveit(NONE, err, (pf, goal, pile));
 				 raise GenLoop (startseed, err,
                                                verifyit (),
                                                pile)
				end);

                     print "+";
                     {name = name,
                      steps = SOME steps,
                      time = Time.- (Time.now (), then_) }

                end handle Proof.OUTATIME => (print "-";
                                              {name = name,
                                               steps = NONE,
                                               time = Time.- (Time.now (), then_) })
                         | Proof.Solver s => 
				let val err = "SOLVER " ^ name ^ " failout: " ^ s ^ "\n"
				in
		                 saveit(NONE, err, (pf, goal, pile));
 				 raise GenLoop (startseed, err,
                                                verifyit (),
                                                pile)
				end
                  

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
                 
                 saveit(SOME score, desc, (pf, goal, pile));
                 tryit(score, (pf, goal, pile), pf, goal, pile, startseed)
               end
             else
               let in
                 print "!";
                 findbest(bestscore, bestproblem)
               end
           end)
    end

  fun loop () = 
    let 
	val default_seed = 0wx3010107
    in
	seed := (case map Int.fromString (CommandLine.arguments ()) of
		   [SOME w] => (print ("Using seed " ^ Int.toString w ^ "...\n"); (0w0, Word32.fromInt w))
                  | _ => (0w0, default_seed));
	
        (* for bug *)
        (* seed := 0wx12505B17; *)
      findbest(~1, (nil, Solver.Noun "ILLEGAL", nil));
      ()
    end handle GenLoop ((see1, see2), s, vfyok, pile) => 
        (print ("\n\n(" ^ Word32.toString see1 ^ "," ^ Word32.toString see2 ^ ") GENLOOP: " ^ s ^ "\n");
         print "\n===== pile =====\n";
         print (StringUtil.delimit "\n" (map Proof.itos pile));
         print "\n";
         print "\n===== generated solution: =====\n";
         print (vfyok ^ "\n"))
    
end

val () = GenLoop.loop ()

