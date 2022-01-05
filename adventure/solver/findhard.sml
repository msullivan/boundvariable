(* Runs the generator and a collection of solvers,
   limiting their time (in steps) spent trying to solve the
   puzzle. *)

structure Findhard =
struct

  exception Findhard of (Word32.word * Word32.word) * string * string * Proof.item list
	exception Bug of string

  structure GoodArg =
  struct
    val coalesce_missings = true
    val analyze_useless = true
    val prefer_incinerate = false
    val comb_newest = true
    val hashing = true
    val lazy_expand = true
  end

  (* best! everything on *)
  structure Good = SolverFn(open GoodArg)
                         
  structure GoodP = SolverFn(open GoodArg
                             val prefer_incinerate = not prefer_incinerate)
  structure GoodN = SolverFn(open GoodArg
                             val comb_newest = not comb_newest)
  structure GoodNP = SolverFn(open GoodArg
			      val comb_newest = not comb_newest
                              val prefer_incinerate = not prefer_incinerate)

  val solvers = [
{ name = "Good",  solve = Good.provepuzzle }, 
{ name = "GoodP", solve = GoodP.provepuzzle },
{ name = "GoodN", solve = GoodN.provepuzzle },
{ name = "GoodNP", solve = GoodNP.provepuzzle }
]

  val INVSIZE = 6
 (* perhaps a bit long?? *)
  val LENGTH = 120

  val MINMAXDEPTH = 1000000

  val MAXDEPTH = 3000000
  val seed = ref (0wxDEADBEEF : Word32.word,
		  0wx1234 : Word32.word)

  val start = Time.now ()

  val harder = "harder_" ^ Int.toString LENGTH ^ "_"

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
      StringUtil.writefile (harder ^ "-" ^ (case score of
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

      val maxdepth = NONE

      (* some solver has lower than bestscore steps to solve? *)
      fun quick_exit l =
         List.exists(fn {name=s, steps=SOME x, time=_} => x < bestscore | _ => false) l

      fun eval l =
        let
  	  val scores =
             List.mapPartial (fn {name=s, steps, time} => steps) l
        in
           (* minimum across all solvers *)
	   (case scores of
		nil => raise Bug "nobody solved it?"
	| h :: t => foldl Int.min h t)
        end

      fun verifyit () =
          (case Proof.verify INVSIZE (pile, nil, goal) pf of
              Proof.Success => "(verified)"
            | Proof.Failure s => ("(couldn't verify generated proof!: " ^ s ^ ")"))


      val _ = print ":"

      val results =
        let
          fun trying (nil, done, maxdepth) = SOME (rev done)
            | trying ( {name, solve} :: t, done, maxdepth ) = 
            let 
              val maxdepth = if null done then NONE
			     else SOME (eval done)
		
              val then_ = Time.now ()

              val this =
                let
                  val (p, steps) = solve maxdepth (goal, pile, INVSIZE)
                in
                  (case Proof.verify INVSIZE (pile, nil, goal) p
                     of Proof.Success => (* print "YES\n" *) ()
                   | Proof.Failure s => 
				let val err = "PROOF from " ^ name ^ " doesn't verify: " ^ s ^ "\n"
				in
		                 saveit(NONE, err, (pf, goal, pile));
 				 raise Findhard (startseed, err,
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
 				 raise Findhard (startseed, err,
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
              else trying (t, newdone, maxdepth)
            end
        in
          trying (solvers, nil, maxdepth)
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
		   [SOME w] => (print ("Using seed " ^ Int.toString w ^ "...\n"); (0w7, Word32.fromInt w))
                  | _ => (0w7, default_seed));
	
        (* for bug *)
        (* seed := 0wx12505B17; *)
      findbest(~1, (nil, Solver.Noun "ILLEGAL", nil));
      ()
    end handle Findhard ((see1, see2), s, vfyok, pile) => 
        (print ("\n\n(" ^ Word32.toString see1 ^ "," ^ Word32.toString see2 ^ ") GENLOOP: " ^ s ^ "\n");
         print "\n===== pile =====\n";
         print (StringUtil.delimit "\n" (map Proof.itos pile));
         print "\n";
         print "\n===== generated solution: =====\n";
         print (vfyok ^ "\n"))
    
end

val () = Findhard.loop ()

