
(* Runs the generator and a collection of solvers,
   limiting their time (in steps) spent trying to solve the
   puzzle. *)

structure UltraLoop =
struct

  exception UltraLoop of string

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
{ name = "GoodNP", solve = GoodP.provepuzzle },
{ name = "GoodP", solve = GoodP.provepuzzle },
{ name = "GoodN", solve = GoodN.provepuzzle }]

  val INVSIZE = 5

  val MAXDEPTH = 3000000

  val task = "Ultra"

  fun saveit (base, score, description, (pf, goal, pile)) =
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

      val f = task ^ "-" ^ Int.toString score ^ base ^ ".puz"
    in
      StringUtil.writefile f s;
      f
    end

  (* number of steps to add in each phase *)
(*
  val INCREMENT = 4
  val BRANCHING = 100
  val OPTIMISM = 1000000 (* VERY full glass *)
*)
  val INCREMENT = 6
  val BRANCHING = 150
  val OPTIMISM = 1000000 (* VERY full glass *)
  val PARFACTOR = 1.1

  fun restart (bestscore, pflen) =
       pflen >= 30 andalso (bestscore <= 30000)

(*  val KEEPTHRESH = 300000 *)

  (* score is number of steps our best solver took,
     bestproblem is the problem that gets that score (a sequent)
     pf is that problem's solution,
     gc is the generator context that can be used to extend it. 

     we try to find a good way to extend the problem that
     improves our score. *)

  fun extend (bestscore, bestproblem : Proof.sequent, 
              pf : Proof.proofstep list, gc : GenUltra.gencontext,
              solvers) = 

      if restart(bestscore, length pf) then loop ()
      else

    let
      fun oneprob _ = 
        Util.ford 0 (INCREMENT - 1)
        (bestproblem, pf, gc)
        (fn (_, (problem : Proof.sequent, pf : Proof.proofstep list, 
                 gc : GenUltra.gencontext)) =>
         let 
           val (step, gc, seq) = GenUltra.generate gc INVSIZE problem
         in
           (seq, step :: pf, gc)
         end)

      (*       val (extprob, extpf, extgc) =  *)
      val probs = List.tabulate (BRANCHING, oneprob)

      fun eval (extprob as (pile, inv, goal), _, _) =
          let
            fun solvewith limit { solve, name } =
                let
                  val (_, steps) =
                      (* just stick inventory on pile... hehe. *)
                      solve limit (goal, inv @ pile, INVSIZE)
                in
                  steps
                end

            (* WARNING. this code is awesome *)
            fun minimize (beststeps, bestsolver, restsolvers) nil = (beststeps, bestsolver :: restsolvers)
              | minimize (beststeps, bestsolver, restsolvers) (this :: rest) =
                let
                  val steps = solvewith (SOME beststeps) this
                in
                  (* must reduce beststeps, since beststeps was limit *)
                  minimize (steps, this, bestsolver :: restsolvers) rest
                end handle Proof.OUTATIME => minimize (beststeps, bestsolver, this :: restsolvers) rest

          in
            case solvers of
              h :: t => 
                  (let 
                       (* since we might never stop, save this thing *)
                       val steps = solvewith NONE h
                   in
                     minimize(steps, h, nil) t
                   end handle Proof.OUTATIME => minimize ((* XXX if some above *) 0 , h, nil) t)
            | _ => raise UltraLoop "no solvers left!"
          end


      fun findhardest (maxsofar, file, (maxsolver, (maxprob, maxpf, maxgc))) nil = 
          (maxsofar, (maxsolver, (maxprob, maxpf, maxgc)))
        | findhardest (maxsofar, file, (maxsolver, (maxprob, maxpf, maxgc))) (prob :: rest) =
          let
              val ((pile, inv, goal), pf, gc) = prob
              (* write to disk *)
              val f = 
                  if bestscore > 800000
                  then (saveit ("-EVAL", 0, "unknown-eval", (map Proof.Pickup inv @ pf, 
                                                             goal, inv @ pile)))
                  else "DELETEME"

              val (score, solvers) = eval prob
              (* delete it *)
              val () = (OS.FileSys.remove f) handle _ => ()
          in
              if score > maxsofar 
              then
                  let 
                      val newfile = 
                          if score > 800000
                          then saveit ("-MAX", score, "current-max", (map Proof.Pickup inv @ pf,
                                                                      goal, inv @ pile))
                          else "DELETEME"
                  in
                      ((OS.FileSys.remove file) handle _ => ());
                      findhardest(score, newfile, (solvers, prob)) rest
                  end
              else findhardest(maxsofar, file, (maxsolver, (maxprob, maxpf, maxgc))) rest
          end

      val (newscore, (* f, *) (solvers, (extprob, extpf, extgc))) =
          findhardest (bestscore, "DELETEME", (solvers, (bestproblem, pf, gc))) probs

      val orders = StringUtil.delimit ", " (List.take(map (fn {name, solve} => name) solvers, 3))

    in
      print ("Newscore: " ^ StringUtil.pad ~14 (Int.toString newscore) ^
             "    proof: " ^ StringUtil.pad ~6 (Int.toString (length extpf)) ^ 
             " steps  (" ^ orders ^ ")\n");
      (* only keep this is we actually improved *)
      if newscore >= bestscore
      (*
      if real newscore >= (real bestscore * (if length extpf < 6 then 1.0 else PARFACTOR))
       *)
      then
        let 
          val (pile, inv, goal) = extprob
          val description = orders
        in
          saveit ("", newscore, description, (map Proof.Pickup inv @ extpf, 
                                          goal, inv @ pile));
          extend (newscore, extprob, extpf, extgc, solvers)
        end
      else extend (bestscore, bestproblem, pf, gc, solvers)
    end

  and loop () =
    extend (0, ([], [(Proof.Noun "Soccer", ["ultra"])], Proof.Noun "Soccer"), nil, GenUltra.initial, solvers)
    
end

val () = UltraLoop.loop ()
  handle UltraLoop.UltraLoop s => print ("ERROR : " ^ s ^ "\n")
