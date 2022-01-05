functor SolverFn ((* needed for safety *)
		  val coalesce_missings : bool
                  val analyze_useless : bool
                  val prefer_incinerate : bool
                  val comb_newest : bool
                  val lazy_expand : bool
                  val hashing : bool) =

struct

  structure GA = GrowArray
  structure IIM = SplayMapFn(type ord_key = int * int
                             val compare = Util.lex_order Int.compare Int.compare)

  open Proof

  fun showtable table =
    let
      val a = GA.finalize table
    in
      print "\n";
      Array.appi (fn (x, p) => print (Int.toString x ^ " " ^
                                      ptos p ^ "\n")) a
    end

  fun canonize (Noun a) = Noun a
    | canonize (Missing (prop, nil)) = canonize prop
    | canonize (Missing (prop, pl)) =
    let
      val pl = 
        (* COALESCE optimization *)
        if coalesce_missings
        then ListUtil.sort propcmp (map canonize pl)
        else pl
    in
      Missing(canonize prop, pl)
    end

  (* return the index of this prop in the table *)
  fun addgetprop table (p : prop) =
      let
          fun findy n =
              if n >= GA.length table
              then (GA.append table p;
                    (GA.length table - 1, true))
                   (* assumes canonized *)
              else if GA.sub table n = p
                   then (n, false)
                   else (findy (n + 1))
      in
          findy 0
      end

  fun addprop_eager addtransition table p =
    let
      val p = canonize p
      val (i, _) = addgetprop table p
    in
      (case p of
         Noun a => ()
       | (Missing (prop, l)) => 
           let
             (* PERF shouldn't do this so eagerly. instead, put it in the
                database as-is, and later repeatedly do this ONLY WHEN CHOICE
                IS IN THE DATABASE ALREADY. (until we reach a fixed point) *)
             fun chooseone (choice, rest) =
               (* add transition between
                  wholeprop * choice  
                  and
                  rest -> prop *)
               addtransition (i, 
                              addprop_eager addtransition table choice,
                              addprop_eager addtransition table (Missing (prop, rest)))
           in
             ignore (ListUtil.choosemap chooseone l)
           end);
         i
    end

  (* PERF quadratic, but still much better than above *)
  fun addprop_lazy addtransition table pnew = 
      let
          val pnew = canonize pnew
          val (i, new) = addgetprop table pnew
      in
          (* see if we've unlocked anything by adding this proposition *)
          if new
          then
          ( (* print ("add new prop " ^ ptos pnew ^ "\n"); *)
          Util.for 0 (GA.length table - 1)
          (fn j =>
           let
               val pold = GA.sub table j

               (* try to fix the first thing with the second. if possible,
                  then add the (partially) fixed item to the table and also
                  add the transition to the transitiontable. *)
               fun try ((fi, Missing(fixed, missing)), (ai, p)) =
                   ignore
                   (ListUtil.choosemap (fn (choice, rest) =>
                                       (* might be able to fix it
                                          with this! *)
                                        if 
                                          ( (* print (ptos p ^ " =?= " ^ ptos choice ^ "\n"); *)
                                            (* (should be canonized) *)
                                            p = choice
                                           (* propeq(p, choice) *))
                                       then
                                           let 
                                               (* val () = print "HIT!\n" *)
                                               val z = addprop_lazy addtransition table
                                                   (Missing(fixed, rest))
                                           in
                                               addtransition(fi, ai, z)
                                           end
                                       else ()) missing)
                 | try _ = ()
           in
               try ((i, pnew), (j, pold));
               try ((j, pold), (i, pnew))
           end))
          else ();
          i
      end

  val addprop = if lazy_expand 
                then addprop_lazy
                else addprop_eager

  type solvestep = (int * adjective list) step

  exception Done of (solvestep list * int)
  fun solve transitions maxsteps invsize (goal : int) pile =
    let

      val stepsleft = ref maxsteps

      (* compute useless tables *)
      val (transitions, useless) =
        if analyze_useless 
        then
          let

            (* for now, ignore adjectives *)
            val pile = map #1 pile

            (* take a list of transitions and a goal.
               return (useless transitions, useful transitions) *)
            fun find_useless trans goal = 
              let
                (* given a queue of useful ints,

                   and a list that already
                   has useful transitions in it,

                   and a list of possibly
                   useful transitions,

                   move any transitions that
                   result in the int to the
                   useful table, and hereditarily
                   mark the inputs to that
                   transition as useful. *)

                fun copy_useful maybe useful nil = (maybe, useful)
                  | copy_useful maybe useful (g :: rest) =
                  let
                    val (good, maybe) =
                      List.partition (fn ((a, b), c : int) => c = g) maybe

                    fun goods nil = rest
                      | goods (((a,b), _)::more) = a :: b :: goods more
                  in
                    copy_useful maybe (good @ useful) (goods good)
                  end
              in
                copy_useful trans nil [goal]
              end

            (* given a list of transitions,
               and a list of ints that are attainable,
               return
               (unusable transitions, usable transitions) *)
            (* 
               in effect, this pass ignores:
                    1) orderliness of the pile,
                    2) linearity of application, and
                    3) fixed size of inventory
               it could be changed to account for any subset of those three
               (although using all three would be the same code as the main
               search function below).

               also, we can model incineration within the "state-transition"
               search by adding transitions of the form "(X,Y) -> Y" for all X
               and Y.... though this will add lots of transitions.  :(
             *)
            fun find_unattainable trans attainable = 
              let
                fun copy_usable maybe usable attainable =
                  case List.partition (fn ((a : int, b : int), c) => 
                                       List.exists (fn a' => a = a') attainable
                                       andalso
                                       List.exists (fn b' => b = b') attainable) maybe of
                    (nil, maybe) => (maybe, usable)
                  | (l, maybe) => copy_usable maybe (l @ usable) (map #2 l @ attainable)
              in
                copy_usable trans nil pile
              end

            fun loop trans =
              let
                val (_, useful) = find_useless trans goal
                val (maybe, usable) = find_unattainable useful pile
              in
                case maybe of
                  nil => usable
                | _ => loop usable
              end

            (* val () = print ("Transitions before: " ^ Int.toString (length transitions) ^ "\n") *)
            val transitions = loop transitions
            (* val () = print ("Transitions after:  " ^ Int.toString (length transitions) ^ "\n") *)

            val useless =
              List.filter (fn x => x <> goal
                           andalso
                           List.all (fn ((a, b), c) =>
                                     x <> a andalso x <> b) transitions) pile

              (*
            val () = print ("Useless items: " ^ Int.toString (length useless) ^ "/"
                            ^ Int.toString (length pile) ^ "\n")
            *)
          in
            (transitions, useless)
          end
        else (transitions, nil)


      val transitions = 
        let 
          val re = ref IIM.empty : (int IIM.map) ref
        in
          app (fn ((l, r), dest) => 
               re := (IIM.insert (!re, (l, r), dest))) transitions;
          !re
        end


      val called = ref 0

      val HASHSIZE = 313373

      fun hashcode inventory pileidx =
        Word.toInt
        (Word.andb
         (0wx7FFFFFF,
         foldl (fn ((a, _), b) => 
               let val a = Word.fromInt a
               in
                 Word.*
                 (Word.xorb(Word.<<(b, 0w3), Word.>>(b, Word.fromInt Word.wordSize - 0w3)),
                  a)
               end) (Word.fromInt pileidx) inventory)) mod HASHSIZE

      val hashtable = 
          if hashing then Array.array(313373, nil : int list list)
          else Array.fromList nil
      fun beenhere_add (inv : (int * adjective list) list) pi =
        if hashing
        then
        let val c = hashcode inv pi
          val this = (pi, nil) :: inv
          val l = Array.sub(hashtable, c)
        in
          if List.exists (fn that => ListUtil.all2 (fn ((a, _), b) => a = b) this that) l
          then true
          else (Array.update(hashtable, c, (map #1 this) :: l);
                false)
        end
        else false

      fun add_inventory new old =
        if comb_newest
        then
          ListUtil.Sorted.insert (ListUtil.byfirst Int.compare) old new
        else
          ListUtil.Sorted.insert (ListUtil.byfirst (ListUtil.Sorted.reverse Int.compare)) old new

      val pile = Vector.fromList pile

      (* inv is a list of the current contents of
         the inventory; it has length at most invsize - 1 *)
      fun searchnew path inventory pileidx : unit =

        if beenhere_add inventory pileidx
        then ()
        else
        (* did we win? *)
        if List.exists (fn (i, _) => i = goal) inventory 
        then 
          let 
            val steps = rev path
          in
            (* print ("Called: " ^ Int.toString (!called) ^ "\n"); *)
            raise Done (steps, !called)
          end
        else

          (* try all actions on the top of the pile: *)
          let in
            called := !called + 1;

            (case !stepsleft of
               NONE => ()
             | SOME 0 => raise OUTATIME
             | SOME x => stepsleft := SOME (x - 1));

            if pileidx < Vector.length pile
            then 
              let 
                val (a, adjs) = Vector.sub(pile, pileidx)
                fun comb () =
                  (* don't try picking up useless stuff! *)
                  (* PERF useless could be bitmask *)
                  if not (List.exists (fn a' => a = a') useless)
                  then trycombine (Pickup(a, adjs) :: path) inventory (pileidx + 1) (a, adjs)
                  else ()
                fun trash () = 
                  searchnew (Incinerate(a, adjs) :: Pickup(a, adjs) :: path) inventory (pileidx + 1)
              in
                if prefer_incinerate
                then (trash (); comb ())
                else (comb (); trash ())
              end
            else () (* no more pile *)
          end

      (* trycombine path inventory pileidx item
         
         we know we may have opened up opportunities for combination
         with our current inventory, because we just discovered ITEM.
         try combining ITEM with everything that it's compatible with
         in the INVENTORY.

         Also try just keeping it in our inventory without combining.

         *)
      and trycombine path inventory pileidx (item as (a, adjs)) =
        (* try combining things already in the inventory *)
        let in
          (* for each item in inventory ... *)
          ListUtil.choosemap
          (fn ((b, adjs'), inventory) =>
           let in

             (* maybe we can fix the a with the b? *)
             (case IIM.find (transitions, (a, b))
                of SOME c => trycombine (Combine((a, adjs), (b, adjs')) :: path)
                                    inventory pileidx (c, adjs)
              | NONE => ());

                (* or fix the b with the a? *)
             (case IIM.find (transitions, (b, a))
                of SOME c => trycombine (Combine ((b, adjs'), (a, adjs)) :: path)
                                    inventory pileidx (c, adjs')
              | NONE => ())
           end) inventory;

          (* Also, try NOT combining things, if we have room in our
             inventory to keep this item around *)
          if List.length inventory < (invsize - 1)
          then searchnew path (add_inventory item inventory) pileidx
          else ()
        end
    in
      searchnew nil nil 0
    end


  fun provepuzzle maxsteps gpi = 
    let 
        val table = GA.empty () : prop GA.growarray

        fun solvepuzzle (goal, pile, invsize) =
          let
          (* map indices to props,
             every prop is in the table at most once *)

          val transitions = ref nil : ((int * int) * int) list ref
          fun addtransition (l, r, dest) =
              let (* val _ = print ("adding trans " ^ (Int.toString l) ^ ", " ^ (Int.toString r)
                                 ^ " -> " ^ (Int.toString dest) ^ "\n") *)
              in 
                transitions := ((l, r), dest) :: !transitions
              end

          val g = addprop addtransition table goal
          val pile = map (fn (p, a) => (addprop addtransition table p, a)) pile

          fun showtransitions l =
              let in
                  print "== transitions ==\n";
              app (fn ((f, a), r) =>
                   print (Int.toString f ^ "(" ^ Int.toString a ^ ") -> " ^
                          Int.toString r ^ "\n")) l
              end

        in
(*
          if (GA.length table > 100)
          then print ("(" ^ Int.toString (GA.length table) ^ ")")
          else ();
*)
(*
          showtable table;
          showtransitions (!transitions);
*)
          (* print ("Table size: " ^ (Int.toString (GA.length table)) ^ "\n"); *)
          (solve (!transitions) maxsteps invsize g pile;
           raise Solver "can't solve")
          handle Done r => r
        end

      val (s, n) = solvepuzzle gpi

      fun cvt (a, aa) = (GA.sub table a, aa)

    in 
      (map (step_map cvt) s, n)
    end

end

structure Solver = SolverFn(val coalesce_missings = true
                            val analyze_useless = true
                            val prefer_incinerate = true
                            val comb_newest = true
                            val lazy_expand = true
                            val hashing = true)
