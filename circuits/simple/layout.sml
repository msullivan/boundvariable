
structure SimpleLayout =
struct

  exception SimpleLayout of string
  exception Unimplemented
  open Circuits

  fun itos N = "N"
    | itos W = "W"

  fun otos S = "S"
    | otos E = "E"

  fun etos Unit = "()"
    | etos (Pair (e1, e2)) = "(" ^ etos e1 ^ ", " ^ etos e2 ^ ")"
    | etos (Inl e) = "Inl " ^ etos e
    | etos (Inr e) = "Inr " ^ etos e
    | etos (Face N) = "N"
    | etos (Face W) = "W"

  fun ctos Sink = "send []"
    | ctos (Send1 (e, ou)) = "send [(" ^ etos e ^ ", " ^ otos ou ^ ")]"
    | ctos (Send2 (e, ou, e', ou')) = "send [(" ^ etos e ^ ", " ^ otos ou ^ ")," ^
                                            "(" ^ etos e' ^ ", " ^ otos ou' ^ ")]"
    | ctos (Case (e, o1, o2)) = "case " ^ etos e ^ " of " ^ otos o1 ^ ", " ^ otos o2
    | ctos (Split e) = "split " ^ etos e
    | ctos (Use s) = "use " ^ s

  (* turn a module into a string list, each of
     the same length *)
  fun laymod_inside (name, boxes, wires, north, west, east) =
    let
        val (b, w, north, west, east) =
        let
            (* boxes will now be known by their indices in the vector *)
            val b = Vector.fromList boxes
            val w = Vector.fromList (map (fn ((src, outi), (dst, ini)) =>
                                          case (Vector.findi (fn (i, (name, _)) => name = src) b,
                                                Vector.findi (fn (i, (name, _)) => name = dst) b) of
                                            (SOME (si, _), SOME (di, _)) => ((si, outi), (di, ini))
                                          | _ => raise SimpleLayout ("couldn't find boxes " ^
                                                                     src ^ " / " ^ dst)) wires)
            fun fixend (name, iface) = 
                case Vector.findi (fn (_, (name', _)) => name = name') b of
                    SOME (i, _) => (i, iface)
                  | _ => raise SimpleLayout ("couldn't find input box " ^ name)


            val north = Option.map fixend north
            val west  = Option.map fixend west
            val east  = map fixend east

(*
            val _ = Vector.appi (fn (i, (name, cmd)) =>
                                 print (Int.toString i ^ " '" ^ name ^ "' = " ^ ctos cmd ^ "\n")) b
*)
            (* no need for names any more *)
            val b = Vector.map (fn (name, cmd) => cmd) b

            (* first, topologically sort the boxes using wires as dependencies. 
               we don't care about the inputs and outputs, however. *)

            val nodes = Vector.mapi (fn (i, _) => TopoSort.node i) b

            val constraints = Vector.foldl (fn (((bef, _), (aft, _)), l) => 
                                            TopoSort.constraint (Vector.sub (nodes, bef), 
                                                                 Vector.sub (nodes, aft)) :: l) nil w
            (* box indices in dependency order *)
            val order = map TopoSort.get (TopoSort.sort (Vector.foldr op:: nil nodes) constraints)

            (* inverse map; nth element gives that box's position in the output *)
            val order_inv =
                let
                    val a = Array.array(Vector.length b, ~1)
                in
                    ListUtil.appi (fn (boxi, orderi) =>
                                   Array.update(a, boxi, orderi)) order;
                    Array.vector a
                end

            (* make wires work with the new order. *)

            val w = Vector.map (fn ((si, outi), (di, ini)) =>
                                ((Vector.sub(order_inv, si), outi),
                                 (Vector.sub(order_inv, di), ini))) w

(*
            val _ = print "\n\nSorted:\n"
            val _ = Vector.appi (fn (i, j) => print ("box " ^ Int.toString i ^ " at pos " ^ Int.toString j ^ "\n")) 
                                order_inv

            val _ = print "\nWith wires:\n"
            val _ = Vector.app (fn ((si, outi), (di, ini)) =>
                                print
                                (Int.toString si ^ ":" ^ otos outi ^ " => " ^
                                    Int.toString di ^ ":" ^ itos ini ^ "\n")) w
*)

            fun reend (id, iface) = (Vector.sub(order_inv, id), iface)

        in
            (Vector.fromList (map (fn i => Vector.sub(b, i)) order),
             w, 
             Option.map reend north, 
             Option.map reend west, 
             map reend east)
        end

        (* at this point, b is vector of commands in topologically sorted order.
           each wire starts with a box at index i and ends at some later index j.
           *)

      val lines = ref nil
      fun addline l = lines := l :: !lines

      (* for each box index, we print out that chunk of the module,
         based on the current wire status, and then return the new
         wire status
        *)

      datatype wire_status = 
          (* box at which we'll stop (or NONE if output) *)
          Laying of (int * inface) option
        | Waiting of { start : int * outface,
                       finish : (int * inface) option }
          (* already connected it up *)
        | Done

      (* all wires, in initial configuration *)
      val wstat =
          let
              val wl = Vector.foldr op:: nil w
          in
              Array.fromList (map (fn (x, y) => Waiting { start = x,
                                                          finish = SOME y }) wl @

                              (case west of
                                   NONE => nil
                                 | SOME x => [Laying (SOME x)]) @
                              (case north of
                                   NONE => nil
                                 | SOME x => [Laying (SOME x)]) @

                              map (fn x => Waiting { start = x, finish = NONE }) east)
          end

      fun doboxes () =
          Util.for 0 (Vector.length b - 1)
          (fn i =>
           let
               val cmd = Vector.sub(b, i)
               val s = ctos cmd


               fun anywire f = Array.exists f wstat
                   
               (* bools; true if this box has hook-up at 
                      north, west, south east *)
               val (nn, ww, ss, ee) =
                 (anywire (fn (Laying (SOME (stop, N))) => stop = i | _ => false),
                  anywire (fn (Laying (SOME (stop, W))) => stop = i | _ => false),
                  anywire (fn (Waiting {start = (start, S), finish}) => start = i | _ => false),
                  anywire (fn (Waiting {start = (start, E), finish}) => start = i | _ => false))

               datatype phase =
                   (* no wires turn here *)
                   NeutralTop
                 | NeutralBot
                 | HitWest
                   (* need to turn and make a v *)
                 | HitNorth1
                 | HitNorth2
                 | GetSouth
                   (* width of command *)
                 | GetEast of int

               fun wires phase =
                   let 
                       val s = ref ""
                       (* if true, then we are drawing a wire horizontally,
                          so draw | as # and space as - *)
                       val xover = ref false

                       fun start_xover () =
                           if !xover
                           then raise SimpleLayout "xover when already xover!"
                           else xover := true

                       fun space () = if !xover then "-" else " "
                       fun vbar  () = if !xover then "#" else "|"
                   in
                       Util.for 0 (Array.length wstat - 1)
                       (fn j =>
                        s := !s ^ 
                        (case Array.sub(wstat, j) of
                             Done => space ()
                           | Laying NONE => vbar ()
                           | Laying (SOME (stop, iface)) => 
                                 if stop = i
                                 then (case (iface, phase) of
                                           (W, HitNorth1)  => vbar ()
                                         | (W, HitNorth2)  => vbar ()
                                         | (W, NeutralTop) => vbar ()
                                         | (W, HitWest) =>
                                               let in
                                                   start_xover ();
                                                   "+"
                                               end
                                         | (W, NeutralBot) => space ()
                                         | (W, GetSouth)   => space ()
                                         | (W, GetEast _)  => space ()
                                         | (N, HitNorth1)  => 
                                               let in
                                                   start_xover ();
                                                   "+"
                                               end
                                         (* we only act in HitNorth phase *)
                                         | (N, _) => space ())
                                 else vbar ()
                           | Waiting { start = (start, oface), finish } =>
                                 if start = i
                                 then (case (oface, phase) of
                                           (E, HitNorth1)  => space ()
                                         | (E, HitNorth2)  => space ()
                                         | (E, NeutralTop) => space ()
                                         | (E, HitWest)    => space ()
                                         | (E, NeutralBot) => space ()
                                         | (E, GetEast _)  =>
                                               let in
                                                   start_xover ();
                                                   "+"
                                               end
                                         | (E, GetSouth)   => vbar ()
                                               
                                         | (S, HitNorth1)  => space ()
                                         | (S, HitNorth2)  => space ()
                                         | (S, NeutralTop) => space ()
                                         | (S, HitWest)    => space ()
                                         | (S, NeutralBot) => space ()
                                         | (S, GetEast _)  => space ()
                                         | (S, GetSouth)   =>
                                               let in
                                                   start_xover ();
                                                   "+"
                                               end)

                                 else space ()
                        ));

                       s := !s ^ 
                       (case (phase, (nn, ee, ss, ww)) of
                            (HitWest, (_, _, _, true)) => "->"
                          | (HitNorth1, (true, _, _, _)) => "---+"
                          | (HitNorth2, (true, _, _, _)) => "   v"
                              (* XXX ... *)
                          | (GetSouth, (_, _, true, _)) => "-----+"
                          | (GetEast width, (_, false, true, _)) =>
                                                                ("     |")
                          | (GetEast width, (_, true, s, _)) => ("-----" ^
                                                                 (* maybe crossover from south *)
                                                                 (if s then "#"
                                                                  else "-") ^
                                                                 CharVector.tabulate(width - 2,
                                                                                     fn _ => #"-") ^
                                                                 "+")
                          | _ => "  ");

                       !s
                   end

               fun nextwires () =
                   Array.modify
                   (fn m =>
                    case m of
                        Done => m
                      | Laying NONE => m
                      | Laying (SOME (stop, _)) =>
                            if stop = i
                            then Done
                            else m
                      | Waiting { start=(start, _), finish } =>
                            if start = i
                            then Laying finish
                            else m) wstat

               fun debug_hits (n, s, e, w) =
                 (if n then "N" else " ") ^
                 (if s then "S" else " ") ^
                 (if e then "E" else " ") ^
                 (if w then "W" else " ")

               (* wire coming out of east side, if appropriate *)
               fun eastside1 () =
                 if ee then "+"
                 else ""
                   
               fun eastside2 () =
                 if ee then "|"
                 else ""

           in
               (* PERF shouldn't run wire phases we don't need *)
               addline (wires HitNorth1);
               addline (wires HitNorth2);
               addline (wires NeutralTop ^ "*" ^ CharVector.tabulate(size s, fn _ => #"=") ^ "*");
               addline (wires HitWest    ^ "!" ^ s ^ "!" ^ eastside1 ());
               addline (wires NeutralBot ^ "*" ^ CharVector.tabulate(size s, fn _ => #"=") ^ "*" ^ eastside2 ());
               addline (wires (GetEast (size s)));
               addline (wires GetSouth);
               nextwires ()
           end)

      (* normal wires *)
      val normw = Vector.length w
      val westw = Option.isSome west
      val northw = Option.isSome north


      val north_pos = Option.map (fn _ => normw + (if westw then 1 else 0)) north
      val west_pos = Option.map (fn _ => 1) west

        
      (* read wstat to find columns with outs *)
      fun outs () =
        let
          val os = ref nil
        in
          Util.for 0 (Array.length wstat - 1)
          (fn j =>
           case Array.sub(wstat, j) of
             Laying NONE => os := j :: !os
           | Done => ()
           | _ => raise SimpleLayout "bug: wires still open at end??");
          !os
        end

    in
      (* inputs. *)

      (* first line has module name, also incoming north wire *)
      if northw
      then
         addline (name ^
                  CharVector.tabulate
                  ((normw + (if westw then 1 else 0)) - size name,
                   fn _ => #" ") ^
                  (if northw then "|" else ""))
                      handle Size => raise SimpleLayout ("not enough wires for this long name (" 
                                                         ^ name ^ ")!")
      else (addline name);

      (* second line hooks up westward wire, if any *)
      (* PERF could skip if no west wire? *)
      addline (
               (* leading up to west *)
               CharVector.tabulate
               (normw, fn _ => if westw then #"-" else #" ") ^
               (* then turn, if there's west wire *)
               (if westw then "+" else "") ^
               (* north comes in straight *)
               (if northw then "|" else ""));

      doboxes ();
      (rev (!lines), north_pos, west_pos, outs())
      (* map (fn i => Int.toString i ^ " = " ^ ctos (Vector.sub(b, i))) order *)
    end handle TopoSort.TopoSort s => raise SimpleLayout ("Can't handle flow loops: " ^ s)

  fun laymod r =
    let
      val (lines, northp, westp, outps) = laymod_inside r
      val maxline =
        foldl (fn (s, max) => 
               if size s > max then size s else max) 0 lines
      val len = maxline + 2

    in
      CharVector.tabulate (len,
                           fn 0 => #","
                            | n =>
                           if n = len - 1 then #","
                           else
                             (case northp of
                                SOME m => if (m + 1) = n then #"|" else #"."
                              | NONE => #".")) ::
      ListUtil.mapi (fn (s, i) =>
                     (case westp of
                        SOME m => if m = i then "-" else ":"
                      | NONE => ":") ^ s ^
                        (* pad out to same width *)
                        CharVector.tabulate(maxline - size s,
                                            fn _ => #" ") ^
                        ":") lines @

      let
        fun do_outs (col::rest) =
          CharVector.tabulate(len,
                              fn 0 => #":"
                               | n => 
                              let
                                val active =
                                  List.exists (fn x =>
                                               n = (x + 1)) rest
                              in
                                if n = (col + 1)
                                then #"+" 
                                else 
                                  if n < (col + 1)
                                     (* haven't reached my column yet;
                                        paint active cols as vbars *)
                                  then (if active then #"|"
                                        else #" ")
                                  else
                                    (* past my column. Cross active
                                       wires with # or otherwise hbar *)
                                  (* n > (col + 1) *)
                                  (if active then #"#"
                                   else #"-")
                              end) ::
          do_outs rest
          | do_outs nil = nil
      in
        do_outs outps
      end @
      [CharVector.tabulate (len,
                            fn 0 => #","
                             | n => 
                            if n = len - 1 then #","
                            else #".")]
    end

  (* layout a list of programs. For now, doesn't try
     to arrange these cleverly in a grid--but it should! *)
  fun layprog ml =
    let
      val mods = map laymod ml
    in
      app
      (fn ll =>
       let in
         app (fn l => print (l ^ "\n")) ll;
         (* XXX skip line; not necessary *)
         print "\n"
       end) mods
    end


end

