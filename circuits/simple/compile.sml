
structure Compile =
struct

  (* compile from minml to circuit language (list of modules) *)

  (* generate a new string *)
  local
    val r = ref 0
  in
    fun newstr base =
      (r := !r + 1;
       base ^ "_" ^ Int.toString (!r))

    fun newstring () = newstr "x"
  end

  structure C = Circuits
  datatype inface = datatype C.inface
  datatype outface = datatype C.outface

  exception Compile of string
  open MinML

  fun compile (funs) =
    let
      
      val mods = ref (nil : Circuits.module list)

      (* a single function, add it (and maybe other stuff) to mods
         under the given name.

         Calling convention:
         First arg is W.
         The rest of the args are sent to N as a right-associated
         pairlist. (a1, (a2, (a3, a4)))

         *)

      datatype prewire_source = PWS_N   | PWS_W | PWS_BOX of string * outface
      datatype prewire_dest   = PWD_OUT | PWD_BOX of string * inface

      type prewire = string * prewire_source * prewire_dest list ref
        

      fun comp (name, args, body) =
        let

          fun error s = raise Compile ("(" ^ name ^ ") : " ^ s)

          val boxes = ref nil
            
          (* maps vars to prewires *)
          val prewires = ref nil

          (* adds prewire to list *)
          fun newwire var boxname outface = 
            prewires := (var, PWS_BOX(boxname, outface), ref nil) :: !prewires

          (* add the box *)
          fun addbox (name, cmd) = 
            let
              val name = newstr name
            in
              boxes := (name, cmd) :: !boxes;
              name
            end

          fun adddest var dst =
            let
              fun uw nil = error ("wire called " ^ var ^ " not found")
                | uw ((w, src, dsts) :: rest) =
                if w = var
                then dsts := dst :: !dsts
                else uw rest
            in
              uw (!prewires)
            end

          (* connect the named prewire to this box at this in-interface *)
          fun usewire var boxname inface = adddest var (PWD_BOX(boxname, inface))


          (* convert expression e (pushing boxes onto list),
             if necessary.

             a variable name corresponding to the output.
             *)
          fun c exp =
            (case exp of
               (* XXX implement directly! *)
               Let(v, e1, e2) => c (Split(Pair(e1, Unit), v, newstr "unused_let", e2))
             (* these are probably right as derived forms *)
             | Pi1 e => 
                 let val l = newstr "pi1"
                 in c (Split(e, l, newstr "unused", Var l))
                 end
             | Pi2 e => 
                 let val r = newstr "pi2"
                 in c (Split(e, newstr "unused", r, Var r))
                 end
             | Inl e =>
                 let 
                   val v = c e 
                   val r = newstr "inl_res"
                   val b = addbox("inl", C.Send1(C.Inl(C.Face W), S))
                   val () = usewire v b W
                   val () = newwire r b S
                 in
                   r
                 end
             | Inr e =>
                 let 
                   val v = c e 
                   val r = newstr "inr_res"
                   val b = addbox("inr", C.Send1(C.Inr(C.Face W), S))
                   val () = usewire v b W
                   val () = newwire r b S
                 in
                   r
                 end
             | Pair (e1, e2) => 
                 let
                   val v1 = c e1
                   val v2 = c e2
                   val r = newstr "pair_res"
                   val b = addbox("pair", C.Send1(C.Pair(C.Face W, C.Face N), S))
                   val () = usewire v1 b W
                   val () = usewire v2 b N
                   val () = newwire r b S
                 in
                   r
                 end
             | Unit =>
                 let
                   val r = newstr "unit"
                   val b = addbox("unit", C.Send1(C.Unit, S))
                 in
                   newwire r b S;
                   r
                 end
             | Var x => x

             | App (Var f, el) => 
                 if length el > 2
                 then
                   (case el of
                      w :: rest =>
                        let
                          fun assoc [a, b] = Pair (a, b)
                            | assoc (h :: t) = Pair(h, assoc t)
                            | assoc _ = error "impossible assoc"
                        in
                          c (App(Var f, [w, assoc rest]))
                        end
                    | _ => error "impossible (app long1)")
                 else
                   let
                     val b = addbox("f", C.Use f)
                     val r = newstr (f ^ "_res")
                     val () = newwire r b E
                   in
                     (case map c el of
                        nil => error "functions with no arguments are too eager; send unit"
                      | [v] => usewire v b W
                      | [v1, v2] => (usewire v1 b W;
                                     usewire v2 b N)
                      | _ => error "impossible (app/inside)");
                     r
                   end

             | Split (e, v1, v2, bod) =>
                   let
                     val v = c e
                     val b = addbox("split", C.Split(C.Face W))
                     val () = usewire v b W
                     val () = newwire v1 b S
                     val () = newwire v2 b E
                   in
                     c bod
                   end
                 
             | App (_, _) => error "should only apply variables!"

             | Case _ => 
                   (* generate function *)
                   (* urg, need to closure convert *)
                   let
                     (* get free variables, but not function variables *)
                     val fvs = 
                       List.filter (fn v =>
                                    not (List.exists (fn (v', _, _) => v = v') funs))
                                      (freevars exp)

                     val name = newstr "c"
                   in
                     (* implement as a module *)
                     comp (name, fvs, exp);
                     (* then use it *)
                     c (App(Var name, map Var fvs))
                   end)


          (* FIXME this is wrong because we don't reflect
             the control-flow effect unless el and er depend on v. *)
          fun outerc (Case (e, v, el, er)) =
                let
                  val obj = c e
                  val b = addbox("case", C.Case(C.Face W, S, E))
                  val vl = newstr "casel"
                  val vr = newstr "caser"
                  val () = usewire obj b W
                  val () = newwire vl b S
                  val () = newwire vr b E

                  (* induce control-dependencies.
                     (XX this doesn't work, and doesn't help) *)
                  val el = Let(newstr "ctrl", Var vl, minml_subst v (Var vl) el)
                  val er = Let(newstr "ctrr", Var vr, minml_subst v (Var vr) er)
                in
                  (* PERF should recurse to outerc, in case these
                     are also cases--no need to generate function
                     again. (should also work for split?) *)
                  [c el,
                   c er]
                end
            | outerc e = [c e]
            

          (* using calling convention to set up initial
             wires. *)
          fun makeinputs () =
            (case args of
               nil => ()
             | [w] => prewires := (w, PWS_W, ref nil) :: !prewires
             | w :: rest => 
                 let
                   fun calling src [one] = prewires := (one, src, ref nil) :: !prewires
                     | calling src [one, two] =
                     let 
                       val b = addbox("argchain2", C.Split(C.Face N))
                       val v = newstr "northchain2"
                     in
                       (* this is the wire coming from the source to the
                          split box *)
                       prewires := (v, src, ref nil) :: !prewires;
                       usewire v b N;
                       
                       (* then add the two wires coming off the box *)
                       prewires := (one, PWS_BOX (b, S), ref nil) ::
                                   (two, PWS_BOX (b, E), ref nil) :: !prewires
                     end
                     | calling src (h :: t) =
                     let
                       val b = addbox("argchain", C.Split(C.Face N))
                       val v = newstr "northchain"
                     in
                       prewires := (v, src, ref nil) :: !prewires;
                       usewire v b N;

                       prewires := (h, PWS_BOX (b, S), ref nil) :: !prewires;
                       calling (PWS_BOX(b, E)) t
                     end
                     | calling _ _ = error "impossible (calling)"
                 in
                   prewires := (w, PWS_W, ref nil) :: !prewires;
                   calling PWS_N rest
                 end)

          (* set up initial wires *)
          val () = makeinputs ()

          (* compile the body *)
          val outs = outerc body

          (* connect the result to the outputs *)
          val () = app (fn w => adddest w PWD_OUT) outs

          (* linearize wire uses *)
          fun linearize nil = nil
            | linearize ((w, s, ref [d]) :: rest) = (w, s, d) :: linearize rest
            | linearize ((w, s, ref nil) :: rest) =
            let
              (* PERF could just use any old box around with 1 input,
                 as long as it doesn't create cycles *)
              val b = addbox("weaken-" ^ w, C.Sink)
            in
              (w, s, PWD_BOX(b, W)) :: linearize rest
            end
            | linearize ((w, s, ref (h::t)) :: rest) =
            let
              val b = addbox("contract-" ^ w, C.Send2(C.Face W, E, C.Face N, S))
            in
              (newstr ("lcon-" ^ w), s, PWD_BOX(b, W)) ::
              (newstr ("lct-" ^ w), PWS_BOX(b, E), h) ::
              linearize ((w, PWS_BOX(b, S), ref t) :: rest)
            end

          val pwires = linearize (!prewires)

          fun getinput s =
            case List.filter (fn (w, src, dst) => src = s) pwires of
              nil => NONE
            | [(w, _, PWD_BOX dst)] => SOME dst
            | [_] => ((*error (name ^ " ... FIXME oops, in->out wire\n");*) NONE)
            | _ => error "oops, too many inputs"
            
        in

          (* add module to list *)
          mods := (name, !boxes, 
                   (* wires are box-box wires *)
                   List.mapPartial (fn (w, PWS_BOX src, PWD_BOX dst) =>
                                     SOME(src, dst)
                                    | _ => NONE) pwires,
                   getinput PWS_N, 
                   getinput PWS_W,
                   (* outputs *)
                   List.mapPartial (fn (w, PWS_BOX src, PWD_OUT) =>
                                     SOME src
(*                                     | (w, _, PWD_OUT) =>
                                     error "oops, wire goes in->out" *)
                                     | _ => NONE) pwires)
               :: !mods;
          ()
        end

    in
      app comp funs;
      !mods
    end

end
