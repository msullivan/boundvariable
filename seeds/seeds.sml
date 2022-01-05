structure Seeds =
struct

  (* 

   A simple, bit-based, lossy virtual machine.  The machine has two symmetric
   stacks (called Input and Output), four special-purpose registers (including
   an instruction pointer), and a vector of immutable instructions.
   Everything is pretty normal, except that once in a while, one of the
   registers will be miraculously set to zero.

   The idea would be to require contestents to implement something like an
   8-bit adder (or something even simpler like XOR?) with a fixed amount of
   accuracy (where, for example, the two input words appear, one after the
   other, on the input stack, and the machine should halt with the answer on
   the output stack.

   *)

  structure L = List
  structure V = Vector
  structure A = Array
  structure R = Random

  val BIRD_PERIOD = 12

  val NUMBER_OF_REGS = 2
  datatype reg = R1 | R2 (* Result registers *)

  datatype io = IN | OUT

  datatype inst =
      PSH of io * int (* push T onto stack at position w *)
    | POP of io       (* T <- top of stack; pop stack *)
    | CPY of reg      (* Ri <- T *)
    | DIS of reg      (* Ri <- Ri or T *)
    | NEG of reg      (* Ri <- neg T *)
    | VOT of reg      (* Ri <- [two or more of T, R1, R2 are true] *)
    | RES of reg      (* T <- Ri *)
    | HLT

  datatype trans =
      Inst of inst
    | Rand of reg

  type mach = { istack : bool list ref,
                ostack : bool list ref,
                rregs : bool A.array,    (* Result registers *)
                treg : bool ref,         (* Transfer register *)
                ireg : int ref,          (* Instruction pointer *)
                code : inst V.vector }

  exception Halt of mach
  exception Stuck

  fun mkmach p istack = { istack = ref istack,
                          ostack = ref nil,
                          rregs = A.fromList [false, false],
                          treg = ref false,
                          ireg = ref 0,
                          code = p }

  fun run (p : inst list) 
          (istack : bool list)
          (d : ((mach * trans) -> unit) option) =
      let val m = mkmach (V.fromList p) istack
          val { istack, ostack, rregs, treg, ireg, code } = m

          val rand = R.rand (13, 23)

          fun regtoidx r = (case r of R1 => 0 | R2 => 1)
          fun idxtoreg i = (case i of 0 => R1 | 1 => R2 | _ => raise Match)

          fun getstack IN = istack
            | getstack OUT = ostack

          fun booltoint true = 1 
            | booltoint false = 0

          fun step () = 
              let val i = V.sub (code, !ireg)
                      handle Subscript => raise Halt m
                  val () = ireg := (!ireg) + 1
              in
                case i of
                  PSH (io, w) => let val stack = getstack io in
                                   stack := (L.take (!stack, w))
                                            @((!treg)::(L.drop (!stack, w)));
                                  rundebug (Inst i);
                                  birdsattack ()
                            end
                | POP io => let val stack = getstack io in
                             case !stack of
                               b::stack' => (treg := b; 
                                        stack := stack'; 
                                        rundebug (Inst i);
                                        birdsattack ())
                             | nil => raise Stuck
                            end
                | CPY r => (A.update(rregs, regtoidx r, !treg); 
                            rundebug (Inst i);
                            birdsattack ())
                | DIS r => (A.update(rregs, regtoidx r,
                                     (!treg) orelse
                                       (A.sub(rregs, regtoidx r)));
                            rundebug (Inst i);
                            birdsattack ())
                | NEG r => (A.update(rregs, regtoidx r, not (!treg));
                            rundebug (Inst i);
                            birdsattack ())
                | VOT r => (A.update(rregs, regtoidx r,
                             ((booltoint (!treg))
                              + (booltoint (A.sub(rregs, regtoidx R1)))
                              + (booltoint (A.sub(rregs, regtoidx R2))))
                             >= 2);
                            rundebug (Inst i);
                            birdsattack ())
                | RES r => (treg := A.sub(rregs, regtoidx r);
                             rundebug (Inst i);
                             birdsattack ())
                | HLT => raise Halt m
              end

          and rundebug t = 
              (case d of
                 SOME d' => d' (m, t)
               | NONE => ())

          and birdsattack () = 
              let val () = if (R.randNat rand) mod BIRD_PERIOD = 0 then
                             let val r = (R.randNat rand) mod NUMBER_OF_REGS
                                 val () = A.update(rregs, r, false);
                             in
                               rundebug (Rand (idxtoreg r))
                             end
                           else ()
              in
                step ()
              end
      in
        step ()
        handle Halt m => (#ostack m)
      end

end
