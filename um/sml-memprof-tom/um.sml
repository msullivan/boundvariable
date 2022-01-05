
(* This SML version attempts to be as fast as possible. *)

structure FastUM =
struct

  structure W = Word32
  structure A = Array
  structure AS = ArraySlice

  val debug = ref false
  val verbose = ref false
  fun vdo f = ()


  structure GA = GrowArray

  exception UM of string

  fun printe s = TextIO.output(TextIO.stdErr, s)

  val ALLOC_START = 1000

  (* invt: mem[0] always has something in it *)
  type mach = { mem : (W.word A.array) GA.growarray,
                (* indices in mem that have been freed *)
                freelist : int list ref,
                (* length 8 *)
                regs : (W.word A.array),
                (* instruction pointer *)
                ip : W.word ref }

  fun triple w =
    (W.toInt (W.andb(W.>>(w, 0w6), 0w7)),
     W.toInt (W.andb(W.>>(w, 0w3), 0w7)),
     W.toInt (W.andb(w, 0w7)))

  fun double w =
    (W.toInt (W.andb(W.>>(w, 0w3), 0w7)),
     W.toInt (W.andb(w, 0w7)))

  fun single w = W.toInt (W.andb(w, 0w7))

  (* give array of instructions *)
  fun newmach code =
    let
      val mem = GA.empty ()
    in
      GA.append mem code;
      (* start allocations at a higher number, 
         so that they are not confused with other
         interesting things. *)
      GA.update mem ALLOC_START (A.fromList [0wxDEADBEEF]);

      { mem = mem,
        freelist = ref nil,
        ip = ref 0w0,
        regs = A.fromList [0w0, 0w0, 0w0, 0w0,
                           0w0, 0w0, 0w0, 0w0] } : mach
    end

  fun copy_array a =
    A.tabulate(A.length a, fn x => A.sub(a, x))

  exception Halt

  val totmem = ref (0 : IntInf.int)
  val maxmem = ref (0 : IntInf.int)
  val memuse = ref (0 : IntInf.int)

  (* fetch the current instruction from memory *)
  fun fetchinst mem ip =
      (A.sub (GA.sub mem 0, W.toInt (! ip)))

  fun runinst (mach as { mem, freelist, ip, regs }) w =
    let
      fun arr a = GA.sub mem (W.toInt a)

      fun reg x = A.sub(regs, x)
      fun setreg x v = A.update(regs, x, v)
    in
      (case W.>>(w, 0w28) of
         0w0 =>
           let 
             val (dest, src, test) = triple w
           in
             if reg test <> 0w0
             then setreg dest (reg src)
             else ()
           end

       | 0w1 =>
           let val (dst, a, off) = triple w
           in
            setreg dst (A.sub(arr (reg a), W.toInt (reg off)))
           end

       | 0w2 =>
           let val (a, off, src) = triple w
           in
            A.update(arr (reg a), W.toInt (reg off), reg src)
           end

       | 0w3 =>
           let val (dst, s1, s2) = triple w
           in
             setreg dst (W.+ (reg s1, reg s2))
           end

       | 0w4 =>
           let val (dst, s1, s2) = triple w
           in
             setreg dst (W.* (reg s1, reg s2))
           end

       | 0w5 => 
           let val (dst, s1, s2) = triple w
           in
             setreg dst (W.div (reg s1, reg s2))
           end

       | 0w6 => 
           let val (dst, s1, s2) = triple w
           in
             setreg dst (W.notb (W.andb (reg s1, reg s2)))
           end

       | 0w7 => raise Halt

       | 0w8 => 
           let
             val (dst, sz) = double w
             val () = memuse := !memuse + (W.toLargeInt (reg sz))
             val () = totmem := !totmem + (W.toLargeInt (reg sz))
             val () = if !memuse > !maxmem
                      then maxmem := !memuse
                      else ()
             val a = A.array(W.toInt (reg sz), 0w0)
             val next = 
               (case !freelist of
                  nil => GA.length mem
                | h :: t => 
                    let in
                      freelist := t;
                      h
                    end)
           in
             GA.update mem next a;
             setreg dst (W.fromInt next)
           end

       | 0w9 => 

           (* PERF should actually remove it *)
           let 
             val r = single w
              val freeme = Word32.toInt (reg r)
           in
             memuse := !memuse - (IntInf.fromInt (A.length (GA.sub mem freeme)));
             freelist := freeme :: !freelist
           end

       | 0w10 => 
           let 
             val r = single w
           in
             TextIO.output1 (TextIO.stdOut, 
                             chr (W.toInt (W.andb (0w255, reg r))));
             TextIO.flushOut TextIO.stdOut
           end

       | 0w11 =>
           let
             val r = single w
           in
             case TextIO.input1 TextIO.stdIn of
               NONE => setreg r 0wxFFFFFFFF
             | SOME c => setreg r (W.fromInt (ord c))
           end

       | 0w12 =>
           let
             val (a, id) = double w
           in
             case reg a of
               (* special case 0, which is very common *)
               0w0 => ip := (reg id) - 0w1
             | _ => 
                 let
                   val newcode = copy_array (arr (reg a))
                 in
                   
                   GA.update mem 0 newcode;
                   (* one less, because it will increment *)
                   ip := (reg id) - 0w1
                 end
           end

         (* really just 13, but other values are "undefined" *)
        | _ =>
           let
             val (r, w) =(W.toInt (W.andb(0w7, W.>>(w, 0w25))),
                          W.andb (w, 0wx2000000 - 0w1))
           in
             setreg r w
           end);

      (* always increment the instruction pointer *)
      ip := !ip + 0w1

    end

  fun runprog a =
    let 
      val mach = newmach a
    in
      while true do
        let
          val i = (A.sub (GA.sub (#mem mach) 0,
                          W.toInt (! (#ip mach))))
        in
          runinst mach i
        end
    end handle Halt => 
       (let in
          printe "\n\n== Halted. ==\n";
          printe ("Maximum live memory: " ^
                  IntInf.toString (!maxmem * 4) ^ " bytes\n");
          printe ("Total bytes allocated: " ^
                  IntInf.toString (!totmem * 4) ^ " bytes\n")
        end)

  fun runfromfile f =
    let
      val f = BinIO.openIn f

      val prog = GA.empty ()

      fun e w = Word32.fromInt (Word8.toInt w)
      fun go () =
        case (BinIO.input1 f,
              BinIO.input1 f,
              BinIO.input1 f,
              BinIO.input1 f) of
          (SOME a, SOME b, SOME c, SOME d) => 
          (GA.append prog (W.orb(W.<<(e a, 0w24),
                                 W.orb(W.<<(e b, 0w16),
                                       W.orb(W.<<(e c, 0w8),
                                             e d))));
           go ())
        | (NONE, NONE, NONE, NONE) => ()
        | _ => raise UM "file is not multiple of 4 bytes"
    in
      go ();
      BinIO.closeIn f;
      runprog (GA.finalize prog)
    end handle UM s => printe ("\n\n== Error: " ^ s ^ " ==\n")

end

val _ = 
  case Params.docommandline () of
    [f] => FastUM.runfromfile f
  | _ => 
      let in
        FastUM.printe "universal machine x\nusage:\num file\n\n";
        print (Params.usage ())
      end
  
