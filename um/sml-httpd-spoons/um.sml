
(* This SML version attempts to be as fast as possible.
   Also it is an interface to the web server embedded within
   umix and can't be used any other way. *)

structure FastUM =
struct

  structure W = Word32
  structure A = Array
  structure AS = ArraySlice

  structure GA = GrowArray

  structure S = Socket

  exception UM of string

  (* could be param *)
  val port = 19106
  exception EOF

  fun printe s = TextIO.output(TextIO.stdErr, s)

  val ALLOC_START = 1000

  val sock = INetSock.TCP.socket()
  val _ = S.Ctl.setREUSEADDR (sock, true)
  val _ = S.bind (sock, INetSock.any port)
  val _ = S.listen (sock, 999)

  datatype state =
     Init of char list (* initializing um; string to pass to read *)
   | Request of (INetSock.inet, S.active S.stream) S.sock * int 
       (* forwarding http request; conn and last char sent *)
   | Headers of (INetSock.inet, S.active S.stream) S.sock * char list * int option
       (* parsing/forwarding http headers; conn, last line received, size of response *)
   | Response of (INetSock.inet, S.active S.stream) S.sock * int
       (* forwarding http response; conn, remaining size of response *)

  fun read_one conn = 
      let val vec = S.recvVec (conn, 1) in
        Word8.toInt (Word8Vector.sub (vec, 0))
      end 

  fun write_one conn c = 
      let val vec = Word8Vector.fromList [Word8.fromInt c] 
          val slice = Word8VectorSlice.full vec
      in
        S.sendVec (conn, slice)
      end

  (* invt: mem[0] always has something in it *)
  type mach = { mem : (W.word A.array) GA.growarray,
                (* indices in mem that have been freed *)
                freelist : int list ref,
                (* length 8 *)
                regs : (W.word A.array),
                (* instruction pointer *)
                ip : W.word ref,
                state : state ref }

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
        state = ref (Init (explode ("guest\ntelnet localhost 80\n"))),
        regs = A.fromList [0w0, 0w0, 0w0, 0w0,
                           0w0, 0w0, 0w0, 0w0] } : mach
    end

  fun copy_array a =
    A.tabulate(A.length a, fn x => A.sub(a, x))

  exception Halt


  (* fetch the current instruction from memory *)
  fun fetchinst mem ip =
      (A.sub (GA.sub mem 0, W.toInt (! ip)))

  fun runinst (mach as { mem, freelist, ip, state, regs }) w =
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
             freelist := freeme :: !freelist
           end

       | 0w10 => 
           let 
             val r = single w
             val c = chr (W.toInt (W.andb (0w255, reg r)))
             fun printc c = (TextIO.output1 (TextIO.stdOut, c);
                             TextIO.flushOut TextIO.stdOut)
           in
             case !state of
               Headers (conn, cs, len) =>
               let val _ = printc c
               in
                 (
                 write_one conn (ord c);
                 (if ord c = 10 then 
                    (* End of line -- see what header it was *)
                    case (cs, len) of 
                      (nil, SOME len) => state := Response (conn, len)
                    | (nil, NONE) => (printe "missing length!?\n";
                                      S.close conn;
                                      state := Init (explode "telnet localhost 80\n"))
                    | (cs, _) => let val s = implode (rev cs) in
                                   case StringUtil.sfields " " s of
                                     "Content-Length:" :: l :: nil => 
                                       state := Headers (conn, nil, Int.fromString l)
                                     | _ => state := Headers (conn, nil, len)
                                 end
                  else
                    state := Headers (conn, c::cs, len))
                     ) handle _ => (print "CONNECTION LOST #1\n";
                                    state := Init (explode "telnet localhost 80\n"))
               end
             | Response (conn, 0) => (S.close conn handle _ => (); printc c;
                                      state := Init (explode "telnet localhost 80\n"))
             | Response (conn, n) => ((write_one conn (ord c); 
                                       (* don't print content (e.g. images) to stdout *)
                                       state := Response (conn, n - 1))
                                      handle _ => (print "CONNECTION LOST #2\n";
                                                   state := Init (explode "telnet localhost 80\n")))
             | _ => printc c
           end

       | 0w11 =>
           let
             val r = single w

             fun read_or_accept () =
                 (case !state of
                     Init (c::cs) => 
                         let in
                             setreg r (W.fromInt (ord c));
                             state := (Init cs)
                         end
                   | Init nil =>
                         let val (conn, addr) = S.accept sock
                         in
                             state := Request(conn, ord #"?");
                             read_or_accept ()
                         end
                   | Request (conn, c') => 
                         let val c = read_one conn
                          in
                              setreg r (W.fromInt c);
                              (if c = 0 then
                                   state := Init (explode "telnet localhost 80\n")
                               else if c = 13 then
                                   state := Request (conn, c')
                               else if c = 10 andalso c' = 10 then
                                   state := Headers (conn, nil, NONE)
                               else 
                                   state := Request (conn, c))
                          end) handle _ =>
                              let in
                                  (* on error, leak connection and retry;
                                     XXX could very well be bogus;
                                     maybe should send some reset sequence
                                     to UMIX or something *)
                                  print "CONNECTION LOST #3\n";
                                  state := Init nil;
                                  read_or_accept ()
                              end
           in
               read_or_accept ()
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
    end handle Halt => printe "\n\n== Halted. ==\n"



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
  
