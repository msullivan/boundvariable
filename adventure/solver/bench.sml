
structure Bench =
struct
(*
  val _ = Control.Print.printDepth := 10000
  val _ = Control.Print.printLength := 10000
*)
  val INVSIZE = 6
(*
  val (pf, (goal, pile)) = Generator.generate 0 (ref 0wxDEADBEEF) 25 INVSIZE (nil, (nil, [(Solver.Noun "EPROMBurner", nil)], Solver.Noun "EPROMBurner"));
*)

  (* hand-written test *)
  local
      open Proof
 
      val mypart = Print.printPart 3

      val A = Noun (mypart(1))
      val B = Noun (mypart(2))
      val C = Noun (mypart(3))
      val D = Noun (mypart(4))
      val E = Noun (mypart(5))
      val F = Noun (mypart(6))
      val G = Noun (mypart(7))
      val H = Noun (mypart(8))
      val I = Noun (mypart(9))
      val J = Noun (mypart(10))
      val K = Noun (mypart(11))
      val L = Noun (mypart(12))
      val M = Noun (mypart(13))
      val N = Noun (mypart(14))
      val O = Noun (mypart(15))
      val P = Noun (mypart(16))
      val Q = Noun (mypart(17))
      val R = Noun (mypart(18))
      val S = Noun (mypart(19))
      val T = Noun (mypart(20))
      val U = Noun (mypart(21))
      val V = Noun (mypart(22))
      val W = Noun (mypart(23))
      val X = Noun (mypart(24))
      val Y = Noun (mypart(25))
      val Z = Noun (mypart(26))


      fun ==>(l, p) = Missing(p, l)
(*      val ==> = Missing *)
      infixr ==>
  in
      val (goal, pile) =
          (Z,
           ListPair.zip
           ([

             (* actual proof *)
	     [A,B,C,D,E,F,G,H,I,J,K,L,M,N,Q,R,S,T] ==> O,

	     (* here, V-Y are useless. N should never show *)
             (* USELESS *)
	     [V] ==> Y,
	     [N] ==> Z,
             [[V] ==> Y] ==> Y,
             [Y] ==> Y,
             [[W,W] ==> Y] ==> [W] ==> Y,
             [Y] ==> X,

	     W, Y, W,  (* USELESS *)

	     A, B, C, D, 
	     [W,W,W,W,W,W,W,W,W,W,W] ==> Y, (* USELESS *)
             E, F, G,
             [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,Q,R,S,T,U] ==> P,
	     A, B, C, D, E, F, G,
	     [[B, C, D, E, F, H, I, J, K, L, N, M,Q,R,S,T] ==> O] ==> P,
	     [P] ==> Z,
	     [O] ==> Z,
	     H, I, J, K, L, M, Q, R, S, T
           ],
            map (fn x => [x]) Generator.adjectives))

  end

  fun bench () =
    let
      val then_ = Time.now ()
      val (p, steps) = Solver.provepuzzle NONE (goal, pile, INVSIZE)
                                handle Match => (print "solvematch\n"; raise Match)
    in
      case Proof.verify INVSIZE (pile, nil, goal) p
                                handle Match => (print "verifymatch\n"; raise Match)

      of Proof.Success => print "YES\n"
      | Proof.Failure s => print ("NO : " ^ s ^ "\n");
(*
        ("------------- goal  -------------------------------------------------------------------------",
         goal, 

         "------------- pile  -------------------------------------------------------------------------",
         pile, 

         "------------- proof -------------------------------------------------------------------------",
         p)
*)
        print ("it took: " ^ Time.toString (Time.- (Time.now (), then_)) ^ "\n")

    end
        handle Match => (print "benchmatch\n"; raise Match)
             | Solver.Solver s => (print ("SOLVER: " ^ s ^ "\n"); raise Match)
end

(*
val _ = print "self-check:\n"
val _ = Bench.bench () *)
