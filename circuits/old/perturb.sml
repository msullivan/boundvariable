structure Perturb =
struct 
  val orig = explode 
 (",..|.......................,\n"
^ ":h | *=================*   :\n"
^ "---#>!send[(W,S),(W,E)]!+  :\n"
^ ":  | *=================*|  :\n"
^ ":  |   | *=============*|  :\n"
^ ":  +---#>!case W of S,E!#-+:\n"
^ ":      | *=============*v |:\n"
^ ":   +--#--+  *===========*|:\n"
^ ":   |  | +-->!send[(N,E)]!#-\n"
^ ":   |  | |   *===========*|:\n"
^ ":   |  | +----------------+:\n"
^ ":   |  +--+ +-----+        :\n"
^ ":   |     | | +---#------+ :\n"
^ ":   v     | | |   v      | :\n"
^ ":*=======*| | | *=====*  | :\n"
^ ":!split N!#-+ +>!use h!--#--\n"
^ ":*=======*v     *=====*  | :\n"
^ ":  | *==================*| :\n"
^ ":  +>!send[(Inl(W,N),E)]!+ :\n"
^ ":    *==================*  :\n"
^ ",..........................,\n")

  val poss = explode ".:,=!*|+><-#[]()send"

  fun perturb seed n =
      let val des_key = DES.key (0wx1234567, 0wx7777777)
          fun random () =
              let 
                val r = DES.encrypt des_key (!seed)
                val () = seed := r
                val r = Word32.toInt(Word32.andb(0wx7FFFFFF, #2 (!seed)))
              in
                (*        print (Int.toString r ^ "\n"); *)
                r
              end
          fun loop 0 s = ()
            | loop n s =
              let val w = random () mod (length s) 
                  val c = List.nth (poss, random () mod (length poss))
                  val s = (List.take (s, w))
                          @
                          [c]
                          @
                          (List.drop (s, w+1))
              in
                print "rm parsee\n/bin/umodem parsee `\n";
                print (implode s);
                print "`\n";
                print "./2d parsee\n";
                loop (n - 1) s
              end
      in 
        loop n orig
      end
        
end

val _ = Perturb.perturb (ref (0wxDEADBEEF, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxD1ADBEEF, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxDE2DBEEF, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxDEA3BEEF, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxDEAD4EEF, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxDEADB4EF, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxDEADBE5F, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxDEADBEE6, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxD8ADBEEF, 0wx00012345)) 10
val _ = Perturb.perturb (ref (0wxDE9DBEEF, 0wx00012345)) 10
