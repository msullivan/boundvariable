
structure Compile =
struct
  

  val steg = Params.param ""
    (SOME ("-steg",
           "File to embed steganographically")) "stegfile"


  fun compile () out =
    let
      val hidden = if !steg <> ""
                   then StringUtil.readfile (!steg)
                   else ""

      (* return the next bit of the steg data *)
      val bit = ref 0
      fun stegf () = 
        let
          val idx = !bit div 8
          val b = if idx >= size hidden 
                  then 0w0
                  else Word32.fromInt (ord (CharVector.sub(hidden, idx)))
        in
          (if 0w0 = Word32.andb(b, Word32.<<(0w1, Word.fromInt (7 - !bit mod 8)))
           then 0w0 : Word32.word
           else 0w1)
             before       
          bit := !bit + 1
        end

      val code = (UMinUM.code (), nil)
      (* val code = (UMPrint.code (), nil) *)
    in
      Assemble.assemble { for_humlock = false,
                          target = Assemble.TARG_UM,
                          steg = stegf } out code;

      (if !steg <> ""
       then print ("\nUsed " ^ Int.toString (!bit) ^ " bits of steganography\n")
       else ());
      
      if !bit div 8 < size hidden
      then print "\nWARNING: couldn't fit steg message"
      else ();
      
      OS.Process.success
    end handle (e as Assemble.Assemble s) => (print ("assemble: " ^ s ^ "\n"); raise e)
             | (e as UMA.UMA s) => (print ("UMA: " ^ s ^ "\n"); raise e)


end