(* This code is super crappy.  First, you have to run it in SML/NJ because
 * it uses random number stuff.
 *
 * After you load it, you can generate a puzzle by calling
 * bk_rand_machine (width, height, freq, seed1, seed2)
 * width and height are the width and height of the puzzle.
 * When deciding what to put in a particular place, the generator will
 *  place a >< pair 1/freq of the time.
 * seed1 and seed2 are seeds for the random number generator.
 *)


(* SMLNJ *)

fun bk_rand_machine (width,height,freq,seed1,seed2) =
  let
    val rands = Random.rand (seed1,seed2)
    fun rand () = Random.randNat rands

    fun build_row (0,accum) = implode accum
      | build_row (1,accum) = implode (#"|" :: accum)
      | build_row (n,accum) = 
        if 0 = rand () mod freq then
           build_row (n - 2, #">" :: #"<" :: accum)
        else build_row (n - 1, #"|" :: accum)

    fun create_mach (0,accum) = foldr (fn (s1,s2) => s1 ^ "\n" ^ s2) "\n" accum
      | create_mach (n,accum) = 
        create_mach (n - 1, (build_row (width,nil)) :: accum)
  in
    print (create_mach (height,nil))
  end
