(* generate moderately tight problems for the
   adventure phase I that require search. *)

structure GenUltra =
struct

  val adjectives = ["amber", "amethyst", "aquamarine", "azure", "beige", "bisque", "black", "blanched-almond-colored", "blaze-orange", "blue", "blue-violet", "bondi-blue", "brass", "bright-green", "bright-turquoise", "bright-violet", "bronze", "brown", "buff", "burgundy", "burlywood-colored", "burnt-orange", "burnt-sienna", "burnt-umber", "cadet-blue", "camouflage-green", "cardinal", "carmine", "carrot", "celadon", "cerise", "cerulean-blue", "chartreuse", "chestnut", "chocolate-colored", "cinnamon", "cobalt", "copper", "coral", "corn", "cornflower-blue", "cornsilk", "cream", "crimson", "cyan", "deep-pink", "deep-sky-blue", "denim", "dim-gray", "dodger-blue", "eggplant", "emerald", "fern-green", "firebrick", "flax", "floral-white", "forest-green", "fuchsia", "gamboge", "ghost-white", "gold", "goldenrod", "gray10", "gray20", "gray30", "gray40", "gray50", "gray60", "gray70", "gray80", "gray90", "gray-tea-green", "green", "green-yellow", "grey", "heliotrope", "honeydew", "hot-pink", "indigo", "ivory", "jade", "khaki", "lavender", "lavender-blush", "lawn-green", "lemon", "lemon-chiffon", "lemon-cream", "light-brown", "lilac", "lime", "lime-green", "linen-colored", "magenta", "malachite", "maroon", "mauve", "midnight-blue", "mint-cream", "mint-green", "misty-rose", "moccasin", "moss-green", "mountbatten-pink", "mustard", "navajo-white", "navy-blue", "ochre", "old-gold", "old-lace", "olive", "olive-drab", "olive-green", "orange", "orange-red", "orchid", "pale-blue", "pale-brown", "pale-carmine", "pale-chestnut", "pale-cornflower-blue", "pale-goldenrod", "pale-green", "pale-magenta", "pale-mauve", "pale-pink", "pale-red-violet", "pale-sandy-brown", "pale-turquoise", "pale-violet-red", "papaya-whip", "pastel-green", "pastel-pink", "peach", "peach-orange", "peach-puff", "peach-yellow", "pear", "periwinkle", "persian-blue", "pine-green", "pink", "pink-orange", "plum", "powder-blue", "prussian-blue", "puce", "pumpkin", "purple", "raw-umber", "red", "red-violet", "robin-egg-blue", "rosy-brown", "royal-blue", "russet", "rust", "saddle-brown", "safety-orange", "saffron", "salmon-colored", "sandy-brown", "sangria", "sapphire", "scarlet", "school-bus-yellow", "sea-green", "seashell", "selective-yellow", "sepia", "sienna", "silver", "sky-blue", "slate-blue", "slate-gray", "snow", "spring-green", "steel-blue", "swamp-green", "tan", "tangerine", "taupe", "tea-green", "teal", "terra-cotta", "thistle-colored", "turquoise", "ultramarine", "vermilion", "violet", "violet-eggplant", "violet-red", "viridian", "wheat-colored", "white", "wisteria", "yellow", "yellow-green", "rotating", "reciprocating", "old", "shiny", "gross", "tepid", "sturdy", "exceptional", "imported", "domestic", "floating", "heavy", "organic", "vegan", "waterproof", "low-carb", "mysterious", "hypoallergenic", "discounted", "peppered", "water-logged", "imitation", "miniature", "gargantuan", "imaginary", "foreign", "reverse-chirality", "left-handed", "momemtum-preserving"]


  open Solver 

  exception GenUltra of string

  structure Map = SplayMapFn (type ord_key = string val compare = String.compare)

  datatype gencontext =
    GC of { last_item_number : int,
            atom_map : string list Map.map,
            prop_list : Proof.prop list }

  val initial = GC { last_item_number = 0,
                     atom_map = Map.empty,
                     prop_list = nil }

  val missing_percent_chance = 60
  val missing_discount = 6
  val max_missing = 12
  val max_discount = 3
  val lambda_percent_chance = 35
  val duplicate_percent_chance = 85
  val reuse_prop = 90


  (* for random *)
  val des_key = DES.key (0wx1234567, 0wx7777777)

  val seed = ref (case map Int.fromString (CommandLine.arguments ()) of
                    [SOME w] => (print ("Using seed " ^ Int.toString w ^ "...\n"); (0w7, Word32.fromInt w))
                  | _ => (0w7 : Word32.word, 0wx1234: Word32.word))
 

  fun random () =
    let 
      val r = DES.encrypt des_key (!seed)
      val () = seed := r
      val r = Word32.toInt(Word32.andb(0wx7FFFFFF, #2 (!seed)))
    in
      (*        print (Int.toString r ^ "\n"); *)
      r
    end


fun generate (GC{ last_item_number, atom_map, prop_list}) maxinv ((pile, inv, res) : Proof.sequent) =
  let
    val last_item_number = ref last_item_number
    val atom_map = ref atom_map
    val prop_list = ref prop_list
    local
  in
    fun shufflelist l =
        ListUtil.sort (fn _ =>
                          if random () mod 2 = 0
                          then LESS else GREATER) l

    fun genhead (h :: t) = (h, t)
      | genhead _ = raise GenUltra ("oops, out of items in genhead")

    fun new_atom () =
        let val r = last_item_number 
            val () = r := !r + 1;
            val atom = #1 (Print.partNo (!r))
            val () = case Map.find (!atom_map, atom) of
                        NONE => ()
                      | SOME _ => raise GenUltra ("oops, not a new_atom: " ^ atom)

            val (adjective, adjectives) = genhead (shufflelist (shufflelist adjectives))
            val () = atom_map := (Map.insert (!atom_map, atom, adjectives))
        in
          (* XX adjectives must be sorted! *)
          (atom, adjective::nil)
        end

    fun randitem l =
      let
        val n = random () mod length l
      in
        (List.nth (l, n),
         List.take (l, n) @
         List.drop (l, n + 1))
      end

    (* bias towards low numbers, black magic *)
    fun randitem_bias l =
      let
        val ll = length l
        val nums = List.tabulate (ll, fn _ => random () mod ll)
        val n = foldl Int.min (ll - 1) nums
      in
        (List.nth (l, n),
         List.take (l, n) @
         List.drop (l, n + 1))
      end


    (* always generates a prop with head "atom" *)
    fun new_prop possibly_missing max_missing possibly_duplicate atom =
        if (random () mod 100) < possibly_missing then
          Missing(new_prop (possibly_missing div missing_discount) (max_missing div max_discount) possibly_duplicate atom,
                  let val n = (random () mod max_missing) + 1
                      fun gen_atom () = if Map.numItems (!atom_map) > 1
                                     andalso (random () mod 100) < possibly_duplicate then
                                    let val atoms = List.map #1 (Map.listItemsi (!atom_map))
                                        (* XX should we filter out the current atom? *)
                                        val (_, atoms) = List.partition (fn a => atom = a) atoms
                                    in
                                      #1 (randitem atoms)
                                    end
                                  else #1 (new_atom ())
                  in
                    List.tabulate(n, fn _ => new_prop (possibly_missing div missing_discount) 
                                                      (max_missing div max_discount)
                                                      possibly_duplicate
                                                      (gen_atom ()))
                  end)
        else
          Noun atom

    fun new_item possibly_missing max_missing possibly_duplicate =
        let fun new_adjs atom = 
                let in
                  case Map.find (!atom_map, atom) 
                   of SOME adsleft =>
                      let 
                          val (adjective, adjectives) = genhead adsleft
                          val () = atom_map := Map.insert (!atom_map, atom, adjectives)
                      in 
                        (* XX adjectives must be sorted! *)
                        adjective::nil
                      end
                    | NONE => raise GenUltra ("unknown atom " ^ atom)
                end
            fun random_atom () = 
                let val atomsandadjectives = Map.listItemsi (!atom_map)
                    val ((atom, adsleft), _) = randitem atomsandadjectives
                    val (adjective, adjectives) = genhead adsleft
                in
                  atom
                end
        in
          if (random () mod 100) < reuse_prop andalso (length (!prop_list) > 0)
          then 
            let val p = List.nth(!prop_list, random () mod length (!prop_list))
                val atom = propatom p
                val adjs = new_adjs atom
            in
              (p, adjs)
            end
          else
            if Map.numItems (!atom_map) > 0
               andalso (random () mod 100) < possibly_duplicate then
              let val atom = random_atom ()         
                  val adjs = new_adjs atom
                  val p = new_prop possibly_missing max_missing possibly_duplicate atom
                  val () = prop_list := p :: !prop_list
                  val item = (p, adjs)
              in
                item
              end
            else
              let val (atom, adjectives) = new_atom ()
                  val p = new_prop possibly_missing max_missing possibly_duplicate atom
                  val () = prop_list := p :: !prop_list                          
              in
                (p, adjectives)
              end
        end
    end

  (* basically just "verify" in reverse, but picks randomly
     what it will do. *)

  datatype retract_action = BREAK | PHOENIXIZE | DROP
  fun retract (pile, inv) =
    let
      val invn = length inv
      val actions = (if invn < maxinv andalso invn > 0 then [BREAK, BREAK, BREAK] else [])
                    @ (if invn < maxinv then [PHOENIXIZE, PHOENIXIZE] else [])
                    @ (if invn > 0 then [DROP] else [])

    in
      case #1 (randitem actions) of
        BREAK =>
          let
            val ((origprop, adjs), inv) = randitem_bias inv

            val (fixer as (fixerprop, _)) = new_item (missing_percent_chance)
                                                   (max_missing) 
                                                   (duplicate_percent_chance)
            val newprop =
              (* randomly break it. *)
              case origprop of
                Noun _ => Missing(origprop, [fixerprop])
              | Missing (p, l) =>
                  if (random () mod 100) < (lambda_percent_chance) then
                    Missing(origprop, [fixerprop])
                  else
                    Missing(p,
                            shufflelist (fixerprop :: l))
          in
            (Combine((newprop, adjs), fixer), (pile, (newprop, adjs) :: fixer :: inv))
          end
      | PHOENIXIZE => 
          let
            val item = new_item (missing_percent_chance) (max_missing) (duplicate_percent_chance)
          in
            (Incinerate item, (pile, item :: inv))
          end
      | DROP =>
          let
            val (item, inv) = randitem_bias inv
          in
            (Pickup item, (item :: pile, inv))
          end
    end


  val (step, (pile, inv)) = retract (pile, inv)
  val gc = GC{last_item_number = ! last_item_number,
              atom_map = !atom_map,
              prop_list = ! prop_list }

  in
    (step, gc, (pile, inv, res))
  end handle Match => (print "whoa match\n"; raise Match)
end
