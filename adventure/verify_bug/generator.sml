(* generate moderately tight problems for the
   adventure phase I that require search. *)

structure Generator =
struct

  open Solver 


(*
  val example_seq = ([Missing([Noun "car", Noun "pen"], Noun "journalist"),
                      Noun("junk"),
                      Noun("pen"),
                      Noun("car")],
                     [],
                     Noun "journalist")
    
  val example_pf =
    Pickup (Missing([Noun "pen", Noun "car"], Noun "journalist")) ::
    Pickup (Noun "junk") ::
    Pickup (Noun "pen") ::
    Incinerate (Noun "junk") ::
    Pickup (Noun "car") ::
    Combine (Missing([Noun "pen", Noun "car"], Noun "journalist"), Noun "car") ::
    Combine (Missing([Noun "pen"], Noun "journalist"), Noun "pen") :: nil
*)
  val missing_percent_chance = 40
  val max_missing = 4
  val lambda_percent_chance = 30
  val duplicate_percent_chance = 95
  val reuse_prop = 90

  structure Map = SplayMapFn (type ord_key = string val compare = String.compare)

fun generate lin seed x =
  let
    val last_item_number = ref lin
    local
    val adjectives = ["Amber", "Amethyst", "Aquamarine", "Azure", "Beige", "Bisque", "Black", "BlanchedAlmondColored", "BlazeOrange", "Blue", "BlueViolet", "BondiBlue", "Brass", "BrightGreen", "BrightTurquoise", "BrightViolet", "Bronze", "Brown", "Buff", "Burgundy", "BurlywoodColored", "BurntOrange", "BurntSienna", "BurntUmber", "CadetBlue", "CamouflageGreen", "Cardinal", "Carmine", "Carrot", "Celadon", "Cerise", "CeruleanBlue", "Chartreuse", "Chestnut", "ChocolateColored", "Cinnamon", "Cobalt", "Copper", "Coral", "Corn", "CornflowerBlue", "Cornsilk", "Cream", "Crimson", "Cyan", "DeepPink", "DeepSkyBlue", "Denim", "DimGray", "DodgerBlue", "Eggplant", "Emerald", "FernGreen", "Firebrick", "Flax", "FloralWhite", "ForestGreen", "Fuchsia", "Gamboge", "GhostWhite", "Gold", "Goldenrod", "Gray10", "Gray20", "Gray30", "Gray40", "Gray50", "Gray60", "Gray70", "Gray80", "Gray90", "GrayTeaGreen", "Green", "GreenYellow", "Grey", "Heliotrope", "Honeydew", "HotPink", "Indigo", "Ivory", "Jade", "Khaki", "Lavender", "LavenderBlush", "LawnGreen", "Lemon", "LemonChiffon", "LemonCream", "LightBrown", "Lilac", "Lime", "LimeGreen", "LinenColored", "Magenta", "Malachite", "Maroon", "Mauve", "MidnightBlue", "MintCream", "MintGreen", "MistyRose", "Moccasin", "MossGreen", "MountbattenPink", "Mustard", "NavajoWhite", "NavyBlue", "Ochre", "OldGold", "OldLace", "Olive", "OliveDrab", "OliveGreen", "Orange", "OrangeRed", "Orchid", "PaleBlue", "PaleBrown", "PaleCarmine", "PaleChestnut", "PaleCornflowerBlue", "PaleGoldenrod", "PaleGreen", "PaleMagenta", "PaleMauve", "PalePink", "PaleRedViolet", "PaleSandyBrown", "PaleTurquoise", "PaleVioletRed", "PapayaWhip", "PastelGreen", "PastelPink", "Peach", "PeachOrange", "PeachPuff", "PeachYellow", "Pear", "Periwinkle", "PersianBlue", "PineGreen", "Pink", "PinkOrange", "Plum", "PowderBlue", "PrussianBlue", "Puce", "Pumpkin", "Purple", "RawUmber", "Red", "RedViolet", "RobinEggBlue", "RosyBrown", "RoyalBlue", "Russet", "Rust", "SaddleBrown", "SafetyOrange", "Saffron", "SalmonColored", "SandyBrown", "Sangria", "Sapphire", "Scarlet", "SchoolBusYellow", "SeaGreen", "Seashell", "SelectiveYellow", "Sepia", "Sienna", "Silver", "SkyBlue", "SlateBlue", "SlateGray", "Snow", "SpringGreen", "SteelBlue", "SwampGreen", "Tan", "Tangerine", "Taupe", "TeaGreen", "Teal", "TerraCotta", "ThistleColored", "Turquoise", "Ultramarine", "Vermilion", "Violet", "VioletEggplant", "VioletRed", "Viridian", "WheatColored", "White", "Wisteria", "Yellow", "YellowGreen", "Rotating", "Reciprocating", "Old", "Shiny", "Gross", "Tepid", "Sturdy", "Exceptional", "Imported", "Domestic", "Floating", "Heavy", "Organic", "Vegan", "Waterproof", "LowCarb", "Mysterious", "Hypoallergenic", "Discounted", "Peppered", "WaterLogged", "Imitation", "Miniature", "Gargantuan", "Imaginary", "Foreign", "ReverseChirality", "LeftHanded", "MomentumPreserving"]
  in
    val atom_map = ref Map.empty : string list Map.map ref

    val prop_list = ref nil : Proof.prop list ref

    fun random () =
      let 
        val () = seed := !seed * 0wx31337;
        val () = seed := !seed + 0w99999;
        val r = Word32.toInt(Word32.andb(0wx7FFFFFF, !seed))
      in
(*        print (Int.toString r ^ "\n"); *)
        r
      end

    fun shufflelist l =
        ListUtil.sort (fn _ =>
                          if random () mod 2 = 0
                          then LESS else GREATER) l

    fun new_atom () =
        let val r = last_item_number 
            val () = r := !r + 1;
            val atom = #1 (Print.partNo (!r))
            val (adjective::adjectives) = shufflelist (shufflelist adjectives)
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

    fun new_prop possibly_missing max_missing possibly_duplicate atom =
      if (random () mod 100) < reuse_prop andalso (length (!prop_list) > 0)
      then List.nth(!prop_list, random () mod length (!prop_list)) 
      else
        let
          val p =
        if (random () mod 100) < possibly_missing then
          Missing(new_prop (possibly_missing div 2) max_missing possibly_duplicate atom,
                  let val n = (random () mod max_missing) + 1
                      val atom' = if Map.numItems (!atom_map) > 1
                                     andalso (random () mod 100) < possibly_duplicate then
                                    let val atoms = List.map #1 (Map.listItemsi (!atom_map))
                                        (* XX should we filter out the current atom? *)
                                        val (_, atoms) = List.partition (fn a => atom = a) atoms
                                    in
                                      #1 (randitem atoms)
                                    end
                                  else #1 (new_atom ())
                  in
                    List.tabulate(n, fn _ => new_prop (possibly_missing div 4) 
                                                      max_missing
                                                      possibly_duplicate
                                                      atom')
                  end)
        else
          Noun atom
        in
          prop_list := p :: !prop_list;
          p
        end

    fun new_item possibly_missing max_missing possibly_duplicate =
        if Map.numItems (!atom_map) > 0
           andalso (random () mod 100) < possibly_duplicate then
          let val atomsandadjectives = Map.listItemsi (!atom_map)
              val ((atom, adjective::adjectives), _) = randitem atomsandadjectives
              val () = atom_map := Map.insert (!atom_map, atom, adjectives)
          in
            (* XX adjectives must be sorted! *)
            (new_prop possibly_missing max_missing possibly_duplicate atom, adjective::nil)            
          end
        else
          let val (atom, adjectives) = new_atom ()
          in
            (new_prop possibly_missing max_missing possibly_duplicate atom, adjectives)
          end
  end
  

  (* basically just "verify" in reverse, but picks randomly
     what it will do. *)

  datatype retract_action = BREAK | PHOENIXIZE | DROP
  fun retract invsize (pile, inv, res) =
    let
      val invn = length inv
      val actions = (if invn < invsize andalso invn > 0 then [BREAK, BREAK, BREAK, BREAK] else [])
                    @ (if invn < invsize then [PHOENIXIZE, PHOENIXIZE] else [])
                    @ (if invn > 0 then [DROP] else [])

    in
      case #1 (randitem actions) of
        BREAK =>
          let
            val ((origprop, adjs), inv) = randitem inv

            val fixer as (fixerprop, _) = new_item (missing_percent_chance)
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
            (Combine((newprop, adjs), fixer), (pile, (newprop, adjs) :: fixer :: inv, res))
          end
      | PHOENIXIZE => 
          let
            val item = new_item (missing_percent_chance) (max_missing) (duplicate_percent_chance)
          in
            (Incinerate item, (pile, item :: inv, res))
          end
      | DROP =>
          let
            val (item, inv) = randitem inv
          in
            (Pickup item, (item :: pile, inv, res))
          end
    end

  fun genloop 0 invsize (proof, seq) = (proof, seq)
    | genloop n invsize (proof, seq) = 
    let
      val (p, seq') = retract invsize seq
    in
      genloop (n - 1) invsize (p :: proof, seq')
    end

  fun generate n invsize (proof, seq) =
      let val (proof, seq) = genloop n invsize (proof, seq)
          fun cleanup (proof, (pile, nil, res)) = (proof, (res, pile))
            | cleanup (proof, (pile, p :: l, res)) =
              cleanup ((Pickup p)::proof, (p :: pile, l, res))
      in 
        cleanup (proof, seq)
      end
  in
    generate x
  end handle Match => (print "whoa match\n"; raise Match)
end
