structure Print =
struct

  open Proof

  fun partNo n = ("Part(" ^ (Int.toString n) ^ ")", "Huh" ^ Int.toString n)

  (* XX has collisions? *)
  fun printPart offset n =    
       let val n = offset + n
           val s1 = String.implode 
                            ((Char.chr (((4 * n + 3) mod 26) + 65)) ::
                             nil)
               val s2 = String.implode
                            ((Char.chr (((n * n) mod 10) + 48)) ::
                             (Char.chr (((6 * n - 10) mod 10) + 48)) ::
                             (Char.chr (((n * n * n + 1) mod 10) + 48)) ::
                             (Char.chr (((3 * n * n) mod 10) + 48)) ::
                             nil)
               val s3 = String.implode 
                            ((Char.chr (((5 * n + 7) mod 26) + 65)) ::
                             (Char.chr (((3 * n + 1) mod 25) + 65)) ::
                             (Char.chr (((7 * n + 11) mod 24) + 67)) ::
                             nil)
       in
          s1 ^ "-" ^ s2 ^ "-" ^ s3
       end

  fun nounToString (Noun s) = s
    | nounToString (Missing (cod, _)) = nounToString cod

  fun itemToString (np, adjs) = (List.foldl (fn (x, s) => (String.map Char.toLower x) ^ " " ^ s)
                                            "" adjs)
                                ^ (nounToString np)

  fun printPile loc pile =
      let fun proptostring (Noun s) = "Noun " ^ s
            | proptostring (Missing (cod, dom)) = 
              let in 
                "Missing ("
                ^ (proptostring cod)
                ^ ", "
                ^ (List.foldr (fn (i, s) => "(" ^ (proptostring i) ^ ")::" ^ s)
                              "nil"
                              dom)
                ^ ")"
              end
          fun p ((prop, adjectives)::items) =
              let in
                print ("do add_item_ex(" ^ loc ^ ", ");
                print (proptostring prop);
                print (", true, Exemplary, "
                       ^ (List.foldr (fn (a, s) => a ^ "::" ^ s) "nil" adjectives) 
                       ^ ")\n");
                p items
              end
            | p nil = ()
      in 
        p (List.rev pile)
      end

  fun printProof nil = ()
    | printProof ((Pickup i)::pf) = 
      let in 
        print ("get " ^ (itemToString i) ^ "\n");
        printProof pf
      end
    | printProof ((Incinerate i)::pf) = 
      let in 
        print ("inc " ^ (itemToString i) ^ "\n");
        printProof pf
      end
    | printProof ((Combine (i1, i2))::pf) =
      let in
        print ("comb " ^ (itemToString i1) ^ " " ^ (itemToString i2) ^ "\n");
        printProof pf
      end
                         
  fun printNouns n max = 
      if n > max then ()
      else let val s = printPart 0 n
           in 
             print "---\n";
             print (s ^ "\n");
             print (s ^ "\n");
             printNouns (n + 1) max
           end

end
