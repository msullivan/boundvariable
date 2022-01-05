
structure Proof =
struct

  (* HACK_ATTACK! *)
  exception OUTATIME
  exception Solver of string

  exception Verify of string


  type atom = string
  type adjective = string

  datatype prop = 
    Noun of atom
  | Missing of prop * prop list

  type item = prop * adjective list
(*
  fun ptos (Noun s) = s 
    | ptos (Missing (p, pl)) =
    "(" ^ ptos p ^ (case pl of nil => "" | _ => " without ") ^
    (StringUtil.delimit " and " (map ptos pl)) ^ ")"
*)
  fun ptos (Noun s) = s 
    | ptos (Missing (p, pl)) = "([" ^ StringUtil.delimit ", " (map ptos pl) ^ "] => " ^ ptos p ^ ")"

  fun itos (p, al) = "(" ^ (StringUtil.delimit " and " al) ^ " " ^ ptos p ^ ")"

  fun propatom (Noun a) = a
    | propatom (Missing (p, _)) = propatom p

  datatype 'item step =
    (* apply f to a single item *)
    Combine of 'item * 'item
  | Pickup of 'item
  | Incinerate of 'item

  fun step_map f (Combine (a, b)) = Combine (f a, f b)
    | step_map f (Pickup a) = Pickup (f a)
    | step_map f (Incinerate a) = Incinerate (f a)

  type proofstep = item step

                   (* pile   inventory   res *)
  type sequent = item list * item list * prop

  fun propcmp (Noun a, Noun b) = String.compare (a, b)
    | propcmp (Missing (a, al), Missing (b, bl)) =
    let
      val al = ListUtil.sort propcmp al
      val bl = ListUtil.sort propcmp bl

      val proplcmp = Util.lex_list_order propcmp 
    in
      Util.lex_order proplcmp propcmp ((al, a), (bl, b))
    end
    | propcmp (Noun _, Missing _) = LESS
    | propcmp (Missing _, Noun _) = GREATER

  fun propeq r = propcmp r = EQUAL
  fun adjseq (a, b : adjective list) = ListUtil.all2 (op =) a b
  fun itemeq ((p1, a1), (p2, a2)) = propeq (p1, p2) andalso adjseq (a1, a2)

  datatype verifyresult =
    Success | Failure of string

  (* check that a proof validates a sequent *)
  fun verify invsize (pile, inv, res) nil = 
    if List.exists (fn (r', _) => propeq (res, r')) inv
    then Success
    else Failure "don't have goal"
    | verify invsize ((p, adjs) :: pile, inv, res) (Pickup (prop, adjs') :: proof) =
    if propeq(prop, p) andalso adjseq (adjs, adjs') then
       if length inv < invsize then
          verify invsize (pile, (p, adjs) :: inv, res) proof
       else Failure "no room to pick up"
    else Failure "only pickup top"
    | verify invsize (pile, inv, res) (Incinerate item :: proof) =
        (case List.partition (fn item' => itemeq (item, item')) inv of
           (nil, _) => Failure "can't incinerate it"
         | (_::nil, nomatch) => verify invsize (pile, nomatch, res) proof
         | (l, _) => Failure ("bug/incinerate:didn't expect two equal items : "
                              ^ StringUtil.delimit ", " (map itos l) ^
                              "\n  ---- with inventory:\n" ^
                                StringUtil.delimit ",\n" (map itos inv) ^
                              "\n  ---- and pile:\n" ^ StringUtil.delimit " ::\n" 
                                (map itos pile)))
    | verify invsize (pile, inv, res) (Combine (f as (Missing (fcod, fdom), adjs), a as (p, _)) :: proof) =
        (case List.partition (fn r => itemeq (f, r)) inv of
           (nil, _) => Failure ("don't have " ^ itos f)
         | (_::nil, nomatch) =>
             (case List.partition (fn r => itemeq (a, r)) nomatch of
                (nil, _) => Failure "don't have arg"
              | (_::nil, nomatch) =>
                  let val new_inv = nomatch
                  in
                    (case List.partition (fn r => propeq(p, r)) fdom of
                       (nil, _) => Failure "wasn't missing that"
                     | (_:: others, nomatch) =>
                         let val new_prop = (case nomatch @ others of
                                               nil => fcod
                                             | l => Missing(fcod, l))
                         in
                           verify invsize (pile, (new_prop, adjs) :: new_inv, res) proof
                         end)
                  end
              | _ => Failure "not unique 2")
         | _ => Failure "not unique 3")
    | verify invsize _ _ = Failure "wtf"

end
