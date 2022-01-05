

structure MinML =
struct
  datatype exp =
    (* functions are all at top-level *)
(*    Fn of string * string list * exp *)
    Var of string
  | Pair of exp * exp
  | Pi1 of exp
  | Pi2 of exp
  | Split of exp * string * string * exp
  | App of exp * exp list
  | Case of exp * string * exp * exp
  | Inl of exp
  | Inr of exp
  | Unit
  | Let of string * exp * exp


  fun minml_subst src va e =
    let val m = minml_subst src va
    in
      (case e of
         Var s => if s = src
                  then va else e
       | Pair (e1, e2) => Pair (m e1, m e2)
       | Pi1 e => Pi1 (m e)
       | Pi2 e => Pi2 (m e)
       | Split (e, v1, v2, e') =>
                    if src = v1 orelse src = v2
                    then Split (m e, v1, v2, e')
                    else Split (m e, v1, v2, m e')
       | App (e, el) => App (m e, map m el)
       | Case (e, v, e1, e2) => if src = v
                                then Case(m e, v, e1, e2)
                                else Case(m e, v, m e1, m e2)
       | Inl e => Inl (m e)
       | Inr e => Inr (m e)
       | Unit => Unit
       | Let (v, e1, e2) => if src = v
                            then Let (v, m e1, e2)
                            else Let (v, m e1, m e2))
    end

  (* return the set of free vars, as a list *)
  fun freevars e =
    let
      fun add (h : string) l = if List.exists (fn h' => h = h') l 
                               then l else h :: l
      fun union nil l2 = l2
        | union (h :: t) l2 = add h (union t l2)

      fun subtract (v : string) l = List.filter (fn v' => v <> v') l
    in
      (case e of
         Var s => [s]
       | Pair (e1, e2) => union (freevars e1) (freevars e2)
       | Pi1 e => freevars e
       | Pi2 e => freevars e
       | Split (e, v1, v2, e') =>
           union (freevars e) (subtract v1 (subtract v2 (freevars e')))
       | App (e, el) => foldl (Util.uncurry union) (freevars e) (map freevars el)
       | Case (e, v, e1, e2) => union (freevars e) (subtract v (union (freevars e1)
                                                                      (freevars e2)))
       | Inl e => freevars e
       | Inr e => freevars e
       | Unit => []
       | Let (v, e, e') => union (freevars e) (subtract v (freevars e')))
    end
end

