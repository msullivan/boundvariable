
structure Rewrite = 
struct

    (* ---------------------------------------------------------------------- *)
    (* syntax *)
    
    structure N :> NAME_ORD = Name

    datatype term = 
	Const of N.name
      | App of term * term
	
    datatype pattern =
	PVar of N.name
      | PConst of N.name
      | PApp of pattern * pattern
	
    type rule = pattern * pattern
    type state = rule list * term
    type bindings = (N.name * term) list
	
    fun equalTerms (Const n, Const n') = N.equal (n, n')
      | equalTerms (App (e1, e2), App (e1', e2')) = 
	equalTerms (e1, e1') andalso equalTerms (e2, e2') 
      | equalTerms _ = false

    fun termToString (Const n) = (N.toString n)
      | termToString (App (e1, e2)) = "(" ^ (termToString e1) ^ " " ^ (termToString e2) ^ ")"

    fun patternToString (PVar n) = ":"^ (N.toString n)
      | patternToString (PConst n) = (N.toString n)
      | patternToString (PApp (p1, p2)) = "(" ^ (patternToString p1) ^ " " ^ (patternToString p2) ^ ")"


    (* ---------------------------------------------------------------------- *)
    (* operational semantics utilities *)

    (* merge two bindings *)
    fun merge ([], b) = SOME b
      | merge (b, []) = SOME b
      | merge (b1 as (n1, t1)::rest1, b2 as (n2,t2)::rest2) = 
	if N.equal(n1, n2) 
	then
	    if equalTerms (t1, t2) 
	    then
		case merge(rest1, rest2) of 
		    NONE => NONE
		  | SOME bRest => SOME ((n1, t1) :: bRest)
	    else
		NONE
	else
	    let val recAndMerge = 
		(fn (rec1, rec2, firstName, firstTerm) => 
		 case merge (rec1, rec2) of
		     NONE => NONE
		   | SOME bRest => SOME ((firstName, firstTerm) :: bRest))
	    in
		if N.lessThan (n1, n2) 
		then recAndMerge (rest1, b2, n1, t1)
		else recAndMerge (b1, rest2, n2, t2)
	    end
	
    (* determine whether a term matches a pattern exactly
       (NOT whether the pattern matches somewhere in a term)
       
       if they don't match, NONE
       if they do, some of bindings to variables *)
    fun match (PVar n, e) = SOME [(n, e)]
      | match (PConst n, Const n') = 
	if N.equal (n, n') then SOME [] else NONE
      | match (PApp (p1, p2), App(e1, e2)) = 
	   (case (match (p1, e1), match (p2, e2)) of
		(NONE, NONE) => NONE
	      | (SOME _, NONE) => NONE
	      | (NONE, SOME _) => NONE
	      | (SOME b1, SOME b2) => merge (b1, b2))
      | match _ = NONE
	
    (* apply a bindings to a pattern, resulting in a term
       
       assumes that every var is bound in the pattern
       *)
    fun subst b (p : pattern) = 
	let val s = subst b 
	in
	    case p of
		PApp (p1, p2) => App (s p1, s p2)
	      | PVar n => 
		    (case (List.find (fn (curName, _) => N.equal (n, curName))) b of
			 NONE => raise Fail "variable not in bindings" 
		       | SOME (_, bindingVal) => bindingVal)
	      | PConst n => Const n
	end
	     
    (* ---------------------------------------------------------------------- *)
    (* operational semantics *)
    
    (* perform the step when the given rule matches the term immediately
       otherwise, call ifnot *)
    fun stepIfMatch (ifnot : rule * term -> term option)
	            (rul as (pattern, result) : rule, e : term)
		    : term option = 
	case match (pattern, e) of
	    SOME bindings => SOME (subst bindings result)
	  | NONE => ifnot (rul, e)
		
    (* presumes that the rule doesn't immediately match 
       goes inside, left to right, 
       and applies the rule at the first match site *)
    fun stepInsideSingle (rul : rule, e : term) : term option = 
	let 
	    (* recursive call needs to check if it's an immediate match,
	       and then come back to this function if not *)
	    val stepInside = stepIfMatch stepInsideSingle 
	in
	    (* go inside e *)
	    (case e of 
		 App (e1, e2) => 
		     (* left to right for now, 
			always takes rule if it matches anywhere *)
		     (case stepInside (rul, e1) of
			  SOME e1' => SOME (App (e1', e2))
			| NONE => 
			      (case stepInside (rul, e2) of
				   SOME e2' => SOME (App (e1, e2'))
				 | NONE => NONE))
	       (* if variables or constants don't match, 
		  there are no subterms to check *)
	       | _ => NONE)
	end
    
    (* presumes that the rule doesn't immediately match 
       
       goes inside and applies the rule to all subterms that the rule matches 

       as usual, returns NONE if there are no matches at all
       *)
    fun stepInsideAll (rul : rule, e : term) : term option = 
	let 
	    (* ifmatch and recur to step inside the term *) 
	    val stepInside = stepIfMatch stepInsideAll 
	in 
	    (case e of 
		 App (e1, e2) => 
		     (case (stepInside (rul, e1), stepInside (rul, e2)) of
			  (NONE, NONE) => NONE
			| (SOME e1', NONE) => SOME (App (e1', e2))
			| (NONE, SOME e2') => SOME (App (e1, e2'))
			| (SOME e1', SOME e2') => SOME (App (e1', e2')))
	       | _ => NONE)
	end
    
    (* counts the number of subterms a rule matches *)
    fun countMatches (r as (pattern, _), e) = 
	case match (pattern, e) of 
	    SOME _ => 1
	  | NONE => 
		(case e of 
		     App (e1, e2) => countMatches (r, e1) + countMatches (r, e2)
		   | _ => 0)

    (* presumes the rule doesn't immediately match 
       
       applies the rule on one side of the application or the other.
       which side is determined by 
       (a) counting the number of matches in each side 
       (b) the side with fewer matches wins, with the left side winning ties

       what to do on the side is an argument
       *)
    fun stepInsideStrange (doToSide : rule * term -> term option) (rul : rule, e : term) : term option = 
	let 
	    (* assumes that stepInsideAll will return SOME 
	       e.g. because matches is nonzero *)
	    fun doStep (applyTo : term,  context : term -> term) = 
		(case doToSide (rul, applyTo) of
		     SOME resultOfStep => SOME (context resultOfStep)
		   | NONE => raise Fail "expected a step to be possible, but stepInsideAll returned none")
	in
	    (case e of 
		 App (e1, e2) =>
		     (* DEBUG *)
		     (*
		     let val _ = print ("Matches: (" ^ (Int.toString (countMatches (rul, e1))) ^ ", " ^ (Int.toString (countMatches (rul, e2))) ^ ")\n")
		     in
		     *)
		     (* DEBUG *)
		     (case (countMatches (rul, e1), countMatches (rul, e2)) of
			  (0,0) => NONE
			| (_ (* nonzero *), 0) => doStep (e1, fn hole => App (hole, e2))
			| (0, _ (* nonzero *)) => doStep (e2, fn hole => App (e1, hole))
			| (matches1, matches2) => 
			      if matches1 <= matches2 
			      then (* apply in 1 only *) 
				  doStep (e1, (fn hole => App (hole, e2)))
			      else (* apply in 2 only *) 
				  doStep (e2, (fn hole => App (e1, hole))))
		     (* end *)
	       | _ => NONE)
	end
	     
    (* for fold, 
       accumulates the first value for which the function returns true *)
    fun lift (f : 'a -> 'b option) : ('a * 'b option -> 'b option) =
	(fn (a, bOpt) =>
	 case bOpt of 
	     SOME x => SOME x
	   | NONE => f a)
	
    (* searches the rules left-to-right
       and returns the result of the first matching rule (if any)
       *)
    fun stepWithFirstMatchingRule 
	(stepSingleRule : rule * term -> term option)
	(ruleList : rule list, e : term)
	: term option = 
	List.foldl (lift (fn r : rule => stepSingleRule (r, e))) NONE ruleList
	
    (* ---------------------------------------------------------------------- *)
    (* tests *)

    (* 
       run until stuck with some stepper 
       
       return the final result and the count of how many steps it took
       *)
    fun stepToStuckGen (step : rule list * term -> term option) (ruleAndTerm : rule list * term) = 
	let 
	    fun loop ((rl, e), count) = 
		case step (rl, e) of 
		    NONE => (e, count)
		  | SOME e' => 
			loop ((rl, e'), count + 1)
	in
	    loop (ruleAndTerm, 0)
	end

	    
    val stepToStuckSingle = stepToStuckGen (stepWithFirstMatchingRule (stepIfMatch stepInsideSingle))
    val stepToStuckAll = stepToStuckGen (stepWithFirstMatchingRule (stepIfMatch stepInsideAll))
    val stepToStuckStrangeSingle = stepToStuckGen (stepWithFirstMatchingRule (stepIfMatch (stepInsideStrange (stepIfMatch stepInsideSingle))))
    val stepToStuckStrangeAll = stepToStuckGen (stepWithFirstMatchingRule (stepIfMatch (stepInsideStrange (stepIfMatch stepInsideAll))))
		
    local 
	val plusName = (N.new "plus")
	val multName = (N.new "mult")
	val factName = (N.new "fact")
	val zName = (N.new "z")
	val sName = (N.new "s")
	val vName = (N.new "v")
	val v1Name = (N.new "v1")
	val v2Name = (N.new "v2")

	val plusP = PConst plusName
	val multP = PConst multName
	val factP = PConst factName
	val zP = PConst zName
	val sP = PConst sName
	val v = PVar vName
	val v1 = PVar v1Name
	val v2 = PVar v2Name

	val plus = Const plusName
	val mult = Const multName
	val fact = Const factName
	val z = Const zName
	val s = Const sName

    in
	val plusRules = 
	    [(PApp (PApp (plusP, zP), v),
	      v),
	     (PApp (PApp (plusP, PApp(sP, v1)), v2),
	      PApp(sP, (PApp (PApp (plusP, v1), v2))))]

	val multRules = 
	    (plusRules @
	     [(PApp (PApp (multP, zP), v),
	       zP),
	      (PApp (PApp (multP, PApp(sP, v1)), v2),
	       PApp (PApp(plusP, v2), (PApp (PApp (multP, v1), v2))))])
	     
	val factRules = 
	    (multRules @ 
	     [(PApp (factP, zP),
	       PApp (sP, zP)),
	      (PApp (factP, PApp(sP, v1)),
	       PApp (PApp(multP, PApp(sP, v1)), (PApp (factP, v1))))])
	    
	fun numeral 0 = z
	  | numeral n = App (s, numeral (n - 1))
	
	fun unnumeral (Const name) =
	    (if N.equal (name, zName) 
	     then 0
	     else raise Fail "not a numeral")
	  | unnumeral (App (Const shouldBeSName, t2)) =
		if N.equal (shouldBeSName, sName)
		then 1 + (unnumeral t2)
		else raise Fail "not a numeral"
	  | unnumeral _ = raise Fail "not a numeral"
		    
 	val plusTest0 = (plusRules, App (App (plus, (numeral 0)), numeral 2))
 	val plusTest1 = (plusRules, App (App (plus, (numeral 1)), numeral 2))
 	val multTest1 = (multRules, App (App (mult, (numeral 1)), numeral 2))
	val factTest1 = (factRules, App (fact, (numeral 6)))
	
    end

    local 
	val yname = N.new "Y"
	val sname = N.new "S"
	val kname = N.new "K"
	val iname = N.new "I"
	val YP = PConst yname
	val Y = Const yname
	val SP = PConst sname
	val S = Const sname
	val KP = PConst kname
	val K = Const kname
	val IP = PConst iname
	val I = Const iname

	val x = PVar (N.new "x")
	val y = PVar (N.new "y")
	val z = PVar (N.new "z")
    in
	val combRules = 
	    [(PApp (IP, x), 
	      x),
	     
	     (PApp (PApp (KP, x), y), 
	      x),
	     
	     (PApp (PApp (PApp (SP, x), y), z), 
	      PApp(PApp(x,z), PApp (y,z))),
	     
	     (PApp (YP, x),
	      PApp (x, PApp (YP, x)))
	     ]

	val combTest0 = (combRules, App (Y, I))
    end
    
end