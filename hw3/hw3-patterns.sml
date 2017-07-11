(* CSE 341 Winter 2017 hw3 *)

exception NoAnswer

datatype pattern = WildcardP
		 | VariableP of string
		 | UnitP
		 | ConstantP of int
		 | ConstructorP of string * pattern
		 | TupleP of pattern list

datatype valu = Constant of int
	      | Unit
	      | Constructor of string * valu
	      | Tuple of valu list

(**** for the challenge problem only ****)

datatype typ = AnythingT
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | DatatypeT of string

(**** you can put all your code here ****)

(* Problem 1 *)
fun first_answer (f : ('a -> 'b option), l : ('a list)) : 'b = 
  case l of
      [] => raise NoAnswer
    | h::t => case f(h) of
		  NONE => first_answer(f, t)
		| SOME v => v 
(* Problem 2 *)				
fun all_answers (f : ('a -> 'b list option), l : ('a list)) : 'b list option =
  let
      fun helper (acc) =
	case l of
	    [] => SOME(acc)
	  | h::t => (case f(h) of
		      NONE => NONE
		    | SOME lst => helper(acc@lst))
  in
     helper([])
  end

(* Problem 3 *)
fun check_pat (p : pattern) : bool =
  let
      fun get_string (p : pattern) =
	case p of
	    TupleP (x::y) => (case x of
				  VariableP (s) => s :: get_string(TupleP (y))
				| _ => get_string(TupleP (y)) )
	  | _ => []
		     
      fun unique (s : string list) =
	case s of
	    x::t => if
		       List.exists (fn z => x=z) t
		   then
		       false
		   else
		       unique(t)
	  | _ => true
  in
      unique(get_string(p))
  end
      
(* Problem 4 *)
fun match (v : valu, p : pattern) : (string * valu) list option  =
  case p of
      ConstantP x => if Constant x = v then SOME[] else NONE
    | VariableP s => SOME[(s,v)]
    | UnitP => if v = Unit then SOME[] else NONE
    | ConstructorP (s1,p1) => (case v of
				   Constructor (s2,v1) => if s1=s2 then match(v1,p1) else NONE
				 | _ => NONE)	    
    | WildcardP => SOME[]
    | TupleP ps => (case v of
			Tuple vs => all_answers(match, ListPair.zip(vs,ps))
		      | _ => NONE)		 

(* Problem 5 *)
fun first_match (v : valu, p : pattern list) : (string * valu) list option =
  SOME( first_answer(match, map (fn x => (v,x)) p ) )
  handle NoAnswer => NONE
	      
  
      
  
      
								
								
      
