(* As you probably guessed, we can rule out "bad expressions" (those
   that would cause the evaluator to throw exceptions) by introducing
   a typechecker. *)

(* Problem 1: Explain why your `bad_expr` at the bottom of the
   previous file would not pass a typechecker. (Just use your
   intuitive understanding of how typecheckers (eg, SML's) work, no
   need to get very technical.) *)
(*
Answer: bad_expr wouldn't pass a typechecker because it's technically a val binding 
associated with an exception. Since we didn't pass an appropriate value type into one
of the helper functions, it returned an exception and was unable to calculate an acceptable
answer for the eval function
*)

(* Start by copying over all your code from the previous file. *)
datatype expr =
         Constant of int
       | Negate of expr
       | Add of expr * expr
       | Multiply of expr * expr
       | bConstant of bool
       | IfThenElse of expr * expr * expr
       | LessThanEqual of expr * expr

datatype value =
	 integer of int
	 | boolean of bool



exception IncorrectType
fun eval (ex: expr) : value =
  let
      fun val_to_int (x : value) : int =
	case x of
	    integer y => y
	  | boolean y  => raise IncorrectType
				
      fun val_to_bool (x : value) : bool =
	case x of
	    boolean y => y
	  | integer y => raise IncorrectType			       
  in      
      case ex of
	  Constant c =>  integer c
	| Negate x => integer (~ (val_to_int(eval x)))
	| Add (x,y) => integer (val_to_int(eval x) + val_to_int(eval y))
	| Multiply (x,y) => integer(val_to_int(eval x) * val_to_int(eval y))
	| bConstant c => boolean c
	| IfThenElse (x,y,z) => if val_to_bool(eval(x)) then eval(y) else eval(z)
	| LessThanEqual (x,y) => boolean (val_to_int(eval(x)) <= val_to_int(eval(y)))
  end
      
(* Now it's time to implement a typechecker for expressions. *)

(* Problem 2: Introduce a new datatype called `ty`, to represent the
   possible types for expressions. (We can't call it `type` because
   that's a keyword in SML. But it will also help us not get confused
   between SML types and the types of our little expression language.)

   `ty` should have one constructor for each possible type in the
   expression language. The constructors should take no additional
   data; they only serve to "name" types. *)
datatype ty = intT | boolT
		       
(* Problem 3: Implement a typechecker as a function `typecheck` of
   type `expr -> ty`.

   Just like the evaluator, the typechecker will need to consider each
   kind of expression separately, and implement the typechecking rule
   for that kind of expression. Recall a few relevant examples from
   when we learned the typechecking rules for SML:
     - integer constants have type int
     - to typecheck `e1 + e2`, typecheck e1 and e2 and ensure that
       they are ints; then the result is also int.
     - to typecheck `if e1 then e2 else e3`, first typecheck e1 and
       ensure that it is a bool; then typecheck e2 and e3 and ensure
       that they have the same type as each other.

   You will need to implement rules like these for each kind of
   expression in the language.

   Your typechecker should raise an exception if the given expression
   does not typecheck. For example, adding a boolean to an int should
   cause it to raise an exception. (You can introduce a new exception
   or just use `Fail` with an appropriate message.)

   There are several ways to implement the "checks" of the
   typechecker. You could directly use SML's if-then-else or pattern
   matching to see whether the type is as expected, throwing an
   exception if not. Or you could use helper functions (somewhat
   analogous to the "casting" functions used in the evaluator) that
   "assert" that the type is a particular one, and throw an exception
   otherwise. It's up to you which strategy you chose.
  *)

exception IncorrectType

fun typechecker (ex: expr) : ty = 
      case ex of
	  Constant c => intT
			    
	| Negate x => typechecker x
			       
	| Add (x,y) => if typechecker x = intT andalso typechecker y = intT
		       then intT
		       else raise IncorrectType
				  
	| Multiply (x,y) => if typechecker x = intT andalso typechecker y = intT
			    then intT
			    else raise IncorrectType
				      
	| bConstant c => boolT
			     
	| IfThenElse (x,y,z) => if typechecker x = boolT 	 
				then
				    if typechecker y = typechecker z
				    then typechecker y
				    else raise IncorrectType
				else raise IncorrectType
					   
	| LessThanEqual (x,y) => if typechecker x = intT andalso typechecker y = intT
				 then boolT
				 else raise IncorrectType


(* Problem 4: Write enough tests that you typecheck every kind of
   expression at least once. Bind your tests to variable names of your
   choosing here.

   Make sure your typechecker throws an exception when given your
   `bad_expr` from the previous file.

   Make sure your tyechecker still throws an exception even if
   `bad_expr` is nested inside some larger program.

   Convince yourself that no expression that passes the typechecker
   can ever cause the evaluator to throw an exception. *)
val intT = typechecker (Negate (Add (Constant 4, Constant 5)))
val intT =  typechecker (Negate (Constant 4))
val intT =  typechecker (Negate (Add (Constant 4, Constant 5)))
val boolT = typechecker (IfThenElse (bConstant true, bConstant true, bConstant false))
val intT =  typechecker (IfThenElse (bConstant true, Constant 4, Constant 3))
val boolT =  typechecker (LessThanEqual (Constant 4, Constant 3))
val intT = typechecker (Add (Constant 4, Constant 3))
val intT = typechecker (Add (Add (Constant 4, Constant 3), Constant 3))
val intT = typechecker (Multiply (Add (Constant 4, Constant 3), Constant 3)) 
val bad_expr = IfThenElse (Constant 3, Constant 4, Constant 3)
			  
