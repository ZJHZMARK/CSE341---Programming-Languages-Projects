(* We've seen in lecture and on previous assignments how to extend our
   `expr` "language" with more integer operations. But what about
   other types? In this file, we'll add booleans to our language. *)

				
(* Problem 1: Modify `expr` to add these new programming constructs:
     - boolean constants (true and false)
     - boolean and
     - if-then-else expressions
     - the less-than-or-equal-to operation
 *)
datatype expr =
         Constant of int
       | Negate of expr
       | Add of expr * expr
       | Multiply of expr * expr
       | bConstant of bool
       | IfThenElse of expr * expr * expr
       | LessThanEqual of expr * expr

(* Now we'd like to extend our evaluator to the new language.

   Start by copying over your `eval` function from the previous
   language. *)

(* Problem 2: Try to extend `eval` to the new language, starting first
   with the boolean constants. What problem do you run into with its
   return type? *)
(* 
Answer:  We can't use the old eval function because it's designed to return an integer
and our new expr datatype now includes booleans so this wouldn't be appropriate
*)

(* Problem 3: Introduce a new datatype, called `value` which will
   represent the possible return values for `eval`. Think carefully
   about how many constructors the datatype will need.

   Hint: How many "types" of values are there in our language so far? *)
			      
datatype value =
	 integer of int
	 | boolean of bool

(* Problem 4: Finish the new definition of `eval`.

   First change `eval` to return a `value` instead. In each branch of
   `eval`, use a constructor of the `value` type to "package up" your
   answer.

   In the previous language, we "knew" what kind of value recursive
   calls to `eval` would return (it would always be an int). Now, we
   don't necessarily know, since it could be built with any of the
   constructors for the `value` type. However, each of the operations
   in our language expects values of a certain type, so we will need to
   check that the values were built using the correct constructor.

   Write helper functions, one per constructor of `value`, that "cast"
   a value to that type. For example, one of your constructors to
   `value` should be for integer values, so one of your helper
   functions should be of type `value -> int`. In these cast
   functions, throw an exception if the value was not built with the
   correct constructor. (You can introduce a new exception or just use
   `Fail` with an appropriate message.)

   Now complete the definition of `eval`. You should implement the
   intuitive meaning of each new program construct. For example, the
   "and" operation on booleans should recursively evaluate its
   subexpressions, use the "casting" helper functions convert the
   results to booleans and then return a boolean `value` representing
   their "and".

   For less-than-or-equal-to, assume that only integers are compared.

   For if-then-else expressions, do not assume anything in particular
   about the types of the "then" and "else" branches.
 
 *)

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
      
(* Problem 5: Write enough tests that you use each new programming
   construct at least once, and record them here as bindings (to
   whatever variable names you want).

   Also, find an expression that causes your evaluator to throw an
   exception, and bind it to the name `bad_expr`. Contemplate how one
   might "rule out" expressions like this one. *)

val integer 3 = eval (Constant 3)
val integer ~2 = eval (Negate (Constant 2))		      
val integer 10 = eval (Add (Constant 7, Constant 3))
val integer 64 = eval (Multiply (Constant 8, Constant 8))
val boolean true = eval (bConstant true)
val integer 4 = eval (IfThenElse (bConstant true, Constant 4, Constant 3))
val boolean false = eval (LessThanEqual (Constant 4, Constant 3))
val integer ~6 = eval (Negate (Add (Constant 3, Constant 3)))
val integer 13 = eval (Add (Multiply (Constant 3, Constant 3), Multiply(Constant 2, Constant 2)))
val integer 5 =  eval (IfThenElse (LessThanEqual (Constant 4, Constant 3), Constant 3, Add(Constant 2, Constant 3)))

val boolean true =  eval (LessThanEqual (Constant 3, Constant 3))
val boolean true =   eval (LessThanEqual (Add (Constant 4, Constant 3), Multiply (Constant 4, Constant 3)))
val bad_expr = IfThenElse (Constant 3, Constant 4, Constant 3)
val boolean true =  eval (bad_expr) handle IncorrectType => boolean true
