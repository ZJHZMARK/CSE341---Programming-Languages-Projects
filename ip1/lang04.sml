(* So far, our language has been completely "constant", in the sense
   that everything boiled down to either integer or boolean constants.
   This file takes one step towards writing more interesting programs
   by adding variables to the language. *)

(* Start by copying your code from the previous language here. *)

(* Problem 1: add a new constructor to `expr` to represent
   variables. (Represent variable names as strings.) *)
datatype value =
	 integer of int
	 | boolean of bool
			  
datatype expr =
         Constant of int
       | Negate of expr
       | Add of expr * expr
       | Multiply of expr * expr
       | bConstant of bool
       | IfThenElse of expr * expr * expr
       | LessThanEqual of expr * expr
       | var of string * value

datatype ty = intT | boolT
			  
type dynamic_environment = (string * value) list
type static_environment = (string * ty) list 					    
	            
exception IncorrectType

(* Problem 2: Write a function `lookup` that takes a dynamic
   environment and a variable name, and looks up that variable's value in
   the dynamic environment.

   If the variable is not in the environment, raise SML's builtin
   `Empty` exception.
*)
fun lookup (var_name : string, env) =
  case env of
     (x,y) :: t => if x = var_name then y else lookup (var_name, t)
    | [] => raise Empty
		  
(* Problem 3: Implement variables in the evaluator.

   First, add a new argument to the evaluator for the dynamic
   environment. Change all the existing recursive calls to pass the
   dynamic environment through.

   Next, add a new case to handle variables, which just uses `lookup`
   to look up the variable's value in the dynamic environment. *)
fun eval (ex: expr, dyn_env: dynamic_environment) : value =
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
	| Negate x => integer (~ (val_to_int(eval (x, dyn_env))) )
	| Add (x,y) => integer (val_to_int(eval (x, dyn_env)) + val_to_int(eval (y, dyn_env)))
	| Multiply (x,y) => integer(val_to_int(eval (x, dyn_env)) * val_to_int(eval (y, dyn_env)))
	| bConstant c => boolean c
	| IfThenElse (x,y,z) => if val_to_bool(eval(x, dyn_env))
				then eval(y, dyn_env)
				else eval(z, dyn_env)
	| LessThanEqual (x,y) => boolean (val_to_int(eval(x, dyn_env))
					  <= val_to_int(eval(y, dyn_env)))
	| var (x,y) => (lookup (x, dyn_env))
  end
      

(* Problem 4: Implement variables in the typechecker.

   First, add a new argument to the typechecker for the static
   environment. Change all the existing recursive calls to pass the
   static environment through.

   Next, add a new case to handle variables, which just uses `lookup`
   to look up the variable's type in the static environment.

   If your `lookup` function won't work, think about why. Then try
   deleting some type annotations and seeing whether that helps. *)

fun typechecker (ex: expr, stat_env) : ty = 
      case ex of
	  Constant c => intT
			    
	| Negate x => typechecker (x, stat_env)
			       
	| Add (x,y) => if typechecker (x, stat_env) = intT
			  andalso typechecker (y, stat_env) = intT
		       then intT
		       else raise IncorrectType
				  
	| Multiply (x,y) => if typechecker (x, stat_env) = intT
			       andalso typechecker (y, stat_env) = intT
			    then intT
			    else raise IncorrectType
				      
	| bConstant c => boolT
			     
	| IfThenElse (x,y,z) => if typechecker (x, stat_env) = boolT 	 
				then
				    if typechecker (y, stat_env) = typechecker (z, stat_env)
				    then typechecker (y, stat_env)
				    else raise IncorrectType
				else raise IncorrectType
					   
	| LessThanEqual (x,y) => if typechecker (x, stat_env) = intT
				    andalso typechecker (y, stat_env) = intT
				 then boolT
				 else raise IncorrectType
					    
	| var (x,y) =>  lookup(x, stat_env) 

(* Problem 5: Write a few tests for evaluating and typechecking
   variables and bind them here to SML variables of your choice. Since
   our expression language does not yet have any way of actually
   binding variables, you will have to manually construct the
   dynamic/static environments in order to run your tests. *)


val my_stat_env = [("test_variable", boolT), ("test2", intT), ("test3", intT)]
val my_dyn_env = [("test1", boolean false), ("test2", integer 1), ("test3", boolean true)]
val boolT = lookup ("test_variable", my_stat_env);
val integer 1 = lookup ("test2", my_dyn_env)
val intT =  typechecker(var ("test2", integer 4), my_stat_env)
val boolean False = eval(var ("test1", boolean false), my_dyn_env)
val intT = typechecker (Negate (Add (Constant 4, Constant 5)), my_stat_env)
val boolT = typechecker (IfThenElse (bConstant true, bConstant true, bConstant false), my_stat_env)
val intT =  typechecker (IfThenElse (bConstant true, Constant 4, Constant 3), my_stat_env)
val boolT =  typechecker (LessThanEqual (Constant 4, Constant 3), my_stat_env)
val intT = typechecker (Add (Constant 4, Constant 3), my_stat_env)
val intT = typechecker (Add (Add (Constant 4, Constant 3), Constant 3), my_stat_env)
val intT = typechecker (Multiply (Add (Constant 4, Constant 3), Constant 3), my_stat_env) 


