(* The previous language had variables, but they were kind of useless
   since there was no way to introduce bindings. This language
   introduces bindings. *)

(* Start by copying your code from the previous file. *)

(* Problem 1: Introduce a new datatype (after `expr` but before
   everything else) called `binding` to represent variables. For now,
   have `binding` consist of only a single constructor, representing
   `val` bindings (like `val x = foo` in SML). The constructor should
   take a string representing a variable name to bind and an
   expression to bind it to.  *)

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
			     
datatype binding = valB of string * expr
			     
datatype ty = intT | boolT
			  
type dynamic_environment = (string * value) list
type static_environment = (string * ty) list 					    
	            
exception IncorrectType

fun lookup (var_name : string, env) =
  case env of
     (x,y) :: t => if x = var_name then y else lookup (var_name, t)
    | [] => raise Empty

		  
fun eval_expr (ex: expr, dyn_env: dynamic_environment) : value =
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
	| Negate x => integer (~ (val_to_int(eval_expr (x, dyn_env))) )
	| Add (x,y) => integer (val_to_int(eval_expr (x, dyn_env)) + val_to_int(eval_expr (y, dyn_env)))
	| Multiply (x,y) => integer(val_to_int(eval_expr (x, dyn_env)) * val_to_int(eval_expr (y, dyn_env)))
	| bConstant c => boolean c
	| IfThenElse (x,y,z) => if val_to_bool(eval_expr(x, dyn_env))
				then eval_expr(y, dyn_env)
				else eval_expr(z, dyn_env)
	| LessThanEqual (x,y) => boolean (val_to_int(eval_expr(x, dyn_env))
					  <= val_to_int(eval_expr(y, dyn_env)))
	| var (x,y) => (lookup (x, dyn_env))
  end


fun typecheck_expr (ex: expr, stat_env) : ty = 
      case ex of
	  Constant c => intT
			    
	| Negate x => typecheck_expr (x, stat_env)
			       
	| Add (x,y) => if typecheck_expr (x, stat_env) = intT
			  andalso typecheck_expr (y, stat_env) = intT
		       then intT
		       else raise IncorrectType
				  
	| Multiply (x,y) => if typecheck_expr (x, stat_env) = intT
			       andalso typecheck_expr (y, stat_env) = intT
			    then intT
			    else raise IncorrectType
				      
	| bConstant c => boolT
			     
	| IfThenElse (x,y,z) => if typecheck_expr (x, stat_env) = boolT 	 
				then
				    if typecheck_expr (y, stat_env) = typecheck_expr (z, stat_env)
				    then typecheck_expr (y, stat_env)
				    else raise IncorrectType
				else raise IncorrectType
					   
	| LessThanEqual (x,y) => if typecheck_expr (x, stat_env) = intT
				    andalso typecheck_expr (y, stat_env) = intT
				 then boolT
				 else raise IncorrectType
					    
	| var (x,y) =>  lookup(x, stat_env) 
      
(* Problem 2: Write a function to evaluate bindings.

   Start by renaming your `eval` function to `eval_expr`.

   Then define a new function, `eval_binding` which takes the current
   dynamic environment and a binding, and returns a *new* dynamic
   environment. `eval_binding` should evaluate the binding in an
   analogous way to the way SML bindings are processed: first evaluate
   the expression in the current dynamic environment (using
   `eval_expr`), and then *extend* the environment with a new binding
   for the variable name to the resulting value. (You need to figure
   out exactly what "extend" means here.)
 *)
      
fun eval_binding (dyn_env : dynamic_environment, b : binding) : dynamic_environment =
  let
      val valB (x,y) = b
  in
      (x, eval_expr(y, dyn_env)) :: dyn_env
  end
      
(* Problem 3: Write a function to typecheck bindings.

   Start by renaming `typecheck` to `typecheck_expr`.

   Then write a function `typecheck_binding` that takes the current
   static environment and a binding and returns a new static
   environment. `typecheck_binding` should typecheck the binding in an
   analogous way to how SML bindings are typechecked: first typecheck
   the expression in the current static environment, and then *extend*
   the static environment with a new binding for the variable to the
   resulting type. (You need to figure out exactly what "extend" means
   here.)
 *)
      
fun typecheck_binding (stat_env: static_environment, b : binding) : static_environment =
  let
      val valB (x,y) = b
  in
      (x, typecheck_expr(y, stat_env)) :: stat_env
  end
      
(* Problem 4: Write a few tests for evaluating and typechecking
   bindings. *)
val [("test", integer 7)] = eval_binding([], valB ("test", Add(Constant 4, Constant 3)))
val [("test2", intT)] = typecheck_binding([], valB ("test2", Multiply(Constant 5, Constant 3)))
val [("test3",boolT),("test2",intT)] =
    typecheck_binding([("test2", intT)], valB ("test3", LessThanEqual(Constant 2, Constant 2)))
		     
(* Problem 5: Implement (the core of) a REPL for our expression
   language, by writing a function `repl` that takes a program
   (represented as a list of bindings) and processes it, analogous to
   how SML processes programs, starting from the empty dynamic/static
   environment, and returns a pair of the "final" dynamic/static
   environments.

   Be careful to typecheck things before evaluating them!

   Hint: define a helper function that takes the current
   dynamic/static environments as additional arguments.
*)
fun repl (bl : binding list) =
  let
      fun stat_append (bl : binding list, env : static_environment) =
	case bl of
	    [] => env
	  | x :: t  => stat_append (t, typecheck_binding(env, x))

      fun dyn_append (bl : binding list, env : dynamic_environment) =
	case bl of
	    [] => env
	  | x :: t  => dyn_append (t, eval_binding(env, x))
  in
      (stat_append (bl, []), dyn_append (bl, []))
  end
      
	
(* Problem 6: write a few test "programs" (lists of bindings) for
   your REPL, and bind them to SML variables of your choice here. *)
val program1 = [valB ("test3", LessThanEqual(Constant 2, Constant 2)), valB ("test2", Multiply(Constant 5, Constant 3))]

val ([("test2",intT),("test3",boolT)],
     [("test2",integer 15),("test3",boolean true)]) = repl(program1)		   
		   
val program2 = [valB ("test3", Multiply(Add (Constant 2, Constant 2), Constant 3)),
		valB ("test2", IfThenElse (LessThanEqual (Constant 2, Constant 3),
					   Constant 2, Constant 3))]

val ([("test2",intT),("test3",intT)],[("test2",integer 2),("test3",integer 12)]) = repl(program2)
	  		   
val program3 =  [valB ("test5", IfThenElse (bConstant true, bConstant true, bConstant false)),
		 valB ("test99", Multiply(Constant 5, Constant 3)),
		 valB ("RanOutOfTestNames", LessThanEqual(Add (Constant 3, Constant 4), Multiply(Constant 3, Constant 1)))]

val ([("RanOutOfTestNames",boolT),("test99",intT),("test5",boolT)],
     [("RanOutOfTestNames",boolean false),("test99",integer 15),
      ("test5",boolean true)]) = repl(program3)
