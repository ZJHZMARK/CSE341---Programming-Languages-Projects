(* Let's start with something barely worthy of calling a programming
   language: arithmetic expressions. *)

(* Problem 1: Define a datatype `expr` to represent arithmetic
   expressions, consisting (for now) of *only* integer constants and
   addition.

   Hint: We've done things like this several times before. *)

datatype expr =
         Constant of int
       | Negate of expr
       | Add of expr * expr
       | Multiply of expr * expr
				
(* Problem 2: Define a function `eval` that takes an `expr` and
   evaluates it to an integer. In other words, `eval` should have
   type expr -> int. *)

fun eval (ex: expr) : int =
  case ex of
      Constant c => c
    | Negate x => ~(eval x)
    | Add (x,y) => (eval x) + (eval y)
    | Multiply (x,y) => (eval x) * (eval y) 

				
(* Problem 3: Write a simple test for `eval` by calling it on an
   `expr` of your choosing. Record your test here as a `val` binding
   named `test`. *)
				       
val test = eval(Add (Constant 4, Constant 3));
