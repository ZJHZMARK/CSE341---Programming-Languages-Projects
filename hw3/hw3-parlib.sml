(* CSE 341 Winter 2017 hw3 *)

(* Please complete the TODOs in this file. Delete the TODOs as you go. *)

(* TODO: Your name here *)
(* TEJVEER RAI *)

(* IMPORTANT NOTE: This file contains tests, which currently fail
   because the solutions are not implemented yet. In order to work on
   the problems in this file and to get things to load in the REPL,
   you should comment out all the parts that you haven't solved yet. *)

(* I love parsing. There, I admitted it. This problem is ostensibly
   about designing a parsing library in SML. But it's really more
   about practicing with higher-order functions and closures than
   parsing.

   This problem does not assume any parsing background. Nor does it
   assume that you even looked at the challenge problem from hw2,
   which just happened to also be about parsing. Did I mention I like
   parsing?

   Parsing is the problem of converting a string representation of
   some data into the data itself. For example, Int.fromString is a
   (simple) parser. More complex parsers are used in compilers to
   convert source code into a suitable internal representation. For
   example, the SML implementation starts by parsing your source code.
   
   Our goal in this problem is to design an SML library for writing
   parsers. We won't write very many actual parsers, but instead
   implement some building blocks and some "combinators" to put the
   blocks together into a whole.
   
   To keep things concrete, consider our favorite expression datatype
   from lecture. *)

datatype expr =
         Constant of int
       | Negate of expr
       | Add of expr * expr
       | Multiply of expr * expr

(* Arithmetic expressions like this are usually written using infix
   syntax, as in the following examples:
       3
       1 + 7
       -8 * (1 + 2)
   In order to make the problem easier, we will actually require all
   expressions to be fully parenthesized. So instead of
       1 + 7
   we will actually require the user to write
       (1 + 7)
   (This restriction is not essential, but significantly simplifies the
   example. It is a challenge problem to remove this restriction.)
   
   Roughly speaking, we want to define a function of type
       string -> expr
   but several complications arise. First, for purposes of beauty, we will
   program over char lists instead of strings. Second, in the process of
   of parsing an expression, we will need to parse its subexpressions,
   which will not "use up" the whole input. So instead, we will define a
   function of type
       char list -> expr * char list
   which takes a list of characters and parses an expr off the front of
   it, returning the remainder of the list.
   
   For example, one subproblem of parsing expressions is to parse
   integers. Luckily, the built-in function Int.fromString does most of
   the work for us. Again, though, we need to be ready for the case where
   the integer does not use up the entire input. So instead of a function
   of type
       char list -> int         (or string -> int)
   we really want a function that parses a number off the front of the
   input, returning the rest of it.
   
   More generally, we will use the term
       'a parser
   for any type of the form
       char list -> 'a * char list
 *)

type 'a parser = char list -> 'a * char list

(* How should we think about something of type 'a parser? It's the kind
   of thing that takes a list of characters as input, inspects the
   beginning (several, perhaps) elements of the list and converts them
   into an 'a, returning the constructed 'a and the rest of the list of
   characters.
   
   So an int parser is a thing that consumes characters representing an
   int off the front of the char list and returns the int and the
   remainder of the list. Similarly an expr parser is a thing that
   consumes characters representing an expr off the front of the list and
   returns the expr and the remainder of the list.
   
   A main goal of this assignment is to deepen our understanding of
   higher-order functional idioms by writing so-called "parser
   combinators", which are functions that combine *parsers
   themselves*. For example, we might write some kind of sequencing
   operation on parsers:
       fun sequence (p1 : 'a parser, p2 : 'b parser) : ('a * 'b) parser = (* ... *)
   which takes two parsers, p1 which parses things of type 'a, and p2
   which parses thing of type 'b, and returns a parser which parses pairs
   of 'a and 'b by first parsing an 'a and then parsing a 'b, and then
   returning the pair of results. In fact, we will write exactly such an
   operation below.
   
   We will use parsing arithmetic expressions as a running example, but
   the library functions we define would be generally useful for parsing
   all kinds of things. *)

(* Here is a simple function that we can use later to report errors. For
   example, if we are parsing fully parenthesized arithmetic expressions,
   and the user forgets a parenthesis, we might call this function. *)

fun parse_error msg = raise Fail ("Parse error: " ^ msg)

(* One of the most basic parsers is one that consumes a single character
   of input. We will call this parser `char`, to reflect the fact that it
   parses a char. In general, we will name parsers by the thing they
   parse. (This introduces no shadowing or anything, since type names
   are completely separate from variable names.)
   
   `char` takes a single character as an argument and returns a parser
   that will consume that character off the beginning of the input, or
   throw an error if the input does not begin with that character. From
   this description, we might intuitively expect that `char` have type
       char -> char parser
   or, expanding type abbreviations (and using unnecessary parens)
       char -> (char list -> char * char list)
   Then for any character c, `char c` is a parser that consumes c if it
   is the first element of the list, and fails (ie, raises some
   exception) otherwise.
   
   But notice that the "final" return value of `char` returns the
   character that was parsed. This is a bit pointless, so instead, we
   will return unit. So the real type will be
       char -> unit parser
   or
       char -> (char list -> unit * char list)
   since presumably the caller "knows" which char it passed in, and
   doesn't need to be reminded. *)

(* Problem 1: Implement char, which takes a character c and returns a
   `unit parser`. If the input begins with c, return (), otherwise call
   parse_error with an appropriate message.

   Hint: remember that, expanding type abbreviations, a `unit parser`
         is a function (in this case, of type
             char list -> unit * char list).
         So to return a `unit parser` is to return a function. In
         other words, use currying and/or lambda.
*)


fun char (c : char) : unit parser =
  fn x => case x of
	      h::t => if h = c
		      then
			  ((),t)
		      else
			  parse_error "Parsing Failure"
	    | _ => parse_error "Parse Failure"
			 


(* For example, the parser that consumes a single open parenthesis can
   now be defined in terms of char as
       char #"("
   
   In the following test, notice that the open parenthesis is consumed,
   but the close parenthesis is returned for later consumption.
   
   (String.explode is a standard library function that converts a string
   into a char list.) *)

val ((),[#")"]) = char #"(" (String.explode "()")

(* Let's now turn to the problem of parsing integers, which we'll
   certainly need to do as a part of parsing expressions. As mentioned
   above, our goal is to detect an integer at the front of a char list,
   parse it, and return the remainder of the list. Using our newly
   defined type abbreviation, we want to implement a function of type
       int parser
   Our strategy will be consume as many digit characters from the input
   as possible, and then pass them to Int.fromString.
*)

(* Problem 2: Write a function `list_such_that`, of type

       (char -> bool) -> char list parser

   which takes a boolean function on characters, and returns a parser
   that consumes characters of the input as long as the function
   returns true. 

 *)
		       
fun list_such_that (f : (char -> bool)) : char list parser =
  fn x =>
     let 
	 fun checker (l,l1,l2) =
	    case l of
		[] => (l1,l2)
	     | h::t => if f(h)
		       then checker(t, l1@[h], l2)
		       else (l1, h::t)
     in
	 checker(x,[],[])
     end	       
		
(* Problem 3: Write a parser `digits`, of type
       char list parser
   which consumes characters of the input as long as they are digits
   (ie, 0-9).

   Hint: Use `list_such_that`.

   Hint: Don't use a fun binding when a val binding will do!

   Hint: The standard library contains a function Char.isDigit, which
         you may want to look up. *)
val digits : char list parser = list_such_that Char.isDigit

					     
val ([#"1",#"2",#"3"],[#"(",#")"]) = digits (String.explode "123()") 

(*
 `digits` parses a list of characters, but in order to actually do
   arithmetic, we need to parse an int. `digits` has done half the
   work already by splitting the input into the part that can be
   converted into an int and the rust. Essentially all that remains is
   to call Int.fromString.

   However, instead of solving this problem like a normal person, we
   will solve it like a PL person: by solving a more general problem,
   and then getting what we need as a special case. *)

(* Problem 4: Implement a generic conversion function, of type
       ('a -> 'b) -> 'a parser -> 'b parser
   which takes a conversion function from 'a to 'b, and an 'a parser
   and returns a 'b parser, which parses the input using the 'a parser
   to get an 'a (and the rest of the input), and then calls the
   conversion function to return a 'b (and the same rest of the
   input).

   We will call this function `map` by analogy to List.map (look at
   the types!!!). *)

				    
fun map (f : ('a -> 'b)) (g: 'a parser) : 'b parser =
  fn x => let
             val (x, l) = g(x)
           in
             (f(x), l)
           end
	      
  


val (3,[#"(",#")"]) = map length digits (String.explode "123()")

			
(* Problem 5: Having solved the general case, now use it to define
   what we actually wanted, an int parser.

   Hint: use digits, String.implode, Int.fromString, and valOf. For
         brownie (ie, not real) points, use function composition (the
         `o` operator) instead of lambda (the `fn` syntax). *)
			 
val int : int parser =
 fn x => let val (l1,l2) = digits(x) in (valOf(Int.fromString(String.implode(l1))), l2) end

 val (123,[#"(",#")"]) = int (String.explode "123()") 
			 
	      

(* Problem 6: Write a parser that parses constant exprs only.

   Hint: Use map and int  *)

	     
val constant : expr parser =
  fn x => let val (z,y) = int(x) in (Constant z, y) end
 
 val (Constant 3,[#"(",#")"]) = constant (String.explode "3()")


(* We already have enough parsers to parse all the basic parts of an
   expression:
     - we'll use `char` to parse things like parens, minus, plus,
       and times.
     - we'll use `int` to parse constants

   What we need now are ways of *combining* these basic parsers into
   more complex parts. As a simple example, suppose we want to parse
   pairs of ints, say in the syntax used in SML: "(1,3)". We should
   have a way to combine the parsers for parens, commas, and ints into
   a single parser for int pairs.

   Again, applying the PL person approach, it would be nice to have a
   generic way of combining two parsers into a single parser, where
   the "combination" is controlled by a user-specified function. *)

(* Problem 7: Write a function `map2`, which takes a "combiner"
   function of type 'a * 'b -> 'c, an 'a parser, and a 'b parser, and
   returns a 'c parser, which parses an 'a, then parses a 'b, and then
   calls f with the results. *)
     
fun map2 (f : ('a * 'b -> 'c)) (ap : 'a parser) (bp : 'b parser) : 'c parser =
  fn l =>
     let
	 val (a,l') = ap(l);
	 val (b,l'') = bp(l');
     in
	 (f(a,b), l'')
     end
		    	      

(* map2 is really the centerpiece of this library. It is a thing of
   beauty. We can use it to define a whole slew of useful parser
   combinators. *)

(* Problem 8: Define a parser combinator `pair` that takes two parsers
   and returns a parser that parses the pair of their results. *)
	
	 
fun pair (ap : 'a parser)(bp: 'b parser) : ('a * 'b) parser =
   map2 (fn (x,y) => (x,y)) ap bp
  
	    
 val (((),123),[#")"]) = pair (char #"(") int (String.explode ("(123)"))

			 
(* It is often the case that you want to parse two things, but then
   just keep one of them. In the above example, there's not much use
   to keeping the unit value around. *)

(* Problem 9: Define a parser combinator `right` that takes two
   parsers and returns a parser that parses both, but throws away the
   first parser's result, returning only the second. *)

fun right (ap : 'a parser) (bp :'b parser) : 'b parser =
  map2 (fn (x,y) => y) ap bp


val (123,[#")"]) = right (char #"(") int (String.explode ("(123)"))
			 
(* Problem 10: Define a parser combinator `left` that takes two
   parsers and returns a parser that parses both, but throws away the
   *second* parser's result, returning only the first. *)

       
fun left (ap : 'a parser) (bp :'b parser) : 'a parser =
  map2 (fn (x,y) => x) ap bp

		
val (123,[#"(",#")"]) = left int (char #",") (String.explode ("123,()"))
		
       
(* Problem 11: Write an (int * int) parser that accepts the syntax "(1,3)"  *)
       
val int_pair : (int * int) parser =
 fn l => pair (right (char #"(") (left int (char #","))) (left int (char #")")) l
	     
  

val ((1,3),[#"f",#"o",#"o"]) = int_pair (String.explode "(1,3)foo")

(* We can use SML's infix operator feature to allow even nicer ways of
   writing parsers using these combinators.*)

infix 2 **
fun f ** g = pair f g

val (((),123),[#")"]) = (char #"(" ** int) (String.explode ("(123)"))

infix 3 >> <<
fun f >> g = right f g
fun f << g = left f g

val (123,[#")"]) = (char #"(" >> int) (String.explode ("(123)"))
val (123,[#"(",#")"]) = (int << char #",") (String.explode ("123,()"))

(* Problem 12: use the infix operators to rewrite your implementation
   of int_pair (we'll just shadow the binding) *)

val int_pair : (int * int) parser =
  fn l => (((char #"(") >> (int << (char #","))) ** (int << (char #")"))) l

val ((1,3),[#"f",#"o",#"o"]) = int_pair (String.explode "(1,3)foo")


					
(* Problem 13: Write a parser combinator that "surrounds" the parser
   with parens. In other words, given a parser p, return a parser that
   parses an open paren, then parses something using p, and then
   parses a close paren, returning only the value given by p. *)
		
fun parens (ap : 'a parser) : 'a parser =
  fn l =>  ((char #"(") >> (ap << (char #")"))) l


val (123,[]) = parens int (String.explode "(123)")

		    
(* An expr can be one of four things: constant, negation, addition,
   and multiplication. So far, we have developed several combinators
   to parse "each of" a set of things, but we need a way to parse "one
   of" a set of things. *)

(* Problem 14: Define a parser combinator `or`, which takes two parsers
   and tries to run the first parser. If it succeeds, `or` returns the
   result immediately. Otherwise, if the first parser throws an
   exception (any exception at all), `or` instead tries to run the
   second parser. *)

fun or (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fn l => p1(l) handle exn => p2(l)

(* Problem 15: Use `or` to define a parser for possibly parenthesized
   ints, which can parse any input that's an int or a parenthesized
   int. *)
				
val possibly_parenthesized_int : int parser =
   fn l => or (parens(int)) (int) l

 val (123,[#"f",#"o",#"o"]) =
  possibly_parenthesized_int (String.explode "123foo")

val (123,[#"f",#"o",#"o"])=
  possibly_parenthesized_int (String.explode "(123)foo")


(* Problem 16: Describe in English how possibly_parenthesized_int
   parses the previous two examples, particularly with regard to
   exceptions and handlers. *)

(*
Explanation: 
The function possibly_parenthesized_int initially makes a call to 
our "or" function with two arguments. We first want to check if the input
is wrapped in parenthesis so it makes a call to "parens" with the argument
"int" within, in order to parse the int within if the parser doesn't throw
an error. If "parens(int)" does throw an error, in the case of the input not
starting with parenthesis, then it will handle the exception by passing the 
list to just the "int" function. 

 *)

(* We can give nice syntax to `or` as well. *)
infix 0 <|>
fun p1 <|> p2 = or p1 p2


(* Problem 17: Write a parser for fully parenthesized exprs. Your
   parser will need to be recursive since expressions contain
   subexpressions. Because recursion only works with functions in SML,
   you cannot just write a big parser combinator expression in a val
   binding, but instead you need to "unfold" the definition of the
   parser type, and take the input char list as an argument, which you
   then immediately call on whatever big parser combinator expression
   you decide to use.

   Hint: Use `or` several times to decide between four branches: one
         each for constants, negation, addition, and multiplication.

   Hint: Use `constant` from above to parse constants.

   Hint: For negation, parse a '-' character and then recursively
         parse an expr, throwing away the minus sign. Then use map to
         apply the Negate constructor to the resulting expression.

   Hint: For addition and multiplication, use `parens`. Inside,
         recursively parse an expr, then a plus or multiplication
         sign, and then another expr. Use `map2` to apply the Add or
         Multiply constructor as appropriate.
*)	

fun expr l =
  ( constant
	<|> parens ((char #"-") >> expr) <|> ((char #"-") >> expr)
	<|> parens (map2 (fn (x,y) => Add(x,y)) expr ((char #"+") >> expr))
	<|> parens (map2 (fn (x,y) => Multiply(x,y)) expr ((char #"*") >> expr))
  ) l

    
	(* =================== COMMENT BAR =====================>
 val (Multiply (Constant 3,Add (Constant 1,Constant 2)),[#"f",#"o",#"o"]) =
  expr (String.explode "(3*(1+2))foo")


(* Challenge Problem 18: Define an alternate parser for exprs, called
   `space_exprs`, which allows spaces in expressions.

   Hint: Start by defining a parser `spaces` that consumes any number
         whitespace characters. Then sprinkle calls to this all over
         the place.
*)


(* Challenge Problem 19: The expr parser above works fine for tiny
   examples, but it has trouble with certain pathological cases.

   Consider the functions `good` and `bad` below, each of which
   constructs an expr that multiplies the first n numbers
   together.
*)

fun range lo hi =
  if lo >= hi
  then []
  else lo :: range (lo + 1) hi

fun good n =
  List.foldr Multiply (Constant 1) (List.map Constant (range 1 n))

fun swap (x, y) = (y, x)

fun bad n =
  List.foldl (Multiply o swap) (Constant 1) (List.map Constant (range 1 n))

(* Here is a function to print an expr in fully parenthesized syntax. *)
fun expr_to_string e =
  case e of
     Constant n => Int.toString n
   | Negate e => "-" ^ expr_to_string e
   | Add (e1, e2) => "(" ^ expr_to_string e1 ^ "+" ^ expr_to_string e2 ^ ")"
   | Multiply (e1, e2) => "(" ^ expr_to_string e1 ^ "*" ^ expr_to_string e2 ^ ")"


(* The only difference between `good` and `bad` is the nesting of
   parentheses (and the location of the initial 1): *)

val "(1*(2*(3*(4*(5*(6*(7*(8*(9*1)))))))))" = expr_to_string (good 10)
val "(((((((((1*1)*2)*3)*4)*5)*6)*7)*8)*9)" = expr_to_string (bad 10)

(* However, try running the parser on these strings as you increase
   how many numbers are being multiplied. The parser will handle the
   `good` expressions easily up to depth 1000, but by the time we get
   to even depth 20 with the bad expressions, we can feel a noticeable
   delay. By 30 or so it fails to terminate within several minutes. *)

(* Challenge Problem 20: Explain why the parser is so much slower on
   the bad expressions.

   Hint(?): What happens if you replace Multiply with Add in the
            definitions of good and bad? *)

(* Challenge Problem 21: Refactor the parser to eliminate this
   performance problem.

   Hint: Eliminate the problem you described in your answer to the
         previous challenge problem. *)

(* We'd like to implement an expr parser that does not require
   parentheses around everything. *)

(* Challenge Problem 22: What happens if you just delete the parts of
   the parser that parse parens? Why does this happen? *)

(* Challenge Problem 23: Fix the problem you discovered in the previous
   problem, resulting in a parser that doesn't require parens.
   (This is pretty hard.)

   Hint: Start by writing a new parser combinator, called `many`,
         which takes a parser p, and parses the input using p an
         arbitrary number of times. You can implement this using <|>
         more just directly on the definition of parsers. (You can
         think of `many` is a generalization of `accept_while` that
         works on arbitrary parsers instead of just single characters
         satisfying a predicate.)

   Hint: Reorganize the "grammar" of expressions into two levels, one
         that parses constants and negations, and one that parses
         additions and multiplications. *)

(* Challenge Problem 24: Extend your new parser to handle parentheses
   where necessary in order to group operations. *)

(* Challenge Problem 25: Extend your new parser to correctly handle
   order of operations, if it doesn't already. *)
*)
