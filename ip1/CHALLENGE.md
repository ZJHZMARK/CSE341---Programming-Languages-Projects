# Challenge Problems

Challenge problems for this project are a bit more open-ended than
most of the ones on the homeworks have been. To work on a challenge
problem, make a new file and copy in all the code you need from
previous files. You do not need to complete the "whole" challenge
problem to receive any extra credit; "partial" extra credit will be
given. Most of these challenge problems are orthogonal, in the sense
that you can add any subset of them to the language. So, you don't
need to work on them in order, and they may not be listed in order of
difficulty, so check out the later ones, too! For simplicity, it may
be best to work on each challenge problem separately (ie, not building
on other challange problems you have also done for this project), but
you may also try to incorporate multiple challenges into a single
language if you want (it's just that debugging may be harder).

## More types

Add some other of your favorite types to the language. For example
pairs (or tuples) or lists. For each type you add, you'll need to add
new expression forms (how do you *make* values of this type? how do
you *use* values of this type?), new value forms (what are the values
of this type?), new `ty` forms representing the type. You will also
need to extend the evaluator and typechecker to handle the new
expression forms.

Write tests that demonstrate all the new expressions and types you add
to the language.


## Let expressions via SML's mutually recursive datatypes

Add `let ... in ... end` expressions to your language. This is weird
because the part between the `let` and the `in` is a sequence of
*bindings*, not expressions. This means that expressions can now have
bindings inside of them! SML does not allow later types to refer to
earlier types by default, but to represent this, we need to allow the
`binding` type to refer to `expr` and vice versa. To do that, you can
use so-called "mutually recursive" datatypes. The syntax is:

    datatype foo = A of int | B of foo | C of bar
    and bar = D of bool | E of foo | F of bar

which defines two types, `foo` and `bar` "simultaneously". Notice that
constructors of both types can refer to both types recursively. You'll
want to use this on your `expr` and `binding` types. The new form of
expression for `let` should take a list of bindings as well as a
"main" expression.

Extend you evaluator and typechecker to support `let ... in ... end`.

Write tests to exercise `let ... in ... end`.


## Parsing and the 'R' in REPL

Write a parser for (some version of) your language. You can either
write the parser in the style of the hw2 challenge problems (write a
tokenizer and then a parser; no higher order functions or
combinators), or you can do it in the style of hw3 (parser
combinators), even reusing the parsing library you developed there.

You will want parsers for (at least) your `ty`, `expr` and `binding`
types. You can use any syntax you want, but it's probably best to
model your syntax on SML.

There is a specific kind of parsing problem that we haven't dealt
with yet, often called "left recursion", which can result in an
otherwise correct-looking parser instead going into an infinite
loop. If you find yourself in such a situation, try requiring more
things to be parenthesized as we did on hw3. (If you want to
understand why this is, a good place to start is to go back and do
the hw3 challenge problem on getting rid of parentheses.)

Use your parser for bindings to write an improved REPL function,
which, instead of taking a pre-parsed list of bindings, instead
reads bindings in from standard input one line at a time, parses
them, typechecks them, and evaluates them, before returning to read
the next binding. This makes your REPL into a "true" REPL. (Before
it was more of an EPL.)

(You may want to check out the TextIO module in the standard library
to see how to read input. I recommend using a line-based input
format (one binding per line) rather than the more complicated
semicolon-based thing that SML does, but you are welcome to try the
latter as well.)


## Anonymous first-class functions (lambdas)

Add first-class functions to the language. This will involve adding
two new expression forms: lambda (aka fn) and function call.
Function calls consist of two subexpressions: the function to be
called, and the single argument.  Lambdas should contain a variable
name (string) and a function body (expr). In order to make type
cheking easy, lambda expressions should also contain a `ty`, which
is interpreted as a type annotation for the argument type. (This
means you will need to move you datatype binding for `ty` up to the
top of the file, above `expr`.)

You will also need to add a new kind of value, namely a closure.
Recall that a closure consists of all the information in a lambda
*plus* a dynamic environment. (In fact, closures will not need the
type information from the lambda, just the variable name and body,
in addition to the environment.)  This means that values can contain
dynamic environments, while dynamic environments also contain
values.  SML does not allow later types to refer to earlier types by
default.  You can work around this in two ways: just "inline" the
definition of dynamic environment into the datatype binding for
`value`; or use SML's `withtype` feature, which allows you to
declare a `datatype` and a `type` abbreviation "simultaneously".
Either solution will work and receive equal credit.  The syntax for
`withtype` is as in this example:

    datatype foo = bar of baz
    withtype baz = foo list

Notice that `foo` refers to `baz`, which refers to `foo`.

Extend your evaluator to handle lambdas and function calls.  Lambdas
should evaluate immediately to a closure (what dynamic environment
should you put in there?). Function calls evaluate by first
evaluating the left subexpression to get a closure, then evaluating
the right subexpression to get a value, and then continuing
evaluation by evaluating the body of the closure in a suitable
dynamic environment (you have to figure out what "suitable" is
here).

Extend you typechecker to handle lambdas and function calls. Start
by adding a new type to the language, for representing function
types. This type takes two arguments: the source type and the
destination type; these should be analogous to the SML types `t1 ->
t2`. Lambdas typecheck by typechecking the body in an extended
static environment (which one? use the type annotation!); then the
resulting type will be some sort of suitable function type.
Function calls typecheck by first checking that the left
subexpression has some function type, and then checking that the
right subexpression (the argument) has the same type as the source
type of that function type.

Write test to cover all the new forms of expression evaluation and
typechecking. Write tests demonstrating that you have implemented
lexical scope.

Briefly describe in English how you would change your *evaluator* to
support dynamic scope. Briefly describe how you would change your
*typechecker* to support dynamic scope.


## Recursive functions via `fun` bindings

Add `fun` bindings to the language. Recall that `fun` bindings are
almost syntactic sugar for `val` bindings with a lambda. The only
difference is recursion. So the hard part of this challenge problem
is dealing with recursion.

Start by adding a new constructor to the `binding` type for our
language, to represent `fun` bindings. It should take a function
name (string), an argument name (string), an argument type
annotation (`ty`), a return type annotation (`ty`) and a function
body (expr).

Then, skip the evaluator for just a second, and add typechecking
support for fun bindings.  Recall that the typechecking rules for
`fun` bindings support recursion by typechecking the body of the
function in a static environment that is extended not only with a
binding for the argument, but also for the function itself!  With
this in mind, briefly explain in English why the return type
annotation is necessary for `fun` bindings but not for lambdas.

Now, go back and implement evaluator support for `fun` bindings.
This is hard. At a high level, a `fun` binding should extend the
dynamic environment with a binding for the function name to a
closure representing the function's body.  The issue is that this
closure might make recursive references to the function itself, so
the dynamic environment inside the closure needs to contain the
closure itself!  You will need to "break this circularity" by using
a refrence.  There's more than one way to do this: you can either
change the dynamic environment type to be `(string * value ref)
list` or you can change the closure constructor to take a
`dynamic_environment ref`.  In either case, during evaluation of a
`fun` binding, you will need to initialize the reference to a
"dummy" value, and then update it later to create a cyclic structure
between the closure and its environment.

Write tests demonstrating typechecking and evaluating recursive
functions. For example, the language is now expressive enough to
write something like the following SML function:

    fun sum_till (n : int) : int =
      if n <= 0
      then 0
      else n + sum_till (n - 1)

(You will need to encode `n - 1` by "adding negative 1".)


## User-declared Datatypes

Add user-declared datatypes to the language. This is hard. Start by
adding a new kind of `ty` (consisting of a string for the name of
the datatype) to represent user-declared datatypes. Then, add a new
kind of expression for using constructors. Also add a kind of
expression for pattern matching. Much of this implementation (for
both typechecking and evaluation) can be adapted from hw3. You will
need to extend the static environment to be a pair of a "type
environment" and a "variable environment" instead of just the old
variable environment. The type environment should be something like
`(string * string * ty) list`, as described in the challenge problem
on hw3.

Also add a new kind of binding for declaring new datatypes. It
should take a name for the new type (string) and a list of
constructor declarations, where a constructor declaration is a pair
of a name (string) and an argument type (`ty`). Evaluating a
datatype binding does nothing. Typechecking a datatype binding adds
the relevant constructor declarations to the type environment
portion of the static environment.

While much of this problem is similar to hw3, the interesting parts
are integrating it as part of a larger language (in hw3 you wrote
all the "core" parts, but didn't integrate them into an actual
evaluator or typechecker; also, by making it part of a larger
language, you can actually use it in larger programs) and in
supporting datatype bindings themselves (hw3 assumed that this was
handled elsewhere; now you should implement it). In order to
simulate allowing constructors to take multiple arguments, you will
probably want to add tuples to your language if you have not already
done so. Support tuples in patterns as well (as in hw3).

Write tests exercising the new forms of binding and expression.  For
example, if you have implemented recursive functions, you could
declare a type of lists containing integers.  (Note that since our
language does not support type variables or polymorphism, we can't
yet make a generic list, but we can make a list of a specific type,
like a list of integers.) Then write some functions that manipulate
these lists of integers using pattern matching and recursion (eg,
sum a list, increment all elements of a list, etc.).  Even if you
haven't implemented recursion, you can still play with
enumeration-style types, eg,

    datatype day_of_week = Sun | Mon | Tues | ... | Sat

or even with one-of types, eg,

    datatype int_or_bool = Int of int | Bool of bool

and maniuplate this type of data with pattern matching.


## Unification-based Type Inference

(This one is sort of ridiculously hard, based on what little I've
given you. It also really only makes sense if you've done the
first-class function challenge above. But you *don't* need to have
done the `fun` binding one.)

Implement true type *inference* for our language. So far, lambdas
have required an argument type annotation, which was specifically
placed there to make typechecking easy. Now, we'd like to remove
this restriction.

Start by changing the lambda constructor of `expr` to not have a
type annotation on it. (If you've done the `fun` binding challenge
as well, you may want to just ignore that at first, and then come
back to it later.)

Type inference (as described in lecture) works essentially by
looking at the code and generating a set of *constraints* (what
must be true of the types in order for this code to make sense).
For example, in the code

    if x then ... else ...

we "know" that x must be a boolean, because otherwise it wouldn't
make sense to use it in an `if` like that. Type inference works by
first assigning a "completely arbitrary" type to each part of
expression, then generating a set of constraints (or equations) that
must be true for the expression to make sense, and then solving
those constraints in order to figure out the types.

Start by adding a new kind of type to the `ty` datatype, which is a
*type variable* (consisting of just a name (string)), analogous to
`'a` and the like in SML.

Structure your type inferencer as three functions:

  - `generate_constraints : expr -> (ty * ty) list` which takes an
    expression and returns a list of equations (represented by a
    pair of `ty`s that should be equal).

  - `solve_constraints : (ty * ty) list -> (string * ty) list` which
    takes a list of constraints and returns a *substitution*, which
    is a mapping from type variable names to the types they must be
    equal to.

  - `substitute : (string * ty) list -> ty -> ty` which takes a
    substitution and a type, and *applies* the substitution to the
    type, by plugging in for all the type variables in the type
    according to the mapping given by the substitution.

As mentioned above, this one is very hard. It's not actually that
much code (my solution is only 50 "new" lines of code), but it is
very subtle code, and I have not described the problem or the setup
very well. You are welcome to look at other resources to help you
with this problem, if you'd like to take a crack at it. You may want
to start by googling around for "unification based type inference"
or similar phrases. Also, as always, feel free to ask clarifying
questions. Remember that points for challenge problems are not given
in the same ration to effort on normal problems. :)

