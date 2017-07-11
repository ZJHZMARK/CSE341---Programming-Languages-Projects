# Problem 1: SML-Forall

Here is the identity function in SML.

    fun id x = x

Its type is 'a -> 'a, which intuitively means:

    Plug in whatever type you want for the type variable 'a, and id has
    the resulting type.

Plugging in int for 'a results in int -> int.

For the rest of this problem, it will be useful to explicitly quantify
type variables. So from now on, we will write

    forall 'a. 'a -> 'a

for the type of the identity function. The "forall" notation is
intended to remind us that type variables allow *any* type to be
plugged in for them.

All types in SML implicitly quantify all the type variables at the
front of the type. For example, the function composition operator's type

    ('b -> 'c) -> ('a -> 'b) -> 'a -> c

would be written

    forall 'a 'b 'c. ('b -> 'c) -> ('a -> 'b) -> 'a -> c



Here is a silly SML program that does not typecheck.

    fun f g = (g true, g 0)

    val (a, b) = f id

Intuitively, the call to `f` is safe at run time, because it passes in
the identity function, which works on any type. But the SML type
inference system refuses to accept the definition of `f` itself.

## Problem 1a:

Explain in a step-by-step manner how the SML typechecker arrives at
the conclusion that `f` does not typecheck, focusing on the two calls
to `g` in the definition of `f`.

SML begins by looking at the written function `f` and seeing that it holds the
argument g. After this, it goes to the right side of `f` to infer the type
of the g argument. When g is initially called with the value true, SML infers
two things. One of which is that g is a function and it also infers that g takes
one argument of the boolean type. Although, this becomes an issue once SML moves
on, as it sees a call to g with the value 0. Now, SML does not know that the
argument g is a function of type 'a -> 'a, at this point it assumes the function
takes in a boolean value. When the typechecker reaches "g 0", it runs into a
problem because the previous inference that g takes a boolean is now false
because an int was passed in. After all of this has occurred, SML will throw
an error claiming the function g's domain is boolean, but it was passed an
integer.


Now consider a hypothetical extension of SML, called SML-Forall, that
allows foralls to appear anywhere in a type, instead of just at the
very beginning.

For example, consider the SML-Forall type

    (forall 'a. 'a -> 'a) -> bool * int

This is the type of a function that takes an argument of type
(forall 'a. 'a -> 'a) and returns a pair of a boolean and an int.

In other words, the argument must be a polymorphic function itself!

This is a valid type for f. You will explain why soon.

## Problem 1b:

Here is a list of possible SML-Forall types for f.

  1: (forall 'a. 'a -> 'a) -> bool * int
  a. This type belongs to a function that takes an argument which is also a function
  that takes an argument of type 'a and also returns a result of 'a. The outer function
  returns a result of pair containing a boolean value (true/false) as the first
  part and an integer as the second, i.e (true, 3).

  b. In nearly all cases within SML, this type could not be replicated with
  standard SML types. The only case that worked in my case was a function that
  takes a function with type 'a -> 'a as a bogus argument and just returns a
  constant pair of (bool * int), i.e (fun f (g : 'a -> 'a) = (true, 3)). Clearly
  this is useless to define, but it does match up with the type. When trying to
  use the function's argument within the function's definition, it will throw an
  error similar to what I described in problem 1 where the inner function cannot
  take two arguments of different types.

  c. In our created extension of SML, this type _does_ typecheck and is a valid
  declaration for a function. In our definition using forall, the program checks
  for 'a as it is explicitly quantified in the type description of the argument
  within f. When it sees the the type (forall 'a. 'a -> 'a), it is able to
  locate the single indicated type. It now knows the program is able to take
  _any_ value and output the same type of the inputted value. Although value
  restriction holds us back in normal SML, our newly expanded language gets
  around this and more freely allows polymorphic arguments to be used.

  d. Yes, `f id` would typecheck because as defined previously in this text,
  the SML-Forall type of the identity is forall 'a. 'a -> 'a. Using this as
  an argument to our function f would be acceptable and matches the type it
  desires.

  2: forall 'a. ('a -> 'a) -> bool * int
  a. For this type, we can see that the forall 'a. is on the outside and not
  wrapped within the parenthesis holding the argument function of `f`. This
  changes our type because "forall 'a." is now wrapping the argument as well as
  it's return type with the type 'a. Similar to the composition function listed
  above, this type imposes that both the argument and result type of function `f`
  should return any value of type 'a.

  b. Similar to the last problem, this would also not typecheck in SML. Due
  to the strict value restrictions in SML, this inner function expects arguments
  of type 'a, and once it sees the bool followed by the integer, it will throw
  an error. Therefore, it would not be possible to translate this type to standard
  ML.

  c. This type would also not typecheck in our extended version of SML because
  the explicit listing of the types being used is in the very front of the
  type declaration, meaning that it spans across both the arguments and result
  of function f. This type needs to include bool and int for it to be valid, as
  'a is not the only type that shows up within the statement given. For it to
  typecheck, it would be wise to wrap the "forall 'a" within the parenthesis of
  the inner function, so it does not apply to the bool * int result.

  d. Lastly, using the identity function within function `f` would satisfy the
  type of the argument, but would still cause an issue with the forall statement
  being up front. The issue with the type in this case does not fall within the
  argument, but rather the types of the result. Therefore, there would not be a
  function foo that would satisfy the typechecker in our extended SML language.
  To resolve this issue, we would need to add bool and int to our explicit
  declaration for forall.

  3: forall 'a. (forall 'b. 'b -> 'a) -> 'a * 'a
  a. In this situation, there are two forall's included in the type declaration.
  The outer forall describes that the function argument and it's result will
  return a type of 'a. The forall located within the inner argument of `f`
  describes the argument will be a function that takes a 'b and returns 'a.

  b. When trying to define a function similar to this in standard, I ran into
  the same issue. Even though the inner function indicates that the argument
  of type 'b will result in type 'a, SML will not accept an input to this
  function. By this I mean that even if you inputted arguments of the same type,
  i.e. two integers, SML will not accept this because of its tendency of keeping
  types implicit while typechecking our function with its declared types.

  c. This type would also not typecheck in our extended version of SML. The outer
  forall is perfectly acceptable, as both the arguments and results are
  listed as type 'a. Although, the problem lies within the inner function, where
  the forall lists b as it's explicit quantifier. This is not the case
  because the inner function actually includes type 'b as the input and 'a as
  the output. To make this valid, the inner function would need to have a type
  of "forall 'a 'b. 'b -> 'a", properly listing the types of both the result
  and arguments of the function.

  d. Using the identity function as an argument for `f` in this situation would
  not be appropriate. This is because the identity function holds the type 'a -> 'a
  as it will return the same object it has been inputted without alteration.
  In this case, the function being used as an argument holds the type 'b -> 'a
  meaning there is some transformation going on. After some research on polymorphic
  functions in SML, I can confidently say that a function of type 'b to 'a would
  not be possible. Interpreting a function like this would be saying it can take
  a value of any type 'b and turn it into another value of any type besides 'b.
  There are simply too many restrictions in SML to accomplish making a polymorphic
  function like this.

  4: forall 'a 'b. ('b -> 'a) -> 'a * 'a
  a. For this scenario, we have `f` taking another function as an argument.
  This inner function transforms objects from type 'b to type 'a. In addition,
  the outer function has a final return type of 'a * 'a which can be interpreted
  as a pair of two objects that share the same type, but are free to hold whatever
  type they desire.

  b. This is similar to the last problem, where the inputted function transformed
  its argument from type 'b to type 'a. Research on polymorphic functions within
  standard SML leads me to believe that a type like this would not be achievable
  in standard ML. SML strongly typechecks a function's argument type against how
  that function was used so declaring type 'b to 'a would need a function that
  can transform any value in SML to another value holding a type other than
  it's own, which does not seem possible. Therefore, this type can not be
  typechecked successfully in standard ML.

  c. This function would typecheck in the SML-Forall extension because it meets
  the criteria for a properly annotated function. At the very beginning, forall
  explicitly declared the use of two types, 'a and 'b. The input and output of
  this function both use those types, and nothing more so the type is appropriate.
  Due to the types being properly listed in the forall statement, I can confidently
  say that this would typecheck in SML-Forall.

  d. As explained in 3d, the identity function is a function that takes an
  argument of type 'a and returns that same element, of type 'a. Using that function
  here would make no sense because the argument is a function of type 'b -> 'a.
  This means `f id` would not be an allowed call. Alternatively, creating a
  function to substitute for `id` would be equally difficult. Transforming such
  a general form into another form is a challenge to overcome with the restrictions
  placed on the SML language.  

For each one, do the following:

  - Describe the meaning of the type in English.

  - Say whether the given SML-Forall type corresponds to any SML type
    or not. If so, give the corresponding SML type. If not, no
    explanation is required.

  - Say whether f would typecheck in SML-Forall at that type. Briefly
    explain why or why not, focusing on the two calls to `g` in the
    definition of `f`. Additionally, if `f` would not typecheck in
    SML-Forall at the type, give a function that *would* and explain
    why it typechecks, or say that no such function exists.

  - Say whether the call `f id` would typecheck in SML-Forall if f had
    that type. Briefly explain why or why not, focusing on whether
    `id` is a valid argument to `f`. Additionally, if `f id` would not
    typecheck in SML-Forall, give another expression `foo` that would
    be a valid argument to `f`, or say that no such expression
    exists. In either case, explain briefly.

There are four types and four tasks for each type. Please organize
your solution so that it is easy to read.


## Problem 1c:

Briefly comment on how many SML-Forall types f has. Is there a "best"
SML-Forall type for f?  Why or why not? If appropriate, draw analogies
to similar situations in SML.

The function `f` has infinitely many SML-Forall types. This can be concluded
because Forall is used to explicitly quantify the type variables, and it has
access to whichever types it needs. This means f has SML-Forall types ranging
from 'a to 'z, and so on. Since representing a type with '(any letter) includes
a type of any value, the function f has an endless choice of what types it wants
to include or not include in its definition.


## Problem 1d:

What do you think of SML-Forall? Do you foresee any issues in
implementing a typechecker or interpreter for SML-Forall? Why do you
think SML does not support this kind of extension? Do you think
languages other than SML might?

I think SML-Forall is a great solution to get around some of SML's drawbacks
on restricting types but it also has its flaws. SML was made with the typechecking
system it has in order to prevent errors during use by strictly enforcing types
on run-time. This means that before any function, value or variable is called,
SML has done its job to prevent as many errors as it can. For example, a dynamically
typed language like Racket would allow you to write a function that added two strings
and would not raise any errors about it even if the file was ran. This becomes an
issue when the user calls this function to realize the incorrect types being used
within. In SML, you can be fairly confident about a minimal amount of errors
post run-time. Creating a typechecker or interpreter for SML-Forall would definitely
be a challenge. Considering the vast amounts of types it holds, as well as pairs
and tuples of those types, it could be a program that is not ready to handle every
part of the SML-Forall extension. In addition, we would run into issues with the
SML side of things while trying to implement a typechecker or interpreter in the
standard ML language, including it's restrictions on inputted and return types
for functions using SML-Forall. I imagine this would be similar to the challenges
we faced during the MUPL homework, but on a much larger scale. SML most likely
resisted to support this kind of extension because of the features it would lose.
I see the extremely restrictive typechecking system within standard ML as its
key aspect and allowing this feature would take away from that. Programs written
in SML using this extension would falsely hold the reputation of being reliable
for use after run-time so it would be better to leave it aside. That being said,
other languages may find use out of it. For example, a language like Ruby with
absolutely no types could replicate a feature from a dynamically typed language
which could further assist in developers catching type related bugs before the
program is even ran. Although implementing something like this in an object
oriented language would be a bit harder, it is still a possibility that could
improve the feel of coding in Ruby. In addition, a language like Racket would
find a Forall extension useful if it were to throw an exception when it detected
an incorrect type. I say this because my experience with Racket has been repeatedly
filled with an absence of errors when running the program, and a large amount of
debugging needed once I run the functions I create. All in all, SML-Forall is a
valuable extension but it can be improved and most likely isn't the best use in
a language like standard ML.
