(* Problem 2: Promises and streams in SML. *)

(*

You will implement two modules (structures) below, corresponding to
the provided signatures PROMISE and STREAM. Each module defines an
abstract type along with several operations on the type.

Note that the provided code will not load in the SML REPL because it
will complain that you have not implemented all of the required
bindings yet. In order to test your code, you can either make dummy
implementations of each binding that just throw an exception for now.
Or you can comment out the ":> PROMISE" and ":> STREAM" from the code,
so that SML will not try to check that all the functionality is
implemented. If you do this, be sure to uncomment these in order to
check that you have implemented all the right functions with all the
right types.

You are also asked to write a few programs that use the Stream module.

*)


(* In SML, a thunk of type foo is a function from unit to foo. *)
type 'a thunk = unit -> 'a


(* Problem 2a: Promises *)

signature PROMISE =
sig

  (* sA promise encapsulates some piece of delayed computation. *)
  type 'a promise

  (* You can construct a promise from a thunk using `delay`. *)
  val delay : 'a thunk -> 'a promise

  (* You can get the result of a promise using `force`. Note that the
     thunk "inside" the promise should be called at most once during
     its life. The first time it is called, the answer should be
     saved and returned by all future calls to `force`. *)
  val force : 'a promise -> 'a

  (* Sometimes you already have a value that you want to treat as a
     promise. `now` lets you do that. It is equivalent to calling
     delay on a thunk that always returns the value. *)
				
   val now : 'a -> 'a promise 
end

structure Promise :> PROMISE =
struct
			
datatype 'a promise = evaluate of 'a thunk | evaluated of 'a
		      | pref of 'a promise ref
							      
fun delay(a: 'a thunk) = pref(ref(evaluate a))
				  
fun force(a: 'a promise) = let val pref(x) = a in
			       case !x of
				   evaluate a =>
				   let
				       val ex = a ()
				   in
				       x := evaluated ex;
				       ex
				   end
				 | evaluated x => x
				 | pref(x) => force(pref(x))
			   end
			       
fun now(a: 'a) = pref(ref(evaluated a))
			       
end

    
(* Tests for Promise. *)

val p1 = Promise.delay (fn () => print "Hello!\n")
(* Shouldn't print anything. If you run the following line though,
   then the print should happen. *)

(* Promise.force p1 *)

(* Calling `Promise.force p1` a second time should *not* print. It
   should just return unit again. *)
		       

(* Problem 2b: Streams *)

signature STREAM =
sig
  (* A stream is an abstraction over a potentially infinite sequence
     of values. Internally, streams are represented as a delayed
     computation (a promise, in fact).  *)
  type 'a stream

  (* The delayed computation returns either an end-of-stream marker
     (Nil) or a pair (Cons) containing the first element of the stream
     and the rest of the stream. *)
  datatype 'a result = Nil | Cons of 'a * 'a stream

  (* Given a stream, ask it for its next element. If the stream is
     empty, the result will be Nil. Otherwise it will be Cons
     containing the head and tail of the stream. *)
  val toResult : 'a stream -> 'a result
				 

  (* Given a thunked result, construct the corresponding stream. *)
				 
   val fromResultThunk : 'a result thunk -> 'a stream 

  (* Given an immediate result, construct the corresponding stream. *)
   val fromResult : 'a result -> 'a stream


  (* Construct a stream that repeatedly calls the given thunk. As long
     as the thunk returns SOME x, the stream continues and consists of
     the sequence of (unwrapped) returned values. The stream
     terminates if (and only if) the thunk eventually returns NONE. *)
  val whileSome : 'a option thunk -> 'a stream


  (* Like `whileSome`, except that the thunk maintains some state,
     represented by a value of type 'a. The states do not appear in
     the resulting stream; they are only used internally by the
     thunk. *)
  val whileSomeStateful : ('a -> ('a * 'b) option) -> 'a -> 'b stream


  (* Apply the given function to every element of the given stream. *)

  val map : ('a -> 'b) -> 'a stream -> 'b stream


  (* Take the first `n` elements of the stream and put them in a list. *)
  val take : int -> 'a stream -> 'a list

  (* Given a "stream transformer" `f` (a function from streams to
     stream), `weird` creates a stream `s` such that `f s` and `s`
     contain the same elements. (This one is hard. And weird. Sample
     solution is one line.)  *)

(*  val weird : ('a stream -> 'a stream) -> 'a stream  *)


(* TESTER FOR WEIRD
 val int_transform: int stream -> int stream *)

end
				 
structure Stream :> STREAM =
struct
open Promise
	 
datatype 'a stream = Empty | delayed of ('a * 'a stream) promise (* do not change *)
						 
datatype 'a result = Nil | Cons of 'a * 'a stream
					   
fun toResult(s :'a stream): 'a result =
  case s of
      Empty => Nil
    | delayed p =>let val (x:'a , y: 'a stream) = force(p) in
		      Cons(x, y)
		  end
							    					
fun fromResultThunk(th :'a result thunk) : 'a stream =
  case th() of
      Nil => Empty
   | Cons (x, y) => delayed(delay(fn() => (x,y)))

fun fromResult(res: 'a result) : 'a stream =
  case res of
      Nil => Empty
   | Cons (x, y) => delayed(delay(fn() => (x,y)))
							 
fun whileSome(th: 'a option thunk) : 'a stream	=
  case th() of
      SOME x => delayed(delay(fn() => (x, whileSome(th))))
   | NONE => Empty

(* f(current state) -> (new state, next element in stream) *)
(* f(1) -> f(2, 3) *)
fun whileSomeStateful(f :('a -> ('a * 'b) option)): 'a -> 'b stream =
  fn(state) => case f(state) of
		   SOME(x, y) => delayed(delay(fn() => (y, whileSomeStateful(f) x)))
		 | NONE => Empty
			       

fun map(f :('a -> 'b))  =
	fn(str) => case str of
		       Empty => Empty
		     | delayed(p) => let val (x , y) = force(p) in
					 delayed(delay(fn() => (f(x), map(f) y)))
				     end
fun take(n :int) =
  fn(str) =>
     if (n > 0)
     then
	 case str of
		     Empty => []
		   | delayed(p) => let val (x, y) = force(p) in
				       x :: (take(n - 1) y)
				   end
     else
	 []
				       
(* Given a "stream transformer" `f` (a function from streams to
     stream), `weird` creates a stream `s` such that `f s` and `s`
     contain the same elements. (This one is hard. And weird. Sample
     solution is one line.)  
fun weird(f: 'a stream -> 'a stream): 'a stream =
  f(delayed(delay(f(weird(f))))) *)
		      
(* TESTER ! 
fun int_transform(s :int stream): int stream =
  case s of
      Empty => Empty
    | delayed p =>let val (x:int , y: int stream) = force(p) in
		      delayed(delay(fn() => (x+5, int_transform(y))))
		  end *)
end
							    
    
(* Problem 2c: Using streams *)

(* Use whileSomeStateful to define the stream of all natural numbers
   (including 0). Keep track of the "next" natural number as your
   state. *)

val nats_whileSomeStateful : int Stream.stream =
    (Stream.whileSomeStateful(fn(x) => SOME(x+1, x)) 0);


(* Show how to define nats using whileSome using mutable state. *)				 
val nats_whileSome : int Stream.stream =
    let
	val state = ref ~1; 
	fun f(x) = (x := !x + 1; !x);
	fun nat() = Stream.whileSome(fn() => SOME(f state))
    in
	nat()
    end


(* Helper functions for testing down below *)

fun stream_tester(num, stream) =
  case Stream.toResult(stream) of
      Stream.Cons(x, y) => if x = num
			   then
			       (print ((Int.toString (x) ^ "... END OF STREAM! ")))
			   else
			       ((print ((Int.toString(x)) ^ "...")); stream_tester(num, y))
    | _ => (print "Finish");


				   
fun nat_until(num) = stream_tester(num, nats_whileSomeStateful)	

fun nat2_until(num) = stream_tester(num, nats_whileSome)	
