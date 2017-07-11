(* CSE 341 hw2, Winter 2017 *)

(* Please complete all the TODOs below. Delete the "TODO"s as you go. *)

(* TODO, approximately 1 line: Your name here *)
(* TEJVEER RAI *)

(*

In this homework, we will work with JSON data in SML. We will
manipulate it, print it, and (optionally, as a challenge
problem) parse it. We'll also play with some real data.

This homework is significantly harder than hw1. There are 33 problems
below (22 required, 11 challenge). Several of the problems (both required
and challenge) require just one line of code. None require more than
25 lines of code.

Get psyched. Start early. Ask questions. We believe in you!

JSON is a human-readable data serialization format, originally
inspired by Javascript's Object Notation. JSON is commonly used to
represent data, for example Seattle Police Department crime report
data near the U district, which we'll play with below.

No previous experience with JSON is expected, but you may wish to read
up on it if you find yourself confused. The ultimate reference on JSON
syntax is RFC 7159, which describes the format in complete detail.

https://tools.ietf.org/html/rfc7159

To make things a little easier on ourselves, we will make some
simplifying assumptions about JSON, which are described as needed
below. This means that we will not quite be RFC compliant, but we will
be close, and it wouldn't be too hard to get there.

A JSON value is one of the following seven things:
- a number (floating point, eg 3.14)
- a string (eg, "hello")
- false
- true
- null
- an array of JSON values, written between square brackets and commas
  - eg: [1, "world", null]
- an "object", which is a sequence of field/value pairs
  - a field is always a string literal (eg, "foo")
  - a value is an arbitrary JSON value
  - objects are written with curly braces, commas, and colons
  - eg: { "foo" : 3.14, "bar" : [1, "world", null], "ok" : true }

We can represent the abstract structure (ie, everything except how its
written down in text) of a JSON value using the following SML datatype.

*)

datatype json =
         Num of real (* `real` is what SML calls floating point numbers *)
       | String of string
       | False
       | True
       | Null
       | Array of json list
       | Object of (string * json) list

(* Here are some examples of elements of this datatype. *)
val pi = Num 3.14
val hello = String "hello"
val my_false = False
val my_array = Array [Num 1.0, String "world", Null]
val my_obj = Object [("foo", pi), ("bar", my_array), ("ok", True)]

(* Part of this assignment is to explore some crime report data from
   the Seattle Police Department. The entire dataset can be viewed at

   https://dev.socrata.com/foundry/data.seattle.gov/pu5n-trf4

   For use in this assignment, we have selected all records in the
   dataset that took place within 1km of the Paul Allen Center
   for Computer Science and Engineering, using the following query URL.

   https://data.seattle.gov/resource/pu5n-trf4.json?$select=*&$where=within_circle(incident_location,47.653240,-122.305698,1000)&$limit=15000

   The resulting data can be found in JSON format in the file
   complete_police.json.

   Since that file contains over 10000 records and weighs in at around
   9 megabytes, we have also included small, medium, and large subsets
   of the data, containing 10, 100, and 1000 records, respectively,
   available in the files small_police.json, medium_police.json, and
   large_police.json.

   Converting the textual representation of JSON data into the
   structured representation given by the SML type json is the problem
   of parsing. Parsing JSON is an interesting problem, which we
   encourage you to explore (it's easier than you might think!), but
   it is marked as a challenge problem, and thus optional.

   In order facilitate playing with the data without needing to parse
   it, we have included the pre-parsed data files
   parsed_small_police.sml, parsed_medium_police.sml, and
   parsed_large_police.sml, each of which is a valid SML file that
   binds a single variable name to a value of type json. Thus they can
   be loaded into the REPL or other files with the `use` function, as
   usual. Unfortunately, the complete dataset is too large for SML to
   load in a reasonable amount of time when it is represented like
   this, so it is not available in the pre-parsed format.

   We will explore the data further below, but for now it suffices to
   say that it consists of a JSON Array of event records, each of
   which has several fields, such as the kind of event (eg,
   shoplifting or noise complaint) and the location (eg, 4500 block of
   15th avenue NE).
*)

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important because it tells SML
            that the previous binding has finished *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(* We can load the smallest set of police data as follows. *)
use "parsed_small_police.sml";
(* Now the variable small_incident_reports contains a json object representing
   10 incident reports. *)

(* We'll also load the medium subset. *)
use "parsed_medium_police.sml";

(* The large data set (which is still less than 10% of all the data)
   takes about 15 seconds to load, so it is commented out for now.
   You will need it for some later problems, so you should eventually
   uncomment it.

   When you do, you may find it useful to not re-load this file again
   and again, but instead load it once, and then try to debug using
   just the REPL. When you think you have something working, paste it
   back into this file, kill the REPL and try to reload. That way you
   don't have to wait 15 seconds between every debugging step.
*)
 use "parsed_large_police.sml";


(* Now make SML print more so that we can see what we're working with. *)
Control.Print.printDepth := 20;
Control.Print.printLength := 20;


(*** Section 1 (Required): Manipulating JSON values ***)

(* This is the exception that will be thrown by various "casting"
   functions that expect only a certain kind of JSON value (eg,
   expects an Array but was given an object). *)
exception BadCast

(* IMPORTANT NOTE: The file tests.sml contains at least one freebie
   test for each function you are asked to write. Looking at these
   tests will also help you understand what each function is supposed
   to do. For some functions, you will definitely want to try your
   code on more examples than the one we have given you. Put your
   additional tests in tests.sml and turn it in along with your
   code. *)

(* Problem 1: Write a function `unString` that takes a json value
   representing a string, and returns the string.

   If the given json value does not represent a string, raise BadCast. *)
fun unString (j : json) : string =
  case j of
      String(s) => s
   | _ =>   raise BadCast

(* Problem 2: Write a function `sub` (stands for subscript) that takes
   a json value representing an array, and an index n and returns the
   nth element of the array.

   If the given json value does not represent an array, raise BadCast.

   Hint: use List.nth from the standard library, which does something
         similar to `get_nth` from hw1, except that it is 0-indexed
         instead of 1-indexed. *)
fun sub (j : json, n : int) : json =
  case j of
      Array(a) => List.nth(a,n)
   | _ => raise BadCast

(* Problem 3: Write a function `assoc` that takes a string and a
   (string * json) list, and returns the first element of the list
   with the given string as its first component.

   If there is no such element, raise the built-in `Empty` exception.

   This function is intentionally annotated with a more general type,
   so that it can be reused in the parser below. For now, you can
   imagine that 'a means json. *)
fun assoc (k : string, l : (string * 'a) list) : 'a =
  case l of
      (x,y)::t => if (x = k) then y else assoc(k,t)
   | _ => raise Empty



(* Problem 4: Write a function `dot` that takes a json value j
   representing an object, and a field name f and returns
   the contents of that field in the object.

   If j is not an object or if it doesn't contain such a field,
   raise Empty.

   Hint: use assoc. *)
fun dot (j : json, f : string) : json =
  case j of
      Object(obj) => assoc(f,obj)
   | _ => raise Empty 


(* By glancing through some of the data, it appears that not all
   reports have the same fields. Let's investigate the set of field
   names. *)

(* Problem 5: Write a function that returns all the field names in a
   given JSON value representing object.

   If the given value does not represent an object, raise BadCast.

   Hint: you will need a helper function. *)
		
fun fields (j : json) : string list =
  let
      fun fieldnames (object_list : (string * json) list) : string list =
	case object_list of
	    (x,y)::t => x :: fieldnames(t)
		     | _ => [] 
  in
      case j of
	  Object(obj) => fieldnames(obj)
	| _ => raise BadCast
  end
      
		    

(* We can also look for fields inside nested objects, for example, to
   find the "coordinates" field in the first incident report. *)

(* Problem 6: Write a function to recursively traverse a json value
   and return all its fields, no matter how deeply nested.

   Hint: You'll need two helper functions, one to traverse arrays and
   one to traverse objects. Define these helper functions inside
   `recursive_fields` so that they can call `recursive_fields`
/   recursively.

      *)
      
fun recursive_fields (j : json) : string list =
  let		      
      fun traverse_arrays (array : json list) : string list =
	case array of
	    h::t => recursive_fields(h) @ traverse_arrays(t)
	  | _ => []

      fun traverse_objects (object_list: (string * json) list) : string list =
	case object_list of
	    (x,y)::t => x :: recursive_fields(y) @ traverse_objects(t)
	  | _ => []
  in
      case j of
	  Object(obj) => traverse_objects(obj)
	| Array(a) => traverse_arrays(a)
	| _ => [] 
  end
      

(*  (x::y)::t => traverse_objects(x) @  *)

(* `recursive_fields` tends to return a bunch of duplicates.  Here is
   a freebie function to deduplicate a string list, based on the
   sorting library that comes with SML.

   Note that because `dedup` is implemented in terms of sorting, it
   will likely change the order of the input list. *)
fun dedup (l : string list) : string list =
  ListMergeSort.uniqueSort String.compare l


(* Problem 7: write a function to compute a deduplicated list of field
   names from an object. *)
fun deduped_recursive_fields (j : json) : string list =
  dedup(recursive_fields(j))


(* When exploring a new dataset, it can be useful to compute various
   summary statistics. For example, we might want to know what the
   most common values in various fields are. To that end, we will
   write a `histogram` function that takes a list of strings and
   returns a list of string * int pairs, where the int represents how
   many times the string appeared in the original list.

   For example,
       histogram ["a", "b", "a"]
   will return
       [("a", 2), ("b", 1)].

   There are several ways one could imagine implementing histogram.
   Just for fun, we'll use the following strategy:
       1) sort the input list, placing all occurrences of the same
          string next to each other
       2) traverse the resulting list to count up the occurrences
          of each string in a single linear pass.
 *)

(* Problem 8: Write a function `count_occurrences` that takes a
   string list, which is assumed to be sorted, and returns a list of
   string * int pairs containing the number of occurrences of each string.

   `count_occurrences` should make a single pass over the list, taking
   advantage of the fact that it is sorted.

   The exact order of strings in the output list is not essential, your
   solution may differ ours, and that's okay, as long as the counts are right.

   Hint: you will likely want a helper function that takes additional
         arguments for the "current" string and its count.
 *)
       
fun count_occurrences (l : string list) : (string * int) list =
  let
      fun compare(x : string, y: string) =
	if y = x then (x,2) else (x,1) 
	      
  in
      case l of
	  h::t::t2 => compare(h,t) :: count_occurrences(t2)
	| [h] =>  [(h,1)]
	| _ => []
  end
      

(* Now here is a freebie implementation of `histogram` in terms of
   `count_occurrences`. It makes use of the standard library function
   ListMergeSort.sort, which uses a few features we haven't talked
   about yet.

   `histogram` works by sorting the input list of strings, then
   calling `count_occurrences`, then sorting the output by frequency,
   so that the most frequent entry appears first. *)
fun histogram (l : string list) : (string * int) list =
  let
    fun compare_strings (s1 : string, s2 : string) : bool = s1 < s2

    val sorted_l = ListMergeSort.sort compare_strings l
    val counts = count_occurrences sorted_l

    fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
      n1 < n2
  in
    ListMergeSort.sort compare_counts counts
  end


(* In order to use `histogram` to explore the data, we'll need a way
   of getting a string list out of the json value. For example, we
   might want to investigate the frequencies of various crimes in the
   U district. A brief description of each event as it was
   resolved is found in the "event_clearance_description" field. So our
   idea will be to project out all the values found in this field and
   then compute a histogram. *)

(* Problem 9: Write a function `string_values_for_field` that takes a field
   name and a list of json values representing objects, and returns a
   list containing the values contained in the given field for all the
   objects, assuming that those values are strings.

   Hint: Use `dot` from above.

   Hint: Use `unString` from above.

   Hint: Since not all objects will necessarily contain the field,
         you'll need to handle the `Empty` exception from `dot`.
         If an element of the list does not contain the field, do
         not include a corresponding value in the output list; just
         act like that object isn't in the input list.
*) 
 
      
fun values_for_field (field : string, l : json list) : string list =
  case l of
      [] =>  []
     | h::t => (unString(dot(h, field)) :: values_for_field(field, t)
	       handle Empty => [] @ values_for_field(field, t))
    	   

 
     
(* Problem 10: Write a function `unique_values_for_field` that
   deduplicates the output of `values_for_field`.

   Hint: Use `dedup` from above. *)
fun unique_values_for_field (field : string, l : json list) : string list =
   dedup(values_for_field(field,l))



(* Problem 11: Write a function `histogram_for_field` that computes a
   histogram based on the output of `values_for_field`. *)
fun histogram_for_field (field : string, l : json list) : (string * int) list =
  histogram(values_for_field(field,l))


(* Since our data is a json value representing an Array, but all our
   functions expect a json list, it is useful to convert one to the other. *)

(* Problem 12: Write a function `unArray` that takes a json value
   representing an array, and returns the underlying list of json
   values.

    If the given json value does not represent an array, raise BadCast. *)
fun unArray (j : json) : json list =
  case j of
      Array(h::t) => h :: unArray(Array(t))
     | Array(h) => h
     | _ => raise BadCast


(* Problem 13: Compute a histogram of the "event_clearance_description"
   field on the large dataset. *)
val large_event_clearance_description_histogram : (string * int) list =
   histogram_for_field("event_clearance_description", unArray(large_incident_reports))

(* Problem 14: Compute a histogram of the locations of the reports in
   the large dataset, using the information in the field "hundred_block_location" *)
val large_hundred_block_location_histogram : (string * int) list =
  histogram_for_field("hundred_block_location", unArray(large_incident_reports));


(* We might be interested in looking only at events at a certain location. *)

(* Problem 15: Write a function `filter_field_value` that takes a field
   name, a value in that field (assumed to be a string), and a list of
   json values, and returns just the elements of the list that have
   the given value in the given field.

   Hint: Use `dot`.

   Hint: Since not all elements may have all fields, you'll need to handle
         the `Empty` exception from `dot`.
*)
fun filter_field_value (field : string, value : string, l : json list) : json list =
  case l of
      [] => []
    | h::t => (if unString(dot(h,field)) = value
	       then h :: filter_field_value(field, value, t)
	       else [] @ filter_field_value(field, value, t)
		    handle Empty => [])


(* Problem 16: Use `filter_field_value` to find all incidents that
   occurred at "43XX BLOCK OF UNIVERSITY WAY NE". *)
val fourty_third_and_the_ave_reports : json list =
    filter_field_value("hundred_block_location", "43XX BLOCK OF UNIVERSITY WAY NE", unArray(large_incident_reports))


(* Problem 17: Compute a histogram of "event_clearance_description" on
   fourty_third_and_the_ave_reports. *)
val fourty_third_and_the_ave_event_description_histogram : (string * int) list =
     histogram_for_field("event_clearance_description", fourty_third_and_the_ave_reports)


(* Problem 18: Use `filter_field_value` to find all incidents that
   occurred at "45XX BLOCK OF 19TH AVE NE". *)
val nineteenth_and_fourty_fifth_reports : json list =
    filter_field_value("hundred_block_location", "45XX BLOCK OF 19TH AVE NE", unArray(large_incident_reports))


(* Problem 19: Compute a histogram of "event_clearance_description" on
   nineteenth_and_fourty_fifth_reports. *)
val nineteenth_and_fourty_fifth_event_description_histogram : (string * int) list =
    histogram_for_field("event_clearance_description", nineteenth_and_fourty_fifth_reports)


(* Open-ended challenge: further analyze the given data by
   writing more SML functions. Briefly describe the functions you
   wrote. If you find anything in the data that you think is
   interesting, also briefly write that up. *)


(*** Section 2 (Required): Printing JSON values ***)

(* Problem 20: Write a function that takes a separator string and a
   list of strings, and returns the string that consists of all the
   strings in the list concatenated together, separated by the
   separator.

   Hint: use the ^ operator to concatenate two strings,
      eg: "hello" ^ "world" evaluates to "helloworld" *)
fun concat_with (sep : string, ss : string list) : string =
  case ss of
      [] => ""
    | [h] => h
    | h::t => h ^ sep ^ concat_with(sep, t)

(* Problem 21: Write a function `quote_string` that takes a string adds
   double quotes to the beginning and end.

   Hint: to put a double quote inside an SML string literal, you need
         to escape it with a backslash, just like in Java. *)
fun quote_string (s : string) : string =
  "\"" ^ s ^ "\""


(* Problem 22: Write a function `json_to_string` that takes an abstract
   representation of a JSON value and converts it to its concrete
   string encoding, using the syntax described at the top of this file.

   Hint: Use the function Real.toString to help converting numbers to
      strings. Real.toString prints negative numbers with a tilde (~)
      instead of a minus sign (-), which is technically not allowed by
      JSON, but it is okay for this assignment. You are also welcome
      to figure out a way to print a minus sign instead, but it is not
      required.

   Hint: Use quote_string to help with converting strings to
      strings(!). In a real implementation of JSON, we would also need
      to handle escape sequences like \n, \\, and \", but let's ignore
      those for now. (It will be a challenge problem below.)

   Hint: For arrays, write a helper function that loops over the json list and
      computes a string list, then use concat_with

   Hint: For objects, write a helper function that loops over the
      (string * json) list and computes a string list, then use concat_with. *)
fun json_to_string (j : json) : string =
  let
      fun list_to_string (js : json list) : string list =
	case js of
	    [] => []
	  | h::t  =>  json_to_string(h) :: list_to_string(t)

      fun object_to_string (objs : (string * json) list) : string list =
	case objs of
	    [] => []
	  | (x,y)::t  => (quote_string(x) ^ " : " ^ json_to_string(y)) :: object_to_string(t)
  in
      case j of 
	  String(s) => quote_string(s)
        | Num (n) => (Real.toString(n))		
	| False => "false"
	| True => "true"
	| Null => "null"
	| Array(a) => "[" ^ concat_with(", ", list_to_string(a)) ^ "]"
	| Object(obj) => "{" ^ concat_with(", ", object_to_string(obj)) ^ "}"
  end
      
		    
      


(*** Section 3 (Challenge): parsing JSON values ***)

(* Section 1 used pre-parsed JSON data, but in this section we will
   implement a parser for JSON. The instructor's solution to this
   section was used to create the pre-parsed data used earlier, and
   will enable you to go off and explore other datasets in SML.

   This section is hard, but rewarding. Getting comfortable with
   parsing and with the functional programming ideas used to implement
   it will serve you well in the long run. I encourage everyone to try
   at least a few of the tokenization problems below, even if you
   don't have time to finish the whole parser. *)

(* Parsing is the problem of converting a textual representation into
   a structured representation. For example, turning
       { "foo" : 3.14, "bar" : [true, false] }
   into the corresponding json value. Parsers are also used in compilers
   to convert source code into a suitable internal representation.

   Parsing a very well-studied problem in computer science. If you
   ever get a chance to take a compilers class, you will probably
   spend at least a week or two talking about different parsing
   algorithms. It is easy to get the impression that parsing is so
   hard that only experts should attempt it. This is not the
   case. Certain parsing techniques are actually very straightforward
   to implement, especially in a functional programming language such
   as SML. *)

(* Our goal is to turn a string into a json value. We will divide the
   problem into two steps, called tokenization and parsing.

   The purpose of tokenization is to make the parsing problem easier
   by grouping related characters of the input together.

   Tokenization will turn the string into a list of "tokens", which
   are pieces of the input that "go together". It may help to think of
   tokens as the "words" or "indivisible atoms" of the input.

   For example, tokenizing the string
       { "foo" : 3.14, "bar" : [true, false] }
   will result in something like the list
       [LBrace, StrLit "foo", Colon, NumLit "3.14", Comma,
        StringLit "bar", Colon, LBracket, TrueTok, Comma,
        FalseTok, RBracket, RBrace]
   Notice how the characters representing field names have been grouped
   together, as have those representing the number 3.14. True and False
   have been represented specially, as has all punctuation. Also, all
   whitespace has been removed.

   We will generally work with lists of characters (char lists)
   instead of strings, so that we can use our favorite tools of
   recursion and pattern matching to write the tokenizer. We will use
   the standard library functions
       String.explode : string -> char list
   and
       String.implode : char list -> string
   to convert between the two representations.

   Character literals in SML are written as string literals with a
   hashtag in front of the opening double quote: #"a" represents the
   character 'a'.

   Our tokens will be represented by the following datatype.
*)

datatype token =
           NumLit of string (* eg, number 3.14 represented as "3.14" *)
         | StringLit of string (* eg, "foo" *)
         | FalseTok (* false *)
         | TrueTok (* true *)
         | NullTok (* null *)
         | LBrace (* { *)
         | RBrace (* } *)
         | LBracket (* [ *)
         | RBracket (* ] *)
         | Comma (* , *)
         | Colon (* : *)

(* For debugging purposes, it will be convenient to be able to print
   tokens. Here is a freebie function to convert tokens to strings. *)
fun token_to_string (t : token) : string =
  case t of
     NumLit s =>  s
  | StringLit s => quote_string s
  | FalseTok => "false"
  | TrueTok => "true"
  | NullTok => "null"
  | LBrace => "{"
  | RBrace => "}"
  | LBracket => "["
  | RBracket => "]"
  | Comma => ","
  | Colon => ":"

(* Here's another freebie function to report lexical errors conveniently.
   ("Lexical analysis" is another name for "tokenization".) *)
fun lexical_error msg = raise Fail ("Lexical error: " ^ msg)

(* The general idea of the tokenizer will be to examine the beginning
   of the character list to find the next token, "consume" it, and
   then continue to process the rest of the list. Here, "consuming"
   just means to process the first few characters of the list, and
   return the rest of them for later processing.

   In other words, a function to consume a foo will have type
       char list -> foo * char list
   that is, it takes the char list, and converts the first bit of it
   into a foo, and returns the remainder of the list.

   We will write three consumers, one for string literals, one for
   keywords (false, true, and null), and one for numeric literals.
 *)

(* Challenge Problem 23: Write a consumer for string literals. String literals
   are required to be quoted in JSON, so your consumer should look for
   a double quote, and then keep consuming characters until it sees
   the closing double quote.

   Call `lexical_error` with an appropriate message if there is
   no string literal at the beginning of the given character list.

   Hint: After accepting the opening double quote, use a helper
         function to recursively look for the closing double quote,
         returning the string of characters in between, and the remainder.
*)
fun consume_string_literal (cs : char list) : string * char list =
  (* Challenge TODO, about 11 lines *) raise Fail "consume_string_literal unimplemented"


(* Challenge Problem 24: Write a consumer for the keywords true, false, and null.

   Call `lexical_error` with an appropriate message if the given
   character list does not start with a keyword.

   Hint: One way to do this is to use pattern matching to look for
         the sequence of characters for each keyword.

   Hint: Another (in our opinion, cleaner) way is to write a helper
         consumer that looks for an *arbitrary* sequence of alphabetic
         characters, and then to use `assoc` from above to convert the
         string to a token using a lookup table such as
             [("true", TrueTok), ("false", FalseTok), ("null", NullTok)].

         (This takes advantage of the fact that assoc has a relatively
         general type, allowing the second components of the pairs to have
         *any* type whatsoever.)

         Remember to handle the Empty exception from assoc and convert
         it to a lexical error.

         You can check whether a character `c` is alphabetic using the
         standard library function
             Char.isAlpha : char -> bool

   Either of the above strategies will receive full credit.
*)
fun consume_keyword (cs : char list) : token * char list =
  (* Challenge TODO, about 15 lines to do it the "clean" way, or about
     5 lines to do it the "ugly" way. *)
  raise Fail "consume_keyword unimplemented"


(* Here's a freebie consumer for numbers, since it's a bit complex.
   You shouldn't need to understand this code unless you want to.

   JSON uses a fairly common textual format for floating point numbers.
   The format described in Section 6 of RFC 7159, which we summarize here.

   A number consists of an optional minus sign, followed by an integer
   part, followed by an optional fractional part, followed by an
   optional exponent part.

       number = [ '-' ] int [ frac ] [ exp ]

   where single quotes enclose literal characters and square brackets
   denote optional components.

   The integer part is a nonempty sequence of digits, which may only
   start with a 0 if the entire integer part is 0.

       int = '0'
           | nonzero-digit digit*

       nonzero-digit = '1' | '2' | ... | '9'

       digit = '0' | nonzero-digit

   where vertical bars (|) denote alternatives and stars ( * ) denote
   0-or-more repetitions.

   The fractional part is a period followed by one or more digits.

       frac = '.' digit+

   where plus (+) denotes 1-or-more repetitions.

   The exponent part is a letter E (lowercase or uppercase), followed
   by an optional sign (minus or plus), followed by one or more
   digits.

       exp = e [ sign ] digit+

       e = 'e' | 'E'

       sign = '-' | '+'

   We structure consume_num in terms of several "helper consumers"
   that consume the various parts described above, such as the
   optional leading minus sign, or the fractional part, and so on.
*)
fun consume_num (cs : char list) : (string * char list) =
  let
    (* Consume an optional minus sign. We support "-" or "~" for
       compatibility with SML's Real.toString. *)
    fun consume_minus (#"-" :: cs) = ([#"-"], cs)
      | consume_minus (#"~" :: cs) = ([#"~"], cs)
      | consume_minus cs = ([], cs)

    fun consume_exp_sign (#"-" :: cs) = ([#"-"], cs)
      | consume_exp_sign (#"+" :: cs) = ([#"+"], cs)
      | consume_exp_sign cs = ([], cs)

    (* Consume a possibly empty list of digits. *)
    fun consume_digit_list cs =
      let
        fun loop acc [] = (List.rev acc, [])
          | loop acc (c :: cs) =
            if Char.isDigit c
            then loop (c :: acc) cs
            else (List.rev acc, c :: cs)
      in
        loop [] cs
      end

    fun consume_frac (#"." :: cs) =
      let
        val (l, cs) = consume_digit_list cs
      in
        (#"." :: l, cs)
      end
      | consume_frac cs = ([], cs)

    fun consume_exp (c :: cs) =
      if c = #"e" orelse c = #"E"
      then
        let
          val (sign, cs) = consume_exp_sign cs
          val (l, cs) = consume_digit_list cs
        in
          (c :: sign @ l, cs)
        end
      else ([], c :: cs)
      | consume_exp [] = ([], cs)

    val (minus, cs) = consume_minus cs
    val (int, cs) = consume_digit_list cs
    val (frac, cs) = consume_frac cs
    val (exp, cs) = consume_exp cs
  in
    (String.implode (minus @ int @ frac @ exp), cs)
  end


(* We now have all the consumers we need to write the main tokenizer loop. *)

(* Challenge Problem 25: Complete the following definition of `tokenize_char_list`
   that implements the tokenizer.

   Call `lexical_error` with an appropriate message if you encounter an
   unexpected character. (Use Char.toString to get a printed
   representation of a character.)

   Hint: You'll need to have one branch per kind of token, plus a few
         more to skip whitespace.

   Hint: Use the consumers from above.

   Hint: Remember to look for whitespace so that you can correctly ignore it.
 *)
fun tokenize_char_list (cs : char list) : token list =
  case cs of
     [] => []
   | #"\n" :: cs => tokenize_char_list cs (* ignore newlines *)
   | #"{" :: cs => LBrace :: tokenize_char_list cs
   (* Challenge TODO, about 7 lines: several more cases here. *)
   | c :: cs =>
     if Char.isDigit c orelse c = #"-" orelse c = #"~"
     then
       let
         val (s, cs) = consume_num (c :: cs)
       in
         NumLit s :: tokenize_char_list cs
       end

     (* Challenge TODO, about 15 lines: check for string literals and keywords here *)

     else lexical_error ("Unknown character " ^ Char.toString c)


(* Challenge Problem 26: Write the top level tokenizer that takes a string,
   converts it to a char list, and passes it to `tokenize_char_list`.

   Hint: use String.explode and tokenize_char_list *)
fun tokenize (s : string) : token list =
  (* Challenge TODO, 1 line *) raise Fail "tokenize unimplemented"



(* The tokenizer produces a list of tokens, which we now need to
   actually parse into a json value. We will structure our parser in a
   very similar way to the "consumers" used above in the tokenizer,
   except that parsers will work on token lists instead of char lists.

   For example, to parse a foo, we will write a function
       parse_foo : token list -> foo * token list
   which examines the beginning of the token list and converts it to a foo,
   returning the remainder of the list.

   As a very simple (but still useful below) example, we can write a
   parser that consumes a string literal at the beginning of the token
   list. *)

(* First, here's a freebie function to report a syntax error. It takes
   the current token list and a message, and raises an exception. It
   uses the token list to print out the current token (or "EOF"
   standing for "end of file" if there are no tokens left), which
   helps when debugging to know where in the token list the error
   occurred. *)
fun syntax_error (ts : token list, msg : string) =
  let
    val tokenName =
      case ts of
         [] => "EOF"
       | t :: _ => token_to_string t
  in
    raise Fail ("Syntax error at " ^ tokenName ^ ": " ^ msg)
  end


(* Challenge Problem 27: write a `parse_string` function that consumes a string
   literal at the beginning of the token list and returns it.

   If there is no string literal at the beginning of the token list,
   call `syntax_error` with the token list and an appropriate message.
*)
fun parse_string (ts : token list) : string * token list =
  (* Challenge TODO, about 3 lines *) raise Fail "parse_string unimplemented"


(* It is often useful to consume a single token from the token list
   and throw it away, returning the rest of the tokens and throwing an
   error if the token was not there. *)

(* Challenge Problem 28: write a function `expect` which consumes a single,
   specific token from the token list.

   If the token is not there as expected, call lexical_error with an
   appropriate message. *)
fun expect (t : token, ts : token list) : token list =
  (* Challenge TODO, about 6 lines *) raise Fail "expect unimplemented"


(* We're now ready to start writing a `parse_json` function, which
   will contain several local helper functions. In this case, it is
   important that these helper functions be local and not at the top
   level, because they need to recursively call `parse_json`.

   This also makes these functions much more difficult to test, since
   they are not available at the top level scope. (There are ways
   around this, eg, using mutual recursion instead of nested
   functions.) So you'll just need to test `parse_json` extra
   thoroughly to ensure each local helper function is working properly.

   You may want to skip the helper function problems at first and
   work on the main body of parse_json (after the let...in), where
   you can parse everything except objects and arrays, and then come
   back to the helper functions later to complete the function.
*)

fun parse_json (ts : token list) : json * token list =
  let
    (* Challenge Problem 29: write a `parse_field_value` function that parses one
       field-value pair in an object.

       The syntax for a field-value pair is a string literal,
       representing the field name, followed by a colon, followed by
       an arbitrary json value.

       Hint: use `parse_string` for the field name, `expect` for the
             colon, and a recursive call to `parse_json` for the value. *)
    fun parse_field_value (ts : token list) : (string * json) * token list =
      (* Challenge TODO, about 7 lines *) raise Fail "parse_field_value unimplemented"


    (* Challenge Problem 30: write a function `parse_field_value_list` that
       parses a possibly empty comma-separated list of field-value
       pairs, terminated by a closing brace. (This will be used below
       to parse strings representing objects, which are always
       surrounded in braces.)

       Hint: use parse_field_value to parse each field-value pair.

       Hint: First check to see if the first token is a closing
             brace. If so, immediately return the empty list.
             Otherwise, parse a field-value pair and then check
             whether the next token is a comma. If so, consume it an
             recursively parse a list of field-value pairs, and then
             cons the new field-value pair on the front. If it is not a comma,
             immediately return a singleton list.
     *)
    fun parse_field_value_list (ts : token list) : (string * json) list * token list =
      (* Challenge TODO, about 15 lines *) raise Fail "parse_field_value_list unimplemented"


    (* Challenge Problem 31: Write a function `parse_array_element_list` that
       parses a possibly empty comma-separated list of json values,
       terminated by a closing square bracket.

       Hint: this is very similar to `parse_field_value_list`, except
             that it calls `parse_json` instead of
             `parse_field_value`, and uses square brackets instead of
             curly braces.
     *)
    fun parse_array_element_list (ts : token list) : json list * token list =
      (* Challenge TODO, about 15 lines *) raise Fail "parse_array_element_list unimplemented"

  in
    (* Challenge Problem 32: complete the definition of `parse_json` by adding
       branches to the pattern match below.

       If the beginning of the token list does not represent a json value,
       call `syntax_error` with an appropriate message.

       Hint: Very little new code needs to be written in each branch.
             Call the helper functions above as appropriate.
     *)
    case ts of
       NumLit s :: ts =>
       let
         val SOME r = Real.fromString s
       in
         (Num r, ts)
       end
     (* Challenge TODO, about 18 lines: more cases here *)

     | _ => syntax_error (ts, "expecting json")

  end

(* Here is a freebie function to parse a .json file. Give it a
   file name (eg, "small_police.json") *)
fun parse_from_file (file_name : string) : json =
  let
    val file = TextIO.openIn file_name
    val input = TextIO.inputAll file
    val (j, []) = parse_json (tokenize input)
  in
    j
  end

(* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important because it tells SML
            that the previous binding has finished *)
Control.Print.printDepth := 3;
Control.Print.printLength := 3;

(* Uncomment once parser is implemented *)
(*
val small_example = parse_from_file "small_police.json"
val medium_example = parse_from_file "medium_police.json"
val large_example = parse_from_file "large_police.json"
val all_data = parse_from_file "complete_police.json"
*)
;

(* Now make SML print more so that we can see what we're working with. *)
Control.Print.printDepth := 20;
Control.Print.printLength := 20;


(* Open-ended challenge problem: use your parser to explore some other
   data than the ones we have provided. If you find something
   interesting, briefly write it up.

   One thing you may run into is that your parser does not handle all
   of JSON, so it may fail to parse some files. For example, we have
   not implemented support for escape sequences inside of string
   literals, which are fairly frequently used.

   If you run into any other incompatabilities, please mention them
   here, and for extra special bonus points, fix them. *)

(* Challenge Problem 33: implement parser support for escape sequences
   such as \\, \", and \n inside of string literals. This should
   require only changing consume_string_literal. The full list of
   string escapes in JSON can be found in Section 7 of RFC 7159.

   Also implement support for *printing* strings with escape
   sequences. This should only require changing quote_string. *)
