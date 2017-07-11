(* Tests for unString *)
val "hello" = unString (String "hello")

(* Tests for sub *)
val String "world" = sub (my_array, 1)

(* Tests for assoc. You'll want a few more. *)
val False = assoc ("foo", [("foo", False)]);
val 3 =  assoc ("match", [("foo", 1),("fo", 2), ("match", 3)]);
val Num 3.14 = assoc("this", [("this", Num 3.14)]);
val True = assoc("test3", [("test1", False), ("test2", Num 3.33), ("test3", True)]);
val String "tej" = assoc("abc", [("cab", True), ("abc", String "tej")]);

(* Tests for dot. *)
val True = dot (my_obj, "ok")

(* Tests for fields. *)
val ["foo","bar","ok"] = fields my_obj

(* Tests for recursive_fields *)
val ["x","y","x"] =
  recursive_fields (Array [Object [("x", String "foo")],
                           Object [("y", String "foo")],
                           Object [("x", String "bar")]])

val ["name", "position", "x", "y"] =
  recursive_fields (Object [("name", String "James"),
                            ("position", Object [("x", Num 3.14),
                                                 ("y", Num 2.718)])])

(* Tests for deduped_recursive_fields *)
val ["x","y"] =
  deduped_recursive_fields (Array [Object [("x", String "foo")],
                                   Object [("y", String "foo")],
                                   Object [("x", String "bar")]])


(* Tests for count_occurrences.
   Your implementation may return things in a different order! *)
val [("b",1),("a",2)] = count_occurrences ["a", "a", "b"]

(* Tests for `values_for_field` *)
val ["foo","bar"] =
  values_for_field ("x", [Object [("x", String "foo")],
                          Object [("x", String "bar")]])

(* Tests for `unique_values_for_field` *)
val ["bar", "foo"] =
  unique_values_for_field ("x", [Object [("x", String "foo")],
                                 Object [("x", String "foo")],
                                 Object [("x", String "bar")]])

(* Tests for `histogram_for_field` *)
val [("foo",2),("bar",1)] =
  histogram_for_field ("x", [Object [("x", String "foo")],
                             Object [("x", String "foo")],
                             Object [("x", String "bar")]])

(* Tests for unArray *)
val [True, String "world", Null] = unArray (Array [True, String "world", Null])

(* Tests for `filter_field_value` *)
val [Object [("x",String "foo"),("y",String "bar")],
     Object [("x",String "foo"),("y",String "baz")]] =
  filter_field_value ("x", "foo", [Object [("x", String "foo"), ("y", String "bar")],
                                   Object [("x", String "foo"), ("y", String "baz")],
                                   Object [("x", String "a")],
                                   Object []])



(* Tests for concat_with. Write more tests until you're sure your
   code is right. *)
val "a,b,c" = concat_with (",", ["a", "b", "c"]);
val "a b c" = concat_with(" ", ["a", "b", "c"]);
val "c:s:e" = concat_with(":", ["c", "s", "e"]);

(* Tests for quote_string *)
val "\"foo\"" = quote_string "foo"

(* Tests for json_to_string. You will likely want a few more. If you
   chose to use whitespace differently than we did, that's fine, just
   Change the tests below accordingly. *)
val "3.14" = json_to_string pi
val "\"hello\"" = json_to_string hello
val "false" = json_to_string my_false
val "[1.0, \"world\", null]" = json_to_string my_array
val "{\"foo\" : 3.14, \"bar\" : [1.0, \"world\", null], \"ok\" : true}" = json_to_string my_obj
val "[null, 2.0]" =  json_to_string( Array [Null, Num 2.0] );
val "\"test\"" = json_to_string (String "test");

										 
(* End of tests for required problems. Feel free to comment out the
   challenge tests below.

(* Tests for consume_string_literal *)
val ("foo",[#" ",#":",#" ",#"t",#"r",#"u",#"e"]) =
  consume_string_literal (String.explode "\"foo\" : true")

(* Tests for consume_keyword *)
val (FalseTok, [#" ",#"f",#"o",#"o"]) =
  consume_keyword (String.explode "false foo")

(* Tests for tokenize_char_list. You'll want more. *)
val [LBrace, StringLit "foo", Colon, NumLit "3.14", Comma,
     StringLit "bar", Colon, LBracket, TrueTok, Comma,
     FalseTok, RBracket, RBrace] =
  tokenize_char_list (String.explode "{ \"foo\" : 3.14, \"bar\" : [true, false] }")

(* Tests for parse_string *)
val ("foo", [FalseTok]) =
  parse_string ([StringLit "foo", FalseTok])

(* Tests for expect *)
val [FalseTok] = expect (Colon, [Colon, FalseTok])

(* Tests for parse_json. You'll probably want way more. *)
val (Object [("foo", Null),("bar",Array [True,False])],[]) =
  parse_json (tokenize "{ \"foo\" : null, \"bar\" : [true, false] }")
*)
