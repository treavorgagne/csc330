datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype value = Const of int
	       | Unit
	       | Tuple of value list
	       | Constructor of string * value

exception NoAnswer

(* start *)

(* 
    Write a function first_answer f lst that has type (’a -> ’b option) -> ’a list ->
    ’b (notice that the 2 arguments are curried. f should be applied to elements lst in order, until
    the first time f returns SOME v for some v; in that case v is the result of the function. If f returns
    NONE for all list elements, then first_answer should raise the exception NoAnswer. 
*)
fun first_answer f lst = NONE

(* 
    Write a function all_answer f lst of type (’a -> ’b list option) -> ’a list -> ’b
    list option (notice the 2 arguments are curried). Like first_answer. f should be applied
    to elements of the second argument. If it returns NONE for any element, then all_answer returns
    NONE. Otherwise the calls to f will have produced SOME lst1, SOME lst2, ... SOME lstn
    and the result of all_answer is SOME lst where lst is lst1, lst2, ..., lstn (the order in
    the result list should be preserved). Note that all_answer f [] should return SOME [] for any f.
*)
fun all_answer f lst = NONE

(* 
    Write a function check_pattern that takes a pattern and returns true if and only if all the variables
    appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
    names are not relevant. Hints: use two helper functions. The first takes a pattern and returns a list of
    all the strings it uses for variables (use foldl with a function that uses append). The second helper
    function a list of strings and decides if it has repeats (List.exists may be useful)
*)
fun check_pattern = false

(* 
    Write a function match that takes a value * pattern and returns a (string * value) list
    option, namely NONE if the pattern does not match and SOME lst where lst is the list of bindings
    if it does. Note that if the value matches but the pattern has no patterns of the form Variable s,
    then the result is SOME []. Remember to look above for the rules for what patterns match what
    values, and what bindings they produce. Hints: use a case expression with 7 branches (one per
    rule). The branch for tuples uses all_answer and ListPair.zip. 
*)
fun match = NONE

(* 
    Write a function first_match that takes a value and a list of patterns and returns a (string *
    value) list option, namely NONE if no pattern in the list matches or SOME lst where lst is
    the list of bindings for the first pattern in the list that matches. Use first_answer and a handle-
    expression. Notice that the 2 arguments are curried. 
*)
fun first_match = NONE

(* end *)

