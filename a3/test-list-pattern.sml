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
    ’b (notice that the 2 arguments are curried). f should be applied to elements lst in order, until
    the first time f returns SOME v for some v; in that case v is the result of the function. If f returns
    NONE for all list elements, then first_answer should raise the exception NoAnswer. 
*)
fun first_answer f lst = 
    case lst of 
        [] => raise NoAnswer
    |   h::tail =>  case f(h) of
                        SOME v => v
                    |   NONE => first_answer f tail 

(* 
    Write a function all_answer f lst of type (’a -> ’b list option) -> ’a list -> ’b
    list option (notice the 2 arguments are curried). Like first_answer. f should be applied
    to elements of the second argument. If it returns NONE for any element, then all_answer returns
    NONE. Otherwise the calls to f will have produced SOME lst1, SOME lst2, ... SOME lstn
    and the result of all_answer is SOME lst where lst is lst1, lst2, ..., lstn (the order in
    the result list should be preserved). Note that all_answer f [] should return SOME [] for any f.
*)
fun all_answers f lst =
    case lst of
        [] => SOME []
    |   h::tail =>  case (f(h), (all_answers f tail)) of
                        (SOME v, SOME vtail) => SOME(v @ vtail)
                    |   _ => NONE 

(* 
    Write a function check_pattern that takes a pattern and returns true if and only if all the variables
    appearing in the pattern are distinct from each other (i.e., use different strings). The constructor
    names are not relevant. Hints: use two helper functions. The first takes a pattern and returns a list 
    of all the strings it uses for variables (use foldl with a function that uses append). The second helper 
    function a list of strings and decides if it has repeats (List.exists may be useful).
*)
fun check_pattern p = 
    let
        fun list_to_bool list = (* pattern -> string list *)
            case list of 
                [] => true
            |   h::tail =>  if List.exists (fn x => h = x) tail
                            then false 
                            else list_to_bool tail

        fun pat_to_str p = (* string list -> bool *)
            case p of 
                Variable(s) => [s]
            |   TupleP(ps) => ( case ps of 
                                    [] => []
                                |   h::tail => pat_to_str(h) @ pat_to_str(TupleP(tail)) )
            |   ConstructorP(str, pat) => pat_to_str pat
            |   _ => [] (* all patterns with empty list bindings *)
    in
        list_to_bool(pat_to_str(p))
    end 
    
(* 
    Write a function match that takes a value * pattern and returns a (string * value) list
    option, namely NONE if the pattern does not match and SOME lst where lst is the list of bindings
    if it does. Note that if the value matches but the pattern has no patterns of the form Variable s,
    then the result is SOME []. Remember to look above for the rules for what patterns match what
    values, and what bindings they produce. Hints: use a case expression with 7 branches (one per
    rule). The branch for tuples uses all_answer and ListPair.zip. 
*)
fun match(v,p) =
    let 
        fun sub_match(vs,ps) = (* similar to all_answers *)
            case (vs,ps) of
                ([],[]) => SOME []
            |   (vsh::vstail, psh::pstail) => ( case (match(vsh,psh),sub_match(vstail,pstail)) of
                                                    (SOME mh, SOME mtail) => SOME(mh @ mtail)
                                                |   _ => NONE )                 
            |   _ => NONE
    in 
        case (v,p) of 
            (_,Wildcard) => SOME []
        |   (v,Variable(s)) => SOME [(s,v)]
        |   (Unit,UnitP) => SOME []
        |   (Const(i2),ConstP(i1)) => if i1 = i2 then SOME [] else NONE
        |   (Tuple(vs),TupleP(ps)) => sub_match(vs,ps)
        |   (Constructor(s2,value),ConstructorP(s1, pattern)) => if s1 = s2 
                                                                 then 
                                                                     case match(value,pattern) of
                                                                         NONE => NONE
                                                                     |   SOME(bind) => SOME(bind)
                                                                 else NONE
        |   _ => NONE
    end

(* 
    Write a function first_match that takes a value and a list of patterns and returns a (string *
    value) list option, namely NONE if no pattern in the list matches or SOME lst where lst is
    the list of bindings for the first pattern in the list that matches. Use first_answer and a handle-
    expression. Notice that the 2 arguments are curried. 
*)
fun first_match v list_p  =
    SOME ( first_answer (fn p => match(v, p)) list_p ) handle NoAnswer => NONE

(* end *)

val first1 = first_answer (fn x => if String.size(x) = 3 then SOME x else NONE) ["this", "is", "the", "end", "of", "the", "world"]
val first2 = first_answer (fn x => if String.size(x) = 2 then SOME x else NONE) ["this", "is", "the", "end", "of", "the", "world"]
val first3 = first_answer (fn x => if String.size(x) = 5 then SOME x else NONE) ["this", "is", "the", "end", "of", "the", "world"]
val first4 = first_answer (fn x => if String.size(x) = 7 then SOME x else NONE) ["this", "is", "the", "end", "of", "the", "world"] handle NoAnswer => "NoAnswer"
val f = first_answer (fn x => if (x < 0) then SOME x else NONE) [1,2,1,2,1,2] handle NoAnswer => 0

val all1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,2,1,2,1,2]
val all2 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [1,1,1,1,1,1]
val all3 = all_answers (fn x => if x = 1 then SOME [x] else NONE) []

val test_check_pattern =
    ("3. check_pattern", [
      check_pattern (TupleP [Wildcard,Variable "cat",
                       Variable "pp",TupleP[Variable "tt"],
                       Wildcard,ConstP 3,
                       ConstructorP("cony",Variable "pp")]) = false,
      (* Greay tests*)
      check_pattern (TupleP [Wildcard,Variable "cat",
                        Variable "pp",TupleP[Variable "tt"],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",Variable "gg")]) = true,
      check_pattern (TupleP [Wildcard,TupleP[Wildcard],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",UnitP)]) = true,
      check_pattern (TupleP [Wildcard,TupleP[ConstP 3],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",ConstP 5)]) = true,
      check_pattern (TupleP [Wildcard,TupleP[Variable "cat"],
                        Wildcard,ConstP 3,
                        ConstructorP("cony",ConstructorP ("frank", TupleP [Variable "cat"]))]) = false,
     (* Matthew P tests*)
     check_pattern (ConstructorP("hello", TupleP[Variable "tt", TupleP[Variable "tt"]])) = false,
     check_pattern (ConstructorP("hello", TupleP[ConstP 1, TupleP[Wildcard]])) = true,
     check_pattern (Variable "helloWorld") = true,
     check_pattern (Wildcard) = true,
     check_pattern (ConstP 1) = true
    ]);

val test_match =
    ("test_match",
         [match(Unit, UnitP) = SOME [],
          match(Unit, Variable "cat") = SOME [("cat", Unit)],
          match(Tuple [Unit, Const 8], TupleP [Variable "cat", Variable "dog"]) = SOME [("cat", Unit),("dog", Const 8)],
          match(Tuple [Unit, Tuple [Unit, Unit]],
                        TupleP [Variable "cat", TupleP [Variable "dog", Variable "rat"]]) = SOME [("cat", Unit), ("dog", Unit),  ("rat", Unit)],
          match(Tuple[Const 7, Const 6, Unit, Const 7],
                        TupleP[
                          Variable "a",
                          Variable "ab",
                          Variable "abc",
                          Variable "abcd"]) = SOME [("a",Const 7), ("ab",Const 6), ("abc",Unit), ("abcd",Const 7)
          ]
    ]);

val test_first_match =
    ("test_first_match",
         [
           first_match Unit [UnitP] = SOME [],
           first_match Unit [Variable "cat"] = SOME [("cat", Unit)],
           first_match Unit [ConstP 3] = NONE,
           first_match (Tuple []) [TupleP [Wildcard]] = NONE,
           first_match (Tuple [Unit]) [TupleP []] = NONE,
           first_match (Tuple [Unit]) [TupleP [Variable "cat"]] = SOME [("cat", Unit)],
           first_match (Const 7) [Wildcard ] = SOME [],
           first_match (Tuple[Const 7, Const 6, Unit, Const 8])
                               [TupleP[ConstP 7, Variable "cat",Wildcard, ConstP 8]] = SOME [("cat",Const 6)],
           first_match (Tuple[Const 7, Const 6, Unit, Const 7])
                               [TupleP[Variable "a", Variable "ab",
                                       Variable "abc", Variable "abcd"]] = SOME [
              ("a",Const 7),
              ("ab",Const 6),
              ("abc",Unit),
              ("abcd",Const 7)
           ]
    ]);