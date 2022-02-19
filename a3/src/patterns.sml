(*
Your name:  Treavor Gagne
Your student id:  V00890643
*)

structure Patterns =

struct

exception NoAnswer
exception NotFound

datatype tree = emptyTree |
                nodeTree of int * tree * tree

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


(* write your tree functions here *)

(* TREE CODE START *)

(* val tree_insert_in_order : tree * int -> tree *)
(* complete *)
fun tree_insert_in_order(t, v) =
	case t of
    	emptyTree => nodeTree(v, emptyTree, emptyTree)
    | 	nodeTree(root_val, node_left, node_right) => 	case Int.compare(v, root_val) of
															GREATER => nodeTree(root_val, node_left, tree_insert_in_order(node_right, v))
														|	LESS => nodeTree(root_val, tree_insert_in_order(node_left, v), node_right)
														|	EQUAL => nodeTree(root_val, tree_insert_in_order(node_left, v), node_right)

(* val tree_height : tree -> int *)
(* complete *)
fun tree_height t = 
	case t of
		emptyTree => 0
	| 	nodeTree(root_val, node_left, node_right) => 1 + Int.max(tree_height node_left, tree_height node_right)

(* val tree_fold_pre_order : (int * ’a -> ’a) -> ’a -> tree -> ’a *)
(* complete *)
fun tree_fold_pre_order f acc t =
	case t of
		emptyTree => acc
	| 	nodeTree(root_val, node_left, node_right) => tree_fold_pre_order f (tree_fold_pre_order f (f(root_val,acc)) node_left) node_right 

(* val tree_max : tree -> int option *)
(* complete *)
val tree_max = 
	let 
		fun get_max(root_val, acc_max) = 
			case acc_max of 
				SOME max => (case (max < root_val) of 
								true => SOME root_val
							|	false => SOME max)
			|	NONE => SOME root_val 
	in
		tree_fold_pre_order get_max NONE 
	end

(* val tree_delete : tree * int -> tree *)
(* complete  *)
fun tree_delete(t,v) = 
	case t of
		(* case: empty exception must be caught *)
		emptyTree => raise NotFound
		(* case 1 *)
	|	nodeTree(root_val, emptyTree, emptyTree) =>		(case (root_val = v) of
															true => emptyTree
														|	false => t)
		(* case 2 *)
	|	nodeTree(root_val, node_left, emptyTree) => 	(case (root_val = v) of
															true => node_left
														|	false => nodeTree(root_val, tree_delete(node_left, v), emptyTree))
		(* case 2 *)
	|	nodeTree(root_val, emptyTree, node_right) => 	(case (root_val = v) of
															true => node_right
														|	false => nodeTree(root_val, tree_delete(node_right, v), emptyTree))
		(* case 3 *)
	|	nodeTree(root_val, node_left, node_right) => 	(case (root_val = v) of
															true => (let
																		val max_op = tree_max node_left
																		val max = valOf(max_op)
																	in
																		nodeTree(max, tree_delete(node_left,max), node_right)
																	end)
														|	false => nodeTree(root_val, tree_delete(node_left,v), tree_delete(node_right,v)))

(* val tree_to_list : tree -> int list *)
(* complete *)
val tree_to_list = 
	let 
		fun to_list(root_val, list_acc) = list_acc@[root_val]
	in
		tree_fold_pre_order to_list []
	end

(* val tree_filter : (int -> bool) -> tree -> tree *)
(* complete once tree_delete is done *)
fun tree_filter f t =
	case t of 
		emptyTree => emptyTree
    | 	nodeTree(root_val, node_left, node_right) => 	case (f root_val) of 
															true => nodeTree(	
																				root_val, 
																				tree_filter f node_left, 
																				tree_filter f node_right
																			)
														| 	false =>	let 
																			val new_tree = tree_delete(t, root_val)
																		in 
																			tree_filter f new_tree 
																		end 	

(* val tree_sum_even : tree -> int *)
(* complete *)
val tree_sum_even = 
	let 
		fun sum_even(root_val, sum_acc) = 
			if (root_val mod 2) = 0
			then sum_acc + root_val
			else sum_acc
	in
		tree_fold_pre_order sum_even 0
	end

(* TREE CODE END *)
(* PATTERN CODE START *)

(* val fun first_answer: ('a -> 'b option) -> 'a list -> 'b  *)
(* complete *)
fun first_answer f lst = 
    case lst of 
        [] => raise NoAnswer
    |   h::tail =>  case f(h) of
                        SOME v => v
                    |   NONE => first_answer f tail 

(* val all_answers = fn : ('a -> 'b list option) -> 'a list -> 'b list option *)
(* complete *)
fun all_answers f lst =
    case lst of
        [] => SOME []
    |   h::tail =>  case (f(h), (all_answers f tail)) of
                        (SOME v, SOME vtail) => SOME(v @ vtail)
                    |   _ => NONE 

(* val check_pattern = fn : pattern -> bool *)
(* complete *)
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
    
(* val match = fn : value * pattern -> (string * value) list option *)
(* complete *)
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

(* val first_match = fn : value -> pattern list -> (string * value) list option *)
(* complete *)
fun first_match v list_p  =
    SOME ( first_answer (fn p => match(v, p)) list_p ) handle NoAnswer => NONE

(* PATTERN CODE END *)

(* leave the following functions untouched *)

fun tree_root t =
    case t of
        emptyTree => NONE
      | nodeTree (n, _, _) => SOME n

fun tree_equal t1 t2  =
    case (t1, t2) of
        (emptyTree, emptyTree) => true
      | (emptyTree, _) =>  false
      | (_, emptyTree) => false
      | (nodeTree(n1, l1, r1),nodeTree(n2, l2, r2)) =>
        n1 = n2 andalso (tree_equal l1 l2) andalso (tree_equal r1 r2)

infix 9 ++
infix 9 --
infix 7 == 

fun t ++ v = tree_insert_in_order(t, v)
fun t -- v = tree_delete(t, v)
fun t1 == t2 = tree_equal t1 t2

end

