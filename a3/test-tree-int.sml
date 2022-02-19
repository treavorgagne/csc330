datatype tree = emptyTree |
                nodeTree of int * tree * tree

exception NotFound

infix 9 ++
infix 9 --
infix 7 == 

(* write your tree functions here *)

(* 
  Insert into tree in order. If the node is duplicated, insert to the left.
  This is a recursive function.
*)
(* val tree_insert_in_order : tree * int -> tree *)
(* complete *)
fun tree_insert_in_order(t, v) =
	case t of
    	emptyTree => nodeTree(v, emptyTree, emptyTree)
    | 	nodeTree(root_val, node_left, node_right) => 	case Int.compare(v, root_val) of
															GREATER => nodeTree(root_val, node_left, tree_insert_in_order(node_right, v))
														|	LESS => nodeTree(root_val, tree_insert_in_order(node_left, v), node_right)
														|	EQUAL => nodeTree(root_val, tree_insert_in_order(node_left, v), node_right)
														
(* Returns height of the tree. Return max height of tree. 0 for EmptyTree. Use Recursion *)
(* val tree_height : tree -> int *)
(* complete *)
fun tree_height t = 
	case t of
		emptyTree => 0
	| 	nodeTree(root_val, node_left, node_right) => 1 + Int.max(tree_height node_left, tree_height node_right)

(* 
  Write "fold" function f to traverse tree in preorder (node, left child, then right child)
  "folding" the tree using the function f. Using acc as the starting value.
*)
(* val tree_fold_pre_order : (int * ’a -> ’a) -> ’a -> tree -> ’a *)
(* complete *)
fun tree_fold_pre_order f acc t =
	case t of
		emptyTree => acc
	| 	nodeTree(root_val, node_left, node_right) => tree_fold_pre_order f (tree_fold_pre_order f (f(root_val,acc)) node_left) node_right 

(* 
  Find the maximum value in the tree (returns an option). Use a val expression and 
  and tree_fold_pre_order to write this function.
*)
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

(* 
  	tree_delete(t,v). First, find the node to delete (if v appears more than once, find the first in-
	stance). If v does not exist, raise NotFound. Second, if the node to delete has one child only, then
	simply delete the node and reconnect the child to the parent; otherwise, find the maximum node in the
	left child, remove it from this subtree, and create a note to replace the one you are deleting (with the
	new subtree as its left child). Use tree_max and a recursive call to tree_delete.
*)
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


(* Tree to list in pre-order. Using val and tree_fold_pre_order. *)
(* val tree_to_list : tree -> int list *)
(* complete *)
val tree_to_list = 
	let 
		fun to_list(root_val, list_acc) = list_acc@[root_val]
	in
		tree_fold_pre_order to_list []
	end

(* Filter all elements of the tree. *)
(* Use tree_delete. *)
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

(* 
  Using val expression tree_filter and tree_fold_pre_order, and function composition to
  to write a function that sums the nodes that are even. Uses mod. Uses function composition 
  and val expression to define func.
*)
(* val tree_sum_even : tree -> int *)
(* complete *)
val tree_sum_even = 
	let 
		fun sum_even(root_val, sum_acc) = sum_acc + root_val
		
		fun elim_odd t = 
			case t of
				emptyTree => t
			|	nodeTree(root_val, node_left, node_right) => (case ((root_val mod 2) = 0) of
																	true => nodeTree(root_val, elim_odd node_left, elim_odd node_right)
																|	false => elim_odd (tree_delete(t,root_val)) 
															  ) 
	in
		tree_fold_pre_order sum_even 0 o elim_odd
	end

(*  *)
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

fun t ++ v = tree_insert_in_order(t, v)
fun t -- v = tree_delete(t, v)
fun t1 == t2 = tree_equal t1 t2

val empty = emptyTree
val e = emptyTree ++ 6 ++ 2 ++ 10 ++ 4 ++ 8 ++ 1 ++ 3 ++ 5 ++ 7
val tt = emptyTree ++ 14 ++ 32 ++ 51 ++ 25 ++ 41 ++ 10 ++ 33 ++ 66 ++ 1 ++ 34
val ttt = emptyTree ++ 4 ++ 2 ++ 1 ++ 5 ++ 1 ++ 10 ++ 3 ++ 7 ++ 1
val neg = emptyTree ++ ~4 ++ ~2 ++ ~1 ++ ~5 ++ ~1 ++ ~10 ++ ~3 ++ ~7 ++ ~1
val ot1 = emptyTree ++ 2 ++ 1
val ot2 = emptyTree ++ 1 ++ 2

val deleteTest = emptyTree ++ 10 ++ 4 ++ 18 ++ 1 ++ 6 ++ 12 ++ 24

val test_tree_fold =
    ("T4. test tree_fold_pre_order", [
      (tree_fold_pre_order (op +) 0 ttt) = 34
    ])

val lempty = tree_to_list empty
val ltt = tree_to_list tt
val lttt = tree_to_list ttt

val hempty = tree_height empty
val htt = tree_height tt
val httt = tree_height ttt

val maxempty = tree_max empty
val maxtt = tree_max tt
val maxttt = tree_max ttt
val maxneg = tree_max neg

val sum_e = tree_sum_even e
val sum_empty = tree_sum_even empty
val sum_tt = tree_sum_even tt
val sum_ttt = tree_sum_even ttt

val lot = tree_to_list ot1

val test1 = ot1 -- 1
val s = tree_to_list test1

val test2 = ot2 -- 2
val s2 = tree_to_list test2

val exceptionTest = ot1 -- 1 handle NotFound => ot1
val exceptionTest1 = empty -- 1 handle NotFound => emptyTree

val d_list1 = tree_to_list(deleteTest)
val delete = deleteTest -- 10
val d_list2 = tree_to_list(delete)

val filt1 = tree_filter (fn x => x <= 32) tt
val filt2 = tree_filter (fn x => x > 32) tt
val f1_list = tree_to_list(filt1)
val f2_list = tree_to_list(filt2)