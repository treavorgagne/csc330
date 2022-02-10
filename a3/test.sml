
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

(* 
  Insert into tree in order. If the node is duplicated, insert to the left.
  This is a recursive function.
*)
(* val tree_insert_in_order : tree * int -> tree *)
(* complete *)
fun tree_insert_in_order(t, v) =
	case t of
    	emptyTree => nodeTree(v, emptyTree, emptyTree)
    | 	nodeTree(root_val, node_left, node_right) => 	let 
															val x = Int.compare(v, root_val)
														in
															case x of
																GREATER => nodeTree(root_val, node_left, tree_insert_in_order(node_right, v))
															|	LESS => nodeTree(root_val, tree_insert_in_order(node_left, v), node_right)
															|	EQUAL => nodeTree(root_val, tree_insert_in_order(node_left, v), node_right)
														end 

(* 
  Delete node with value v. If v does not exist, raise NotFound. Else rearrange tree 
  based on the deletion of the node. Uses tree_max and a recursive call to tree_delete.
*)
(* val tree_delete : tree * int -> tree *)
(* incomplete *)
fun tree_delete(t,v) = 
	case (t,v) of 
		(emptyTree, 0) => emptyTree
	|	(nodeTree(root_val, node_left, node_right), 0) => t

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
(* incomplete *)
fun tree_fold_pre_order f acc t = t

(* 
  Find the maximum value in the tree (returns an option). Use a val expression and 
  and tree_fold_pre_order to write this function.
 *)
(* val tree_max : tree -> int option *)
(* incomplete *)
fun tree_max t = 
	case t of 
		emptyTree => NONE
    | 	nodeTree(root_val, node_left, node_right) => SOME(root_val)

(* Tree to list in pre-order. Using val and tree_fold_pre_order. *)
(* val tree_to_list : tree -> int list *)
(* complete *)
fun tree_to_list t =
	case t of
    	emptyTree => []
    | 	nodeTree(root_val, node_left, node_right) => [root_val] @ tree_to_list node_left @ tree_to_list node_right 

(* Filter all elements of the tree. *)
(* Use tree_delete. *)
(* val tree_filter : (int -> bool) -> tree -> tree *)
(* complete once tree_delete is done *)
fun tree_filter f t =
	case t of 
		emptyTree => emptyTree
    | 	nodeTree(root_val, node_left, node_right) => 	let 
															val x = f root_val
														in
															case x of 
																true => nodeTree(	
																					root_val, 
																					tree_filter f node_left, 
																					tree_filter f node_right
																				)
															| 	false =>	let 
																				val node = tree_delete(t, root_val)
																			in 
																				tree_filter f node 
																			end 
														end

(* 
  Using val expression tree_filter and tree_fold_pre_order, and function composition to
  to write a function that sums the nodes that are even. Uses mod. Uses function composition 
  and val expression to define func.
*)
(* val tree_sum_even : tree -> int *)
(* incomplete *)
fun tree_sum_even t = 
	case t of 
		emptyTree => 0
    | 	nodeTree(root_val, node_left, node_right) => 1