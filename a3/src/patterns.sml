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

(* 
  Insert into tree in order. If the node is duplicated, insert to the left.
  This is a recursive function.
*)
(* val tree_insert_in_order : tree * int -> tree *)
fun tree_insert_in_order(t, v) = t

(* 
  Delete node with value v. If v does not exist, raise NotFound. Else rearrange tree 
  based on the deletion of the node. Uses tree_max and a recursive call to tree_delete.
*)
(* val tree_delete : tree * int -> tree *)
fun tree_delete(t,v) = t

(* Returns height of the tree. Return max height of tree. 0 for EmptyTree. Use Recursion *)
(* val tree_height : tree -> int *)
fun tree_height t = 0

(* 
  Write "fold" function f to traverse tree in preorder (node, left child, then right child)
  "folding" the tree using the function f. Using acc as the starting value.
*)
(* val tree_fold_pre_order : (int * ’a -> ’a) -> ’a -> tree -> ’a *)
fun tree_fold_pre_order f acc t = t

(* 
  Find the maximum value in the tree (returns an option). Use a val expression and 
  and tree_fold_pre_order to write this function.
 *)
(* val tree_max : tree -> int option *)
fun tree_max t = 0

(* Tree to list in pre-order. Using val and tree_fold_pre_order. *)
(* val tree_to_list : tree -> int list *)
fun tree_to_list t = []

(* Filter all elements of the tree. *)
(* val tree_filter : (int -> bool) -> tree -> tree *)
fun tree_filter f t = t

(* 
  Using val expression tree_filter and tree_fold_pre_order, and function composition to
  to write a function that sums the nodes that are even. Uses mod. Uses function composition 
  and val expression to define func.
*)
(* val tree_sum_even : tree -> int *)
fun tree_sum_even t = 0


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

