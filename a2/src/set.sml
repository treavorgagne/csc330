(*
Your name:  Treavor Gagne 
Your student id: V00890643
*)

structure Set =
struct
local
  open Csc330
in

datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

infix 1 IDENTICAL
infix 3 EXCEPT
infix 4 IS_SUBSET_OF
infix 5 INTERSECT
infix 6 UNION
infix 7 IN
infix 8 CONTAINS        
infix 9 ++
infix 9 --


(* val is_empty_set = fn : ’a set -> bool *)
(* complete *)
fun is_empty_set s =
    case s of
        EmptySet comp => true
    |   Set(list, comp) => false

(* val min_in_set = fn : ’a set -> ’a *)
(* complete *)
fun min_in_set s =
    let 
        fun find_min(list, min, comp) = 
            case list of 
                [] => min
            |   h::t => let
                            val ord = comp(h, hd(t))
                        in
                            case ord of
                                LESS => find_min(t, h, comp)
                            |   GREATER => find_min(t, min, comp)
                            |   EQUAL => find_min(t, min, comp)
                        end
    in
    case s of 
        EmptySet comp => raise SetIsEmpty
    |   Set(list, comp) => find_min(list, hd(list), comp)
    end

(* val max_in_set = fn : ’a set -> ’a *)
(* complete *)
fun max_in_set s =
    let 
        fun find_max(list, max, comp) = 
            case list of 
                [] => max
            |   h::t => let
                            val ord = comp(h, hd(t))
                        in
                            case ord of
                                LESS => find_max(t, max, comp)
                            |   GREATER => find_max(t, h, comp)
                            |   EQUAL => find_max(t, max, comp)
                        end
    in
    case s of 
        EmptySet comp => raise SetIsEmpty
    |   Set(list, comp) => find_max(list, hd(list), comp)
    end

(* val insert_into_set = fn : ’a set * ’a -> ’a set *)
(* complete *)
(* insertion must be in order *)
fun insert_into_set(s,v) =
    let 
        fun insert(set_list, comp, new_list) = 
            case set_list of 
                [] => Set(new_list@[v], comp)
            |   h::t => let
                            val ord = comp(v, h)
                        in
                            case ord of
                                EQUAL => s
                            |   GREATER => insert(t, comp, new_list@[h]) 
                            |   LESS => Set(new_list@[h]@[v]@t, comp)
                        end
    in 
        case s of
            EmptySet(comp) => Set([v], comp)
        |   Set(list, comp) => insert(list, comp, [])
    end

(* val in_set = fn : ’a set * ’a -> bool *)
(* complete *)
fun in_set(s, v) =
    let 
        fun find_match(list, comp) = 
            case list of 
                [] => false
            |   h::t => let
                            val ord = comp(v, h)
                        in
                            case ord of
                                EQUAL => true
                            |   GREATER => find_match(t,comp)
                            |   LESS => false
                        end
    in
        case s of
            EmptySet(comp) => false
        |   Set(list, comp) => find_match(list, comp)
    end

(* val union_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun union_set(s, t) =
    s

(* val intersect_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun intersect_set(s, t) =
    s

(* val except_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun except_set(s, t) =
    s

(* val remove_from_set = fn : ’a set * ’a -> ’a set *)
(* complete *)
fun remove_from_set(s,v) =
    let 
        fun remove(set_list, comp, new_list) = 
            case set_list of 
                [] => Set(new_list@[v], comp)
            |   h::t => let
                            val ord = comp(v, h)
                        in
                            case ord of
                                EQUAL => Set(new_list@t, comp)
                            |   GREATER => remove(t, comp, new_list@[h])
                            |   LESS => s
                        end
    in 
        case s of
            EmptySet(comp) => Set([], comp)
        |   Set(list, comp) => remove(list, comp, [])
    end
    
(* val size_set = fn : ’a set -> int *)
fun size_set(s: 'a set) =
    let 
        fun get_size(set_list) = 
            case set_list of 
                [] => 0
            |   h::t => get_size(t) + 1
    in
        case s of 
            EmptySet comp => 0
        |   Set(list, comp) => get_size(list)
    end

(* val equal_set = fn : ’a set * ’a set -> bool *)
fun equal_set(s, t) =
    false
    
(* val is_subset_of = fn : ’a set * ’a set -> bool *)
fun is_subset_of(s, t) =
    false
        
(* val list_to_set = fn : ’a list * (’a * ’a -> order) -> ’a set *)
(* complete *)
fun list_to_set(lst, f) =
    case lst of 
        [] => EmptySet f
    |   h::t => Set(lst, f)

(* val set_to_list = fn : ’a set -> ’a list *)
(* complete *)
fun set_to_list s =
    case s of 
        EmptySet comp => []
    |   Set(list, comp) => list

(* val str_set = fn : ’a set * (’a -> string) -> string *)
fun str_set (s, fstr) =
    "{}"

(* val map_set = fn : ’a set * (’b * ’b -> order) * (’a -> ’b) -> ’b set *)
fun map_set (s, fcomp, f) =
    EmptySet fcomp
    
fun s -- v = s
fun s ++ v = s
fun s IDENTICAL t = false
fun s UNION t = s
fun s INTERSECT t = s
fun s EXCEPT t = s
fun v IN s = false
fun s IS_SUBSET_OF t = false

fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
    EQUAL
                          
end
end    
