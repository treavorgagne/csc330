datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

fun is_empty_set s =
    case s of
      EmptySet s => true
    | Set s => false

fun min_in_set s =
    case s of 
        EmptySet s => raise SetIsEmpty
    |   Set s => s

fun max_in_set s =
    case s of 
        EmptySet s => raise SetIsEmpty
    |   Set s => s

fun insert_into_set(s,v) =
    s

fun in_set(s, v) =
    true

fun union_set(s, t) =
    s

fun intersect_set(s, t) =
    s

fun except_set(s, t) =
    s

fun remove_from_set(s,v) =
    s
    
fun size_set(s: 'a set) =
    case s of 
        EmptySet s => 0
    |   Set s => size_set(Set s) + 1

fun equal_set(s, t) =
    false

fun is_subset_of(s, t) =
    false
        
fun list_to_set(lst, f) =
    EmptySet f

fun set_to_list s =
    []

fun str_set (s, fstr) =
    "{}"
      
fun map_set (s, fcomp, f) =
    EmptySet fcomp

