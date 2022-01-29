datatype 'a set = EmptySet of ('a * 'a -> order) | Set of 'a list * ('a * 'a -> order)

exception SetIsEmpty

(* val is_empty_set = fn : ’a set -> bool *)
(* complete *)
fun is_empty_set s =
    case s of
        EmptySet comp => true
    |   Set(list, comp) => false

(* val min_in_set = fn : ’a set -> ’a *)
(* complete *)
fun min_in_set s =
    case s of 
        EmptySet comp => raise SetIsEmpty
    |   Set(list, comp) =>  case list of 
                                [] => raise SetIsEmpty
                            |   h::t => h

(* val max_in_set = fn : ’a set -> ’a *)
(* complete *)
fun max_in_set s =
    let 
        fun find_max(list, max, comp) = 
            case list of 
                [] => max
            |   h::t => let
                            val ord = comp(h, max)
                        in
                            case ord of
                                LESS => find_max(t, max, comp)
                            |   GREATER => find_max(t, h, comp)
                            |   EQUAL => find_max(t, max, comp)
                        end
    in
    case s of 
        EmptySet comp => raise SetIsEmpty
    |   Set(list, comp) =>  case list of 
                                [] => raise SetIsEmpty
                            |   h::t => find_max(t, h, comp)
    
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

(* val union_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun union_set(s, t) =
    let 
        fun union(s_set_acc, list) = 
        case list of 
            [] => s_set_acc
        |   h::tail =>  let 
                            val new_set = insert_into_set(s_set_acc, h)
                        in 
                            union(new_set, tail)
                        end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => s
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => t
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => union(s, t_list)
    end 

(* val intersect_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun intersect_set(s, t) =
    let 
        fun intersect(t_list, list_acc, s_comp) = 
            case t_list of 
                [] => Set(list_acc, s_comp)
            |   h::tail =>  let 
                                val x = in_set(s, h)
                            in
                                case x of 
                                    true => intersect(tail, list_acc@[h], s_comp)
                                |   false => intersect(tail, list_acc, s_comp)
                            end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => EmptySet(s_comp)
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => intersect(t_list, [], s_comp)
    end

(* val except_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun except_set(s, t) =
    let 
        fun except(s_set_acc, list) = 
            case list of
                [] => s_set_acc 
            |   h::tail => let 
                            val new_set = remove_from_set(s_set_acc, h)
                        in 
                            except(new_set, tail)
                        end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => s
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => EmptySet(s_comp)
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => except(s, t_list)
    end
    
(* val size_set = fn : ’a set -> int *)
(* complete *)
fun size_set(s: 'a set) =
    let 
        fun get_size(set_list, size) = 
            case set_list of 
                [] => size
            |   h::t => get_size(t, size+1)
    in
        case s of 
            EmptySet comp => 0
        |   Set(list, comp) => get_size(list, 0)
    end

(* val equal_set = fn : ’a set * ’a set -> bool *)
(* complete if both sets are the same type *)
fun equal_set(s, t) =
    let 
        fun equals(s_list, s_comp, t_list, t_comp) = 
            case (s_list, t_list) of
                ([], []) => true
            |    (sh::st, th::tt) => let 
                                        val s_ord = s_comp(sh,th)
                                        val t_ord = t_comp(sh,th)
                                      in
                                        case (s_ord, t_ord) of
                                                (EQUAL, EQUAL) => equals(st, s_comp, tt, t_comp)
                                            |   (_,_) => false 
                                      end
            |   (_,_) => false 
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => true
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => false
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => false
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => equals(s_list, s_comp, t_list, t_comp)
    end 
    
(* val is_subset_of = fn : ’a set * ’a set -> bool *)
(* complete *)
fun is_subset_of(s, t) =
    let 
        fun subset(list) =
            case list of
                [] => true
            |   h::tail =>  let
                                val x = in_set(t, h)
                            in
                                case x of 
                                    true => subset(tail)
                                |   false => false
                            end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => true
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => false
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => true
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => subset(s_list)
    end 
        
(* val list_to_set = fn : ’a list * (’a * ’a -> order) -> ’a set *)
(* complete *)
fun list_to_set(lst, f) =
    let 
        fun put_into_set(lst_acc, s_acc) = 
            case lst_acc of 
                [] => s_acc
            |   h::t => put_into_set(t, insert_into_set(s_acc, h))
    in
        case lst of 
            [] => EmptySet f
        |   h::t => put_into_set(lst, EmptySet(f))
    end

(* val set_to_list = fn : ’a set -> ’a list *)
(* complete *)
fun set_to_list s =
    case s of 
        EmptySet comp => []
    |   Set(list, comp) => list

(* val str_set = fn : ’a set * (’a -> string) -> string *)
(* complete *)
fun str_set (s, fstr) =
    let 
        fun to_str(list, str_acc) = 
            case list of 
                [] => str_acc ^ "}"
            |   h::[] =>    let 
                                val new_str_acc = str_acc ^ fstr(h) 
                            in
                                to_str([], new_str_acc)
                            end 
            |   h::tail =>  let 
                                val new_str_acc = str_acc ^ fstr(h) ^ ":"
                            in
                                to_str(tail, new_str_acc)
                            end 
    in  
        case s of 
            EmptySet(comp) => "{}"
        |   Set(list, comp) => to_str(list , "{")
    end 

(* val map_set = fn : ’a set * (’b * ’b -> order) * (’a -> ’b) -> ’b set *)
fun map_set (s, fcomp, f) =
    EmptySet fcomp

(* fun s -- v = remove_from_set(s,v)
fun s ++ v = insert_into_set(s,v)
fun s IDENTICAL t = equal_set(s,t)
fun s UNION t = union_set(s,t)
fun s INTERSECT t = intersect_set(s,t)
fun s EXCEPT t = except_set(s,t)
fun v IN s = in_set(s,v)
fun s IS_SUBSET_OF t = is_subset_of(s,t) *)