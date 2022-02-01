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
    |   Set([], comp) => true
    |   Set(list, comp) => false

(* val min_in_set = fn : ’a set -> ’a *)
(* complete *)
fun min_in_set s =
    case s of 
        EmptySet comp => raise SetIsEmpty
    |   Set([], comp) => raise SetIsEmpty
    |   Set(list, comp) =>  case list of 
                                [] => raise SetIsEmpty
                            |   h::t => h

(* val max_in_set = fn : ’a set -> ’a *)
(* complete *)
fun max_in_set s =
    let 
        fun aux(list, max, comp) = 
            case list of 
                [] => max
            |   h::t => let
                            val ord = comp(h, max)
                        in
                            case ord of
                                LESS => aux(t, max, comp)
                            |   GREATER => aux(t, h, comp)
                            |   EQUAL => aux(t, max, comp)
                        end
    in
        case s of 
            EmptySet comp => raise SetIsEmpty
        |   Set([], comp) => raise SetIsEmpty
        |   Set(h::t, comp) => aux(t, h, comp)
    end

(* val insert_into_set = fn : ’a set * ’a -> ’a set *)
(* complete *)
fun insert_into_set(s,v) =
    let 
        fun aux(set_list, comp, acc_list) = 
            case set_list of 
                [] => Set(acc_list@[v], comp)
            |   h::t => let
                            val ord = comp(v, h)
                        in
                            case ord of
                                EQUAL => s
                            |   GREATER => aux(t, comp, acc_list@[h]) 
                            |   LESS => Set(acc_list@[v]@[h]@t, comp)
                        end
    in 
        case s of
            EmptySet(comp) => Set([v], comp)
        |   Set([], comp) => Set([v], comp)
        |   Set(list, comp) => aux(list, comp, [])
    end

(* val in_set = fn : ’a set * ’a -> bool *)
(* complete *)
fun in_set(s, v) =
    let 
        fun aux(list, comp) = 
            case list of 
                [] => false
            |   h::t => let
                            val ord = comp(v, h)
                        in
                            case ord of
                                EQUAL => true
                            |   GREATER => aux(t,comp)
                            |   LESS => false
                        end
    in
        case s of
            EmptySet(comp) => false
        |   Set([], comp) => false
        |   Set(list, comp) => aux(list, comp)
    end

(* val remove_from_set = fn : ’a set * ’a -> ’a set *)
(* complete *)
fun remove_from_set(s,v) =
    let 
        fun aux(set_list, comp, acc_list) = 
            case set_list of 
                [] => Set(acc_list, comp)
            |   h::t => let
                            val ord = comp(v, h)
                        in
                            case ord of
                                EQUAL =>    let 
                                                val x = acc_list@t
                                            in 
                                                case x of 
                                                    [] => EmptySet(comp)
                                                |   _ => Set(x,comp)
                                            end
                                         
                            |   GREATER => aux(t, comp, acc_list@[h])
                            |   LESS => s
                        end
    in 
        case s of
            EmptySet(comp) => EmptySet(comp)
        |   Set([], comp) => EmptySet(comp)
        |   Set(list, comp) => aux(list, comp, [])
    end

(* val union_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun union_set(s, t) =
    let 
        fun aux(s_set_acc, list) = 
        case list of 
            [] => s_set_acc
        |   h::tail =>  let 
                            val new_set = insert_into_set(s_set_acc, h)
                        in 
                            aux(new_set, tail)
                        end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => s
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => t
        |   (Set([], s_comp), Set([], t_comp)) => EmptySet(s_comp)
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => aux(s, t_list)
    end 

(* val intersect_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun intersect_set(s, t) =
    let 
        fun aux(t_list, set_acc) = 
            case t_list of 
                [] => set_acc
            |   h::tail =>  let 
                                val x = in_set(s, h)
                            in
                                case x of 
                                    true => aux(tail, insert_into_set(set_acc, h))
                                |   false => aux(tail, set_acc)
                            end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => EmptySet(s_comp)
        |   (Set([], s_comp), Set([], t_comp)) => EmptySet(s_comp)
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => aux(t_list, EmptySet(s_comp))
    end

(* val except_set = fn : ’a set * ’a set -> ’a set *)
(* must be tail recursive *)
fun except_set(s, t) =
    let 
        fun aux(s_set_acc, list) = 
            case list of
                [] =>   let 
                            val x = s_set_acc
                        in 
                            case x of 
                                Set([], comp) => EmptySet(comp)
                            |   _ => s_set_acc
                        end 
            |   h::tail =>  let 
                                val new_set = remove_from_set(s_set_acc, h)
                            in 
                                aux(new_set, tail)
                            end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => EmptySet(s_comp)
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => s
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => EmptySet(s_comp)
        |   (Set([], s_comp), Set([], t_comp)) => EmptySet(s_comp)
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => aux(s, t_list)
    end
    
(* val size_set = fn : ’a set -> int *)
(* complete *)
fun size_set(s: 'a set) =
    let 
        fun aux(set_list, size) = 
            case set_list of 
                [] => size
            |   h::t => aux(t, size+1)
    in
        case s of 
            EmptySet comp => 0
        |   Set(list, comp) => aux(list, 0)
    end

(* val equal_set = fn : ’a set * ’a set -> bool *)
(* complete if both sets are the same type *)
fun equal_set(s, t) =
    let 
        fun aux(s_list, s_comp, t_list, t_comp) = 
            case (s_list, t_list) of
                ([], []) => true
            |    (sh::st, th::tt) => let 
                                        val s_ord = s_comp(sh,th)
                                        val t_ord = t_comp(sh,th)
                                      in
                                        case (s_ord, t_ord) of
                                                (EQUAL, EQUAL) => aux(st, s_comp, tt, t_comp)
                                            |   (_,_) => false 
                                      end
            |   (_,_) => false 
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => true
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => false
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => false
        |   (Set([], s_comp), Set([], t_comp)) => true
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => aux(s_list, s_comp, t_list, t_comp)
    end 
    
(* val is_subset_of = fn : ’a set * ’a set -> bool *)
(* complete *)
fun is_subset_of(s, t) =
    let 
        fun aux(list) =
            case list of
                [] => true
            |   h::tail =>  let
                                val x = in_set(t, h)
                            in
                                case x of 
                                    true => aux(tail)
                                |   false => false
                            end
    in 
        case (s,t) of 
            (EmptySet(s_comp), EmptySet(t_comp)) => true
        |   (Set(s_list,s_comp), EmptySet(t_comp)) => false
        |   (EmptySet(s_comp), Set(t_list, t_comp)) => true
        |   (Set([], s_comp), Set([], t_comp)) => true
        |   (Set(s_list, s_comp), Set(t_list, t_comp)) => aux(s_list)
    end 
        
(* val list_to_set = fn : ’a list * (’a * ’a -> order) -> ’a set *)
(* complete *)
fun list_to_set(lst, f) =
    let 
        fun aux(lst_acc, s_acc) = 
            case lst_acc of 
                [] => s_acc
            |   h::t => aux(t, insert_into_set(s_acc, h))
    in
        case lst of 
            [] => EmptySet f
        |   h::t => aux(lst, EmptySet(f))
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
        fun aux(list, str_acc) = 
            case list of 
                [] => str_acc ^ "}"
            |   h::[] =>    let 
                                val new_str_acc = str_acc ^ fstr(h) 
                            in
                                aux([], new_str_acc)
                            end 
            |   h::tail =>  let 
                                val new_str_acc = str_acc ^ fstr(h) ^ ":"
                            in
                                aux(tail, new_str_acc)
                            end 
    in  
        case s of 
            EmptySet(comp) => "{}"
        |   Set([], comp) => "{}"
        |   Set(list, comp) => aux(list , "{")
    end 

(* val map_set = fn : ’a set * (’b * ’b -> order) * (’a -> ’b) -> ’b set *)
fun map_set (s, fcomp, f) =
    let 
        fun aux(set_list, map_set_acc) = 
            case set_list of 
                []  => map_set_acc
            |   h::tail =>  let 
                                val x = f(h)
                                val new_map_set_acc = insert_into_set(map_set_acc, x)
                            in
                                aux(tail, new_map_set_acc)
                            end 
    in 
    case s of 
        EmptySet(comp) => EmptySet(fcomp)
    |   Set([], fcomp) => EmptySet(fcomp)
    |   Set(list, fcomp) => aux(list, EmptySet(fcomp))
    end

fun s -- v = remove_from_set(s,v)
fun s ++ v = insert_into_set(s,v)
fun s IDENTICAL t = equal_set(s,t)
fun s UNION t = union_set(s,t)
fun s INTERSECT t = intersect_set(s,t)
fun s EXCEPT t = except_set(s,t)
fun v IN s = in_set(s,v)
fun s IS_SUBSET_OF t = is_subset_of(s,t)

fun comp_list_any (a: 'a list, b: 'a list, fcomp : ('a * 'a) -> order) =
    case (a,b) of
        ([],[]) => EQUAL
    |   (ah::atail,[]) => GREATER
    |   ([],bh::btail) => LESS
    |   (ah::atail,bh::btail) => let
                                    val x = fcomp(ah,bh)
                                 in 
                                    case x of 
                                        EQUAL => comp_list_any(atail, btail, fcomp)
                                    |   LESS => LESS
                                    |   GREATER => GREATER
                                 end

val se = EmptySet Int.compare
val sa = se ++ 1 ++ 2 ++ 3 ++ 5 ++ 3 ++ 2
val sb = se ++ 9 ++ 3 ++ 2 --2
val s1 = is_empty_set se
val s2 = sb UNION sb 
val s3 = sb INTERSECT sa 
val s4 = sb EXCEPT sb 
val s5 = 2 IN sb
val s6 = se IS_SUBSET_OF sb

val emp = EmptySet Int.compare
val one = emp ++ 1
val two = emp ++ 2
val test = one -- 1 
val test2 = emp -- 1 
val int = one INTERSECT two