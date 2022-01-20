structure Babies =
struct
local
  open Csc330
in
  
(*
Student name: Treavor Gagne
Student id: V00890643

Add your code below this line

Remember: your submitted code cannot use print

*)

(* 
  use case: size_of_list([1,2,3,4])
  return: int
*)
fun size_of_list(list): int = 
  if null list
  then 0
  else 1 + size_of_list(tl list);

(* 
  use case: list_of_lists(["Anne,0,0,1,1,3, ...,"Xander,1,2,3,4,11"])
  return: list * list
*)
fun list_of_lists(list) = 
  if null list
  then []
  else split_at(hd(list), #",") :: list_of_lists(tl(list));

(* 
  use case: 
  return: formatted strings of search_list
*)
fun get_babies_data(babies_data, search_list, offsetSt, num_entries) =
  let
    fun get_babies_match(babies_data, search_list) = 
      let 
        fun get_babies_match_trimmed(trimmed_data, search_list) = 
          let 
            fun get_babies_stats(list) = 
              let 
                fun calculate_babies_stats(list, pos, years, first_pair, last_pair, min_list, max_list) = 
                  let 
                    val curr_year_count_int = valOf(fromString(hd(list)))
                    val curr_year_int = valOf(fromString(offsetSt)) + pos
                  in 
                    if size_of_list(tl(list)) = 1
                    then 
                      let 
                        fun output_babies_stats(pos, years, first_pair:(int * int)list, last_pair:(int * int)list, min_list:(int * int)list, max_list:(int * int)list) = 
                          let 
                            val first_year = int_to_string(#1(hd(first_pair)))
                            val first_count = int_to_string(#2(hd(first_pair)))
                            val last_year = int_to_string(#1(hd(last_pair)))
                            val last_count = int_to_string(#2(hd(last_pair)))
                            val min_year = int_to_string(#1(hd(min_list)))
                            val min_count = int_to_string(#2(hd(min_list)))
                            val max_year = int_to_string(#1(hd(max_list)))
                            val max_count = int_to_string(#2(hd(max_list)))
                            val total_int = valOf(fromString(hd(tl(list))))
                            val total_real = int_to_real(total_int)
                            val total_string = int_to_string(total_int)
                            val num_entries_int = valOf(fromString(num_entries))
                            val num_enteries_real = int_to_real(num_entries_int) 
                            val avg_real = (total_real/num_enteries_real)
                            val avg_string = real_to_string(avg_real)
                          in
                            " Total: " ^ total_string ^ 
                            "\n Years: " ^ int_to_string(years) ^  
                            "\n " ^ int_to_string(curr_year_int) ^ ": " ^ hd(list) ^ 
                            "\n First: " ^ first_year ^ " " ^ first_count ^
                            "\n Last: " ^ last_year ^ " " ^  last_count ^
                            "\n Min: " ^ min_year ^ " " ^ min_count ^
                            "\n Max: " ^ max_year ^ " " ^ max_count ^
                            "\n Avg: " ^ avg_string ^ "\n"
                          end
                      in 
                        if 0 < curr_year_count_int
                        then 
                          if null max_list andalso null min_list
                          then output_babies_stats(pos+1, years+1, [(curr_year_int, curr_year_count_int)]@first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, [(curr_year_int, curr_year_count_int)]@min_list, [(curr_year_int, curr_year_count_int)]@max_list)
                          else if curr_year_count_int >= #2(hd(max_list))
                          then output_babies_stats(pos+1, years+1, first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, min_list, [(curr_year_int, curr_year_count_int)]@max_list)
                          else if curr_year_count_int < #2(hd(min_list))
                          then output_babies_stats(pos+1, years+1, first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, [(curr_year_int, curr_year_count_int)]@min_list, max_list)
                          else output_babies_stats(pos+1, years+1, first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, min_list,  max_list)
                        else output_babies_stats(pos+1, years, first_pair, last_pair, min_list, max_list)
                      end 
                    else if 0 < curr_year_count_int
                    then 
                      if null max_list andalso null min_list
                      then calculate_babies_stats(tl(list), pos+1, years+1, [(curr_year_int, curr_year_count_int)]@first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, [(curr_year_int, curr_year_count_int)]@min_list, [(curr_year_int, curr_year_count_int)]@max_list)
                      else if curr_year_count_int >= #2(hd(max_list))
                      then calculate_babies_stats(tl(list), pos+1, years+1, first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, min_list, [(curr_year_int, curr_year_count_int)]@max_list)
                      else if curr_year_count_int < #2(hd(min_list))
                      then calculate_babies_stats(tl(list), pos+1, years+1, first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, [(curr_year_int, curr_year_count_int)]@min_list, max_list)
                      else calculate_babies_stats(tl(list), pos+1, years+1, first_pair, [(curr_year_int, curr_year_count_int)]@last_pair, min_list,  max_list)
                    else calculate_babies_stats(tl(list), pos+1, years, first_pair, last_pair, min_list, max_list)
                  end
              in 
                calculate_babies_stats(list, 0, 0, [], [], [], [])
              end
          in
            if null trimmed_data
            then hd(search_list) ^ "\n" ^ "Baby name ["^ hd(search_list) ^"] was not found\n"^ get_babies_match(babies_data, tl(search_list))
            else if hd(search_list) = hd(hd(trimmed_data))
            then hd(search_list) ^ "\n" ^ get_babies_stats(tl(hd(trimmed_data))) ^ get_babies_match(babies_data, tl(search_list))
            else get_babies_match_trimmed(tl(trimmed_data), search_list)
          end
      in
        if null search_list
        then ""
        else if null babies_data
        then ""
        else get_babies_match_trimmed(babies_data, search_list)
      end
  in
    get_babies_match(babies_data, search_list)
  end 

fun babies_program (babiesLines, NamesLines, offsetSt) = (* the output of the program is the string returned by this function *)
  let 
    val babies_str_list = split_at(babiesLines, #"\n") 
    val babies_data = list_of_lists(babies_str_list)
    val search_list = split_at(NamesLines, #"\n") 
    val num_babies = int_to_string(size_of_list(babies_str_list)) (* returns str number of babies in the database *)
    val num_entries = int_to_string(size_of_list(split_at(hd babies_str_list, #",")) - 2) (* returns str number of years available in the data *)
    val info_str = "Read " ^ num_babies ^ " babies" ^ dot ^ " Starting year " ^ offsetSt ^ dot ^ " Each baby has " ^ num_entries ^ " entries" ^ dot ^"\n"
  in 
    info_str ^ get_babies_data(babies_data, search_list, offsetSt, num_entries)
  end

end

end
    
