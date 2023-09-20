  let read_lines filename = 
    let contents = In_channel.with_open_bin filename In_channel.input_all in 
  String.split_on_char '\n' contents

 let input = List.map (fun v -> List.init (String.length v) (String.get v) ) (read_lines "input.txt")

 let get_priority item = if Char.code item > 96 && Char.code item < 123 then (Char.code item) - 96 else if item == ' '  then 0 else (Char.code item) - 38 

 let rec get_common_item (first_half, second_half) citem = match first_half, citem with 
    | _, Some x-> x 
    | x :: xs, None -> get_common_item (xs, second_half) (try (Some (List.find (fun i -> i == x) second_half)) with 
        | _ -> None
      )
      (* This means that there is nothing common *)
    | [] , None -> ' '

  let split_list_into_half input_list = 
    let rec split_helper input_list result pointer =
      match input_list, pointer with 
        | _, 0 -> result, input_list
        | [], _ -> result, input_list
        | x :: xs, ptr -> split_helper xs (x :: result) (ptr-1) in
    split_helper input_list [] ((List.length input_list)/2)


  let rec calculate_total input_list total = match input_list with 
    | [] -> total 
    | x :: xs -> calculate_total xs (total + get_priority (get_common_item (split_list_into_half x) None))
  
  let () = print_endline (string_of_int (calculate_total input 0))
