  let read_lines filename = 
    let contents = In_channel.with_open_bin filename In_channel.input_all in 
  String.split_on_char '\n' contents

 let input = List.map (fun v -> List.init (String.length v) (String.get v) ) (read_lines "input.txt")

 let get_priority item = if Char.code item > 96 && Char.code item < 123 then (Char.code item) - 96 else if item == ' '  then 0 else (Char.code item) - 38 

  let rec get_first_common_item (l1,l2) = match l1, l2 with 
      | [], _ -> None
      | (x:: xs, y) -> if List.mem x y then (Some x) else (get_first_common_item (xs,y))

  let split_list_into_half input_list = 
    let rec split_helper input_list result pointer =
      match input_list, pointer with 
        | _, 0 -> result, input_list
        | [], _ -> result, input_list
        | x :: xs, ptr -> split_helper xs (x :: result) (ptr-1) in
    split_helper input_list [] ((List.length input_list)/2)


  let rec calculate_total input_list total = match input_list with 
    | [] -> total 
    | x :: xs -> calculate_total xs (total + match get_first_common_item (split_list_into_half x) with 
                                            | Some y -> get_priority y
                                            | None -> 0
  )
  
  let () = print_endline (string_of_int (calculate_total input 0))

  (* Part 2 *)

  let rec get_common_items (l1,l2) acc = match l1, l2 with 
      | [], _ -> acc
      | (x:: xs, y) -> (get_common_items (xs,y) (if List.mem x y then (x :: acc) else acc)) 
  
  let get_common_item_3 (l1, l2, l3) = let common_items = get_common_items (l1, l2) [] in let rec helper citems = match citems with
    | [] -> None
    | x :: xs -> if List.mem x l3 then Some x else helper xs  in helper common_items

  let rec calculate input_list total = match input_list with 
      | [] -> total
      | _ :: _ :: [] -> total
      | _ :: [] -> total
      | l1 :: l2 :: l3 :: xs -> calculate xs (total + match get_common_item_3 (l1, l2, l3) with
                                              | Some x -> get_priority x
                                              | None -> 0
      )
  
  let () = print_endline (string_of_int (calculate input 0))
