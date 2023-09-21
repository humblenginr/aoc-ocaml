  let read_lines filename = 
    let contents = In_channel.with_open_bin filename In_channel.input_all in 
  String.split_on_char '\n' contents

  (* [[51-88; 52-87]; [41-55; 22-56]...] *)
  let input = List.filter (fun x -> Bool.not (x = [""])) (List.map (fun v -> String.split_on_char ',' v ) (read_lines "input.txt"))

 let is_contained assign_pair = match assign_pair with 
      | first :: second :: _ -> begin let parts1 = String.split_on_char '-' first in let parts2 = String.split_on_char '-' second in
                                let start1 = int_of_string (List.hd parts1) in let end1 = int_of_string (List.nth parts1 1) in 
                                let start2 = int_of_string (List.hd parts2) in let end2 = int_of_string (List.nth parts2 1) in 
                                match start1, start2, end1, end2 with 
                                | s1, s2, _ , _ when s1 == s2 -> true
                                | s1, s2, e1, e2 when s1 > s2 -> if e2 >= e1 then true else false
                                | s1, s2, e1, e2 when s1 < s2 -> if e1 >= e2 then true else false
                                | _ -> assert false end
      | _ :: [] -> assert false
      | [] -> assert false

  let solve input_list = let rec helper il total = match il with 
      | [] -> total
      | assign_pair :: rest -> if is_contained assign_pair then helper rest (total + 1) else helper rest total 
    in helper input_list 0

  let () = solve input |> string_of_int |> print_endline
                                
