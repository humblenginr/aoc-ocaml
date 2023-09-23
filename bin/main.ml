  open Core

  let input_string = String.to_list (In_channel.read_all "input.txt")

  let rec solve input_string res = match input_string with 
      | [] -> List.length res
      | x :: xs -> if (List.length res) > 2 then 
                        (* Make a hashtable out the list of last 3 elements of the `res` list and the upcoming element *)
                        (* If the length is 4, then it means that we have reached the end *)
                        (* 13 Characters for part 2 and 3 chars for part 1 *)
                        let hashtbl = Hashtbl.of_alist (module Char) (List.map (x :: (List.take res 13 )) ~f:(fun x -> (x, "ok"))) in
                        match hashtbl with
                        | Hashtbl_intf.(`Duplicate_key _) -> solve xs (x :: res)
                        | Hashtbl_intf.(`Ok _) -> List.length (x :: res)
                      else solve xs (x :: res)

  let () = solve input_string [] |> Int.to_string |> print_endline
