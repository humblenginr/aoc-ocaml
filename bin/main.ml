
(* X - Rock Y - Paper Z - Scissor*)
(* A - Rock B - Paper C - Scissor*)

let points_for_result res = match res with  
    | "X" ->  0
    | "Y" -> 3
    | "Z" -> 6
    | _ -> assert false


 let points_for_shape = function 
    | "X" -> 1
    | "Y" -> 2
    | "Z" -> 3
    | _ -> assert false

let get_my_value ov result = match ov, result with 
  | "A", "X" -> "Z"
  | "B", "X" -> "X"
  | "C", "X" -> "Y"
  | "A", "Y" -> "X"
  | "B", "Y" -> "Y"
  | "C", "Y" -> "Z"
  | "A", "Z" -> "Y"
  | "B", "Z" -> "Z"
  | "C", "Z" -> "X"
  | _, _ -> assert false

let total_points_for_round my_value res = points_for_shape my_value + points_for_result res

  let read_lines filename = 
    let contents = In_channel.with_open_bin filename In_channel.input_all in 
  String.split_on_char '\n' contents

(* [['X', 'A'], ['Y', 'B']] *)
let input = List.map (fun v -> String.split_on_char ' ' v) (read_lines "input.txt")

let rec total_points input_list total = match input_list with 
    | [] -> total
    | x :: xs -> match x with 
      | ov :: res :: _ -> total_points xs (total + total_points_for_round (get_my_value ov res) res)
      | _ -> total

let () = total_points input 0 |> string_of_int |> print_endline


(* Part 2*)

