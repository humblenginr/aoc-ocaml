
(* X - Rock Y - Paper Z - Scissor*)
(* A - Rock B - Paper C - Scissor*)

let points_from_round my_value opponent_value = match my_value, opponent_value with  
    | "X","A" ->  3
    | "Y", "A" -> 6
    | "Z", "A" -> 0
    | "X", "B" -> 0
    | "Y", "B" -> 3
    | "Z", "B" -> 6
    | "X", "C" -> 6
    | "Y", "C" -> 0
    | "Z", "C" -> 3
    | _, _-> 0

 let points_for_shape = function 
    | "X" -> 1
    | "Y" -> 2
    | "Z" -> 3
    | _ -> assert false

let total_points_for_round my_value opponent_value = points_for_shape my_value + points_from_round my_value opponent_value

  let read_lines filename = 
    let contents = In_channel.with_open_bin filename In_channel.input_all in 
  String.split_on_char '\n' contents

(* [['X', 'A'], ['Y', 'B']] *)
let input = List.map (fun v -> String.split_on_char ' ' v) (read_lines "input.txt")

let rec total_points input_list total = match input_list with 
    | [] -> total
    | x :: xs -> match x with 
      | ov :: mv :: _ -> total_points xs (total + total_points_for_round mv ov)
      | _ -> total

let () = total_points input 0 |> string_of_int |> print_endline

