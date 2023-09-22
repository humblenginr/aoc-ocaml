  open Core
let input_list = [
  ['S'; 'P'; 'H'; 'V'; 'F'; 'G'];
  ['M'; 'Z'; 'D'; 'V'; 'B'; 'F'; 'J'; 'G'];
  ['N'; 'J'; 'L'; 'M'; 'G'];
  ['P'; 'W'; 'D'; 'V'; 'Z'; 'G'; 'N']; 
  ['B'; 'C'; 'R'; 'V'];
  ['Z'; 'L'; 'W'; 'P'; 'M'; 'S'; 'R'; 'V'];
  ['P'; 'H'; 'T'];
  ['V'; 'Z'; 'H'; 'C'; 'N'; 'S'; 'R'; 'Q'];
  ['J'; 'Q'; 'V'; 'P'; 'G'; 'L'; 'F'];
]

  let input_array = Array.of_list input_list

(* let split_on_char_and_filter char content =  *)
    (* List.filter (String.split_on_chars ~on:[char] content) ~f:(fun x ->  String.equal x "" |> not ) *)

let instructions = List.filter (In_channel.read_lines "input.txt") ~f:(fun x -> not (String.equal x ""))

let decode_instruction instr = Scanf.sscanf instr "move %d from %d to %d" (fun qty from too -> qty, from, too)
                                      
let rec rearrange_stacks input_array instrs = match instrs with 
    | [] -> input_array
    | instr :: xs -> let qty, from, too = decode_instruction instr in
                      let from_list = input_array.(from-1) in
                      let to_list = input_array.(too-1) in
                      let elements_to_be_added = List.take from_list qty in
                      let () = Array.set input_array (from-1) (List.drop from_list qty) in
                      (* Adding List.rev will be for part-1 of the problem *)
                      let () = Array.set input_array (too-1) (List.append (elements_to_be_added) to_list) in
                      rearrange_stacks input_array xs

  (* here we assume that stack is a list and is arranged *)
  let rec get_top_crates res stack = match stack with 
      | [] -> res
      | x :: xs -> get_top_crates (( String.of_char (List.last_exn x )) ^ res) xs

  let lss = rearrange_stacks input_array instructions |> Array.to_list 

