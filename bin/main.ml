(* Find the number of calories carried by the elf that carries the most number of calories *)

(* Read the input file and store line by line values in a list*)
(* let lines = In_channel.input_line *)
(* Now write a function that takes the list as an input and outputs a list of totals*)
(* Then pass that to another function which finds the maximum value among those totals*)


  let read_lines filename = 
    let contents = In_channel.with_open_bin filename In_channel.input_all in 
  String.split_on_char '\n' contents


  (* ["1000", "20000", "2300", "", "234", "234490"] *)
  let lines = read_lines "input.txt"


  let rec group input result = 
  match input with 
  | [] -> result 
  | "" :: rest -> group rest (0 :: result)
  | cals :: rest ->  group rest (match result with 
    | [] -> [int_of_string cals]
    | h :: t -> int_of_string cals + h :: t
    )

  let rec max_of_list input highest = match input with 
    | [] -> highest
    | h :: rest ->  max_of_list rest (max highest h)

let () = print_endline (string_of_int  (max_of_list (group lines []) 0))


let rec firstk k xs = match xs with
| [] -> failwith "firstk"
| x::xs -> if k=1 then [x] else x::firstk (k-1) xs;;

let compare_inverse x y = if x == y then 0 else if x > y then -1 else 1

let max3 elf_calories = firstk 3 (List.sort compare_inverse elf_calories)

let sum = List.fold_left (+) 0;;

let () = print_endline (string_of_int (sum (max3 (group lines []))))

