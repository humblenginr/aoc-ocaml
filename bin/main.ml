  open Core

  type file = (string * int)
  type dir = {
    name: string
    ; mutable subdirs : dir list
    ; mutable files : file list
    ; mutable size: int
    ; parent : dir option
  }

  let get_file_size (_, size) = size 

  let empty_dir name parent = {
    name = name
    ; subdirs = []
    ; files = []
    ; size = 0
    ; parent = parent
  } 

  let root_dir = empty_dir "/" None
  type instr = | Cd of string
               | Ls
               | Ls_dir of string
               | Ls_file of file

  let parse_input = In_channel.read_lines "input.txt"
                    |> List.map ~f:(fun x ->
      if String.equal "$ ls" x then Ls
      else if (String.is_prefix ~prefix:"$ cd" x) then Cd (String.drop_prefix x 5)
      else if (String.is_prefix ~prefix:"dir" x) then Ls_dir (String.drop_prefix x 4)
      else let file_list = String.split x ~on:' ' in Ls_file (List.last_exn file_list, Int.of_string (List.hd_exn file_list))
    )

  let find_dir dir_name dir = List.find dir.subdirs ~f:(fun x -> String.equal x.name dir_name)

  let parse_instrs instrs = 
    let root = root_dir in 
    let rec helper instrs cur_dir = match instrs with 
          | [] -> ()
          | instr :: xs -> match instr with 
              | Cd "/" -> helper xs cur_dir
              | Ls -> helper xs cur_dir
              | Ls_file f ->  cur_dir.files <- (f :: cur_dir.files); helper xs cur_dir
              | Ls_dir dir_name -> let d = empty_dir dir_name (Some cur_dir) in cur_dir.subdirs <- (d :: cur_dir.subdirs); helper xs cur_dir
              | Cd ".." -> (match cur_dir.parent with 
                            | None ->  failwith "Parent does not exist"
                            | Some parent -> helper xs parent)
              | Cd d -> (match (find_dir d cur_dir) with 
                    | None ->  failwith "Directory not found"
                    | Some found_dir -> helper xs found_dir)
    in helper instrs root; root

    let tree = parse_instrs (parse_input)

    let rec dir_size dir =
      let file_sum = List.fold dir.files ~init:0 ~f:(fun sum file -> sum + get_file_size file) in
      let dir_sum =
        List.fold dir.subdirs ~init:0 ~f:(fun sum dir -> sum + dir_size dir)
      in
      file_sum + dir_sum

    let rec get_all_sizes dir =
      dir_size dir :: List.concat_map dir.subdirs ~f:get_all_sizes

let part1 = get_all_sizes tree |> List.filter ~f:(fun size -> size <= 100000) |> List.sum (module Int) ~f:Fn.id

let total_used_space =  List.max_elt (get_all_sizes tree) ~compare:(Int.compare) |> Option.value_exn
let space_needed = let space_free = 70000000 - total_used_space in 30000000 - space_free
let part2 = List.filter (get_all_sizes tree) ~f:(fun size -> size >= space_needed) |> List.min_elt ~compare:(Int.compare)
