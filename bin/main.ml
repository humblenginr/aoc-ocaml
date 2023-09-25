  open Core

  let input_matrix = In_channel.read_lines "input.txt" |> List.to_array |> Array.map ~f:(fun x -> String.to_list x |> List.to_array |> Array.map ~f:(fun x -> Char.to_string x |> Int.of_string)) 
  let im = [| [| 3; 0;3;7;3 |]; [| 2;5;5;1;2 |]; [| 6;5;3;3;2 |]; [| 3;3;5;4;9 |]; [| 3;5;3;9;0 |];|]

  let is_tree_visible arr item_index = 
    let is_last_item = Int.equal item_index ((Array.length arr) - 1) in
    match item_index with 
        | 0 -> true
        | _ -> if is_last_item then true else let left_items = Array.slice arr 0 item_index in 
                let right_items = Array.slice arr (item_index+1) (Array.length arr) in
                  let value = arr.(item_index) in 
                    let is_visible_from_left = ( Option.value_exn (Array.max_elt ~compare:Int.compare left_items)) < value in
                      let is_visible_from_right = ( Option.value_exn (Array.max_elt ~compare:Int.compare right_items)) < value in
                        is_visible_from_left || is_visible_from_right

  let scenic_score (row_arr, col_arr) (row_idx, col_idx) = 
      let single_array_scenic_score arr item_idx = let is_tree_at_edge = (Int.equal (item_idx+1) (Array.length arr) ) || Int.equal item_idx 0 in 
                                                        if is_tree_at_edge then 0 else
                                                         let left_slice = Array.slice arr 0 item_idx in 
                                                         let right_slice = Array.slice arr (item_idx+1) (Array.length arr) in 
                                                          let value = arr.(item_idx) in
                                                            let left_blocking_tree_idx = ( match (Array.findi left_slice ~f:(fun _ x -> x >= value)) with 
                                                              | Some (x, _) -> x
                                                              | _ -> 0
                                                              ) in
                                                            let right_blocking_tree_idx  = ( match (Array.findi right_slice ~f:(fun _ x -> x >= value)) with
                                                              | Some (x, _) -> x
                                                              | _ -> (Array.length right_slice) - 1
                                                              ) in
                                                              (item_idx - left_blocking_tree_idx) * (right_blocking_tree_idx + 1)
      in (single_array_scenic_score row_arr col_idx) * (single_array_scenic_score col_arr row_idx)
                    
  let get_row_col matrix (row, col) = matrix.(row), Array.map matrix ~f:(fun row -> row.(col))

  let row_col_length = Array.length input_matrix
  
  let part1 = 
  let rec helper matrix (row_idx, col_idx) visible_trees =  
                                                let (row_array, col_array) = get_row_col matrix (row_idx, col_idx) in 
                                                  let row_visibility = is_tree_visible row_array col_idx in 
                                                    let col_visibility = is_tree_visible col_array row_idx in 
                                                      let (new_row, new_col) = if Int.equal col_idx (row_col_length - 1) then (row_idx + 1, 0) else (row_idx, col_idx + 1) in 
                                                        if Int.equal row_idx (row_col_length - 1) && Int.equal col_idx (row_col_length - 1) then if row_visibility || col_visibility then visible_trees + 1 else visible_trees 
                                                          else if row_visibility || col_visibility then helper matrix (new_row, new_col) (visible_trees + 1) else
                                                          helper matrix (new_row, new_col) (visible_trees) in
            helper input_matrix (0,0) 0


  let part2 = 
    let rec helper matrix (row_idx, col_idx) scores = 
          let (row_array, col_array) = get_row_col matrix (row_idx, col_idx) in 
            let score = scenic_score (row_array, col_array) (row_idx, col_idx) in 
              let (new_row, new_col) = if Int.equal col_idx (row_col_length - 1) then (row_idx + 1, 0) else (row_idx, col_idx + 1) in 
                if Int.equal row_idx (row_col_length - 1) && Int.equal col_idx (row_col_length - 1) then score :: scores else
                  helper matrix (new_row, new_col) (score :: scores) 
    in helper input_matrix (0,0) [] |> List.max_elt ~compare:Int.compare
                                              

