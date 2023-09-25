  open Core

  let input_matrix = In_channel.read_lines "input.txt" |> List.to_array |> Array.map ~f:(fun x -> String.to_list x |> List.to_array |> Array.map ~f:(Char.to_int)) 

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
                        

                    
  let get_row_col matrix (row, col) = matrix.(row), Array.map matrix ~f:(fun row -> row.(col))

  let row_col_length = Array.length input_matrix
  
  let part1 = 
  let rec helper matrix (row, col) visible_trees = if Int.equal row (row_col_length - 1) && Int.equal col (row_col_length - 1) then visible_trees else 
                                                let (row_array, col_array) = get_row_col matrix (row, col) in 
                                                  let row_visibility = is_tree_visible row_array col in 
                                                    let col_visibility = is_tree_visible col_array row in 
                                                      let (new_row, new_col) = if Int.equal col (row_col_length - 1) then (row + 1, 0) else (row, col + 1) in 
                                                        if row_visibility || col_visibility then helper matrix (new_row, new_col) (visible_trees + 1) else 
                                                          helper matrix (new_row, new_col) (visible_trees) in
            helper input_matrix (0,0) 0

                                              

