  open Core

  let is_adjacent (hx, hy) (tx, ty) = ( hx = tx) && (abs (hy - ty) = 1) || (hy = ty) && (abs (hx - tx) = 1) 
  let is_diagonal (hx, hy) (tx, ty) = (abs (hx - tx) = 1) && (abs (hy - ty) = 1) 
  let is_overlapping (hx, hy) (tx, ty) = (hx = tx) && (hy = ty) 

  let does_tail_have_to_move ~headpos:h ~tailpos:t = not (is_adjacent h t || is_diagonal h t || is_overlapping h t)
  let update_tail_pos ~headpos:(hx, hy) ~tailpos:(tx, ty) = match (hx - tx),(hy - ty) with 
                                            | 2, 0 -> (tx+1, ty)
                                            | -2, 0 -> (tx-1, ty)
                                            | 0, 2 -> (tx, ty + 1)
                                            | 0, -2 -> (tx, ty - 1)
                                            (* for diagonal*)
                                            | xdif, ydif -> if ((xdif = 2 && ydif = -1) || (xdif = 1 && ydif = -2)) then (tx+1, ty-1) else
                                                            if ((xdif = -2 && ydif = -1) || (xdif = -1 && ydif = -2)) then (tx-1, ty-1) else 
                                                            if ((xdif = -1 && ydif = 2) || (xdif = -2 && ydif = 1)) then (tx-1, ty+1) else 
                                                            if ((xdif = 2 && ydif = 1) || (xdif = 1 && ydif = 2)) then (tx+1, ty+1) else 
                                                            failwith "don't know what to do for this position"

  type movement_instr = | Right of int
                  | Left of int
                  | Up of int
                  | Down of int

  let input_list = In_channel.read_lines "input.txt" |> List.map ~f:(String.split ~on:' ') 
  let parsed_input =
      (let rec helper instrs input_list = match input_list with 
                  | [] -> instrs
                  | x :: xs -> let dir = List.hd_exn x in 
                                let amt = Int.of_string (List.last_exn x) in 
                                  let instr = (match dir with 
                                    | "L" -> Left amt
                                    | "R" -> Right amt
                                    | "U" -> Up amt
                                    | "D" -> Down amt
                                    | _ -> failwith "Unparsable input"
                                    ) in
                                    helper (instr :: instrs) xs in 
      helper [] input_list) |> List.concat_map ~f:(fun mvmt -> match mvmt with 
                              | Right x -> Array.map ~f:(fun x -> Right x) (Array.create ~len:x 1) |> Array.to_list
                              | Left x -> Array.map ~f:(fun x -> Left x) (Array.create ~len:x 1) |> Array.to_list
                              | Down x -> Array.map ~f:(fun x -> Down x) (Array.create ~len:x 1) |> Array.to_list
                              | Up x -> Array.map ~f:(fun x -> Up x) (Array.create ~len:x 1) |> Array.to_list 
  ) |> List.rev

  let res_matrix = Array.make_matrix ~dimx:500 ~dimy:500 0
  
  let move_head_once_and_update_tail dir matrix (hx, hy) (tx, ty) = 
    let updated_head_pos = (match dir with 
        | Right _ -> (hx+1, hy)
        | Left _ -> (hx-1, hy)
        | Up _ -> (hx, hy-1)
        | Down _ -> (hx, hy+1)
    ) in
    if does_tail_have_to_move ~headpos: updated_head_pos ~tailpos: (tx, ty) then
          let (utx, uty) = update_tail_pos ~headpos: updated_head_pos ~tailpos: (tx, ty) in
              matrix.(utx).(uty) <- 1; (updated_head_pos, (utx, uty)) else
              (updated_head_pos, (tx, ty))

  let rec part1 instrs matrix (hx, hy) (tx, ty) = match instrs with 
    | [] ->  Array.fold ~init:0 ~f:(fun acc cur -> acc + (Array.sum (module Int) cur ~f:Fun.id)) matrix
    | x :: xs -> let (uh, ut) = move_head_once_and_update_tail x matrix (hx, hy) (tx, ty) in 
                   part1 xs matrix uh ut  

  let ans = part1 parsed_input res_matrix (250, 250) (250, 250) 

                  
