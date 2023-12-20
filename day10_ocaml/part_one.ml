let file = "input"

let read_lines name : string list = 
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let lines = read_lines file

let () = List.iter (fun line -> print_endline line) lines

let line_length = String.length (List.hd lines)
let number_of_lines = List.length lines

let () = print_endline ("line length: " ^ (string_of_int line_length))
let () = print_endline ("number of lines: " ^ (string_of_int number_of_lines))


let grid = Array.make_matrix number_of_lines line_length '.'

let () = List.iteri (fun i line -> 
  String.iteri (fun j c -> 
    grid.(i).(j) <- c
  ) line
) lines

(* find start "S" in grid *)
(* (x, y) *)
let start = ref (0, 0)

let () = 
  for i = 0 to number_of_lines - 1 do
    for j = 0 to line_length - 1 do
      if grid.(i).(j) = 'S' then
        start := (j, i)
    done
  done

let () = print_endline ("start: " ^ (string_of_int (fst !start)) ^ ", " ^ (string_of_int (snd !start)))

(* 
  Direction Symbols:
  | : north - south
  - : east - west
  L : north east
  J : north west
  7 : south west
  F : south east
  . : empty
*)

let north_valid_symbols = ['|';'F';'7']
let south_valid_symbols = ['|';'L';'J']
let east_valid_symbols = ['-';'J';'7']
let west_valid_symbols = ['-';'L';'F']


let north_from_start_symbol = if (snd !start) - 1 >= 0 then grid.((snd !start) - 1).(fst !start) else '.'
let south_from_start_symbol = if (snd !start) + 1 < number_of_lines then grid.((snd !start) + 1).(fst !start) else '.'
let east_from_start_symbol = if (fst !start) + 1 < line_length then grid.(snd !start).((fst !start) + 1) else '.'
let west_from_start_symbol = if (fst !start) - 1 >= 0 then grid.(snd !start).((fst !start) - 1) else '.'

let north_from_start_valid = List.mem north_from_start_symbol north_valid_symbols
let south_from_start_valid = List.mem south_from_start_symbol south_valid_symbols
let east_from_start_valid = List.mem east_from_start_symbol east_valid_symbols
let west_from_start_valid = List.mem west_from_start_symbol west_valid_symbols

let () = print_endline ("north from start: " ^ (String.make 1 north_from_start_symbol))
let () = print_endline ("south from start: " ^ (String.make 1 south_from_start_symbol))
let () = print_endline ("east from start: " ^ (String.make 1 east_from_start_symbol))
let () = print_endline ("west from start: " ^ (String.make 1 west_from_start_symbol))

let () = print_endline ("north from start valid: " ^ (string_of_bool north_from_start_valid))
let () = print_endline ("south from start valid: " ^ (string_of_bool south_from_start_valid))
let () = print_endline ("east from start valid: " ^ (string_of_bool east_from_start_valid))
let () = print_endline ("west from start valid: " ^ (string_of_bool west_from_start_valid))

let next_position = if north_from_start_valid then (fst !start, (snd !start) - 1)
                    else if south_from_start_valid then (fst !start, (snd !start) + 1)
                    else if east_from_start_valid then ((fst !start) + 1, snd !start)
                    else if west_from_start_valid then ((fst !start) - 1, snd !start)
                    else (0, 0)

let next_symbol = grid.(snd next_position).(fst next_position)

let number_of_turns = ref 0
  
let rec f (x1, y1) (x2, y2) =
  number_of_turns := !number_of_turns + 1;
  let () = print_endline ("symbol: " ^ (String.make 1 grid.(y2).(x2))) in

  let symbol = grid.(y2).(x2) in
  
  match symbol with
  | '|' -> if (y2 > y1) then f (x2, y2) (x2, y2 + 1) else f (x2, y2) (x2, y2 - 1)
  | '-' -> if (x2 < x1) then f (x2, y2) (x2 - 1, y2) else f (x2, y2) (x2 + 1, y2)
  | 'L' -> if (x2 < x1) then f (x2, y2) (x2, y2 - 1) else f (x2, y2) (x2 + 1, y2)
  | 'J' -> if (x2 > x1) then f (x2, y2) (x2, y2 - 1) else f (x2, y2) (x2 - 1, y2)
  | '7' -> if (x2 > x1) then f (x2, y2) (x2, y2 + 1) else f (x2, y2) (x2 - 1, y2)
  | 'F' -> if (x2 < x1) then f (x2, y2) (x2, y2 + 1) else f (x2, y2) (x2 + 1, y2)
  | '.' -> (x2, y2)
  | 'S' -> (x2, y2)
  | _ -> (0, 0)

let end_position = f !start next_position

let () = print_endline ("end position: " ^ (string_of_int (fst end_position)) ^ ", " ^ (string_of_int (snd end_position)))

let () = print_endline ("number of turns: " ^ (string_of_int !number_of_turns))

let furthest_point_from_start = !number_of_turns / 2

let () = print_endline ("furthest point from start: " ^ (string_of_int furthest_point_from_start))