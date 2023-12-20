(* don't feel like puttings this in 'proper' functions *)
let file = "input"

let zfill s width =
  let to_fill = width - (String.length s) in
  if to_fill <= 0 then s
  else (String.make to_fill '0') ^ s

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

let line_length = String.length (List.hd lines)
let number_of_lines = List.length lines

let grid = Array.make_matrix number_of_lines line_length '.'

let () = List.iteri (fun i line -> 
  String.iteri (fun j c -> 
    grid.(i).(j) <- c
  ) line
) lines

let start = ref (0, 0)

let () = 
  for i = 0 to number_of_lines - 1 do
    for j = 0 to line_length - 1 do
      if grid.(i).(j) = 'S' then
        start := (j, i)
    done
  done

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

let next_position = if north_from_start_valid then (fst !start, (snd !start) - 1)
                    else if south_from_start_valid then (fst !start, (snd !start) + 1)
                    else if east_from_start_valid then ((fst !start) + 1, snd !start)
                    else if west_from_start_valid then ((fst !start) - 1, snd !start)
                    else (0, 0)

let next_symbol = grid.(snd next_position).(fst next_position)

let number_of_turns = ref 0

let grid_only_path = Array.make_matrix number_of_lines line_length '.'

let () = grid_only_path.(snd !start).(fst !start) <- 'X'
  
let rec f (x1, y1) (x2, y2) =
  number_of_turns := !number_of_turns + 1;

  let symbol = grid.(y2).(x2) in

  grid_only_path.(y2).(x2) <- symbol;
  
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

let furthest_point_from_start = !number_of_turns / 2

let tiles_inside = ref 0
let tiles_outside = ref 0

let puzzle_string_list = ref []

let counter = ref 0

let () = for i = 0 to number_of_lines - 1 do
  for j = 0 to line_length - 1 do
    let how_many_direction_changes_north = ref 0 in
    let how_many_direction_changes_south = ref 0 in
    let how_many_direction_changes_east = ref 0 in
    let how_many_direction_changes_west = ref 0 in
    (* '|' -> '║' and | '-' -> '═' and etc. ...*)
    let print_symbol = match (String.make 1 grid.(i).(j)) with
        "|" -> "│"
      | "-" -> "─"
      | "L" -> "└"
      | "J" -> "┘"
      | "7" -> "┐"
      | "F" -> "┌"
      | "S" -> "S"
      | _ -> " " in
    

    let changes_buffer = ref 0 in

    for k = i downto 0 do
      if not (grid_only_path.(k).(j) = '.') then
        how_many_direction_changes_north := match grid_only_path.(k).(j) with
        | '|' -> !how_many_direction_changes_north
        | '-' -> !how_many_direction_changes_north + 1
        | 'L' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_north + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_north)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_north))
        | 'J' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_north + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_north)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_north))
        | '7' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_north + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_north)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_north))
        | 'F' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_north + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_north)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_north))
        | _ -> !how_many_direction_changes_north

    done;

    changes_buffer := 0;

    for k = i to number_of_lines - 1 do
      if not (grid_only_path.(k).(j) = '.') then
        how_many_direction_changes_south := match grid_only_path.(k).(j) with
        | '|' -> !how_many_direction_changes_south
        | '-' -> !how_many_direction_changes_south + 1
        | 'L' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_south + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_south)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_south))
        | 'J' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_south + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_south)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_south))
        | '7' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_south + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_south)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_south))
        | 'F' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_south + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_south)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_south))
        | _ -> !how_many_direction_changes_south

    done;

    changes_buffer := 0;

    for k = j to line_length - 1 do
      if not (grid_only_path.(i).(k) = '.') then
        how_many_direction_changes_east := match grid_only_path.(i).(k) with
        | '|' -> !how_many_direction_changes_east + 1
        | '-' -> !how_many_direction_changes_east
        | 'L' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_east + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_east)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_east))
        | 'J' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_east + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_east)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_east))
        | '7' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_east + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_east)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_east))
        | 'F' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_east + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_east)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_east))
        | _ -> !how_many_direction_changes_east

    done;

    changes_buffer := 0;

    for k = j downto 0 do
      if not (grid_only_path.(i).(k) = '.') then
        how_many_direction_changes_west := match grid_only_path.(i).(k) with
        | '|' -> !how_many_direction_changes_west + 1
        | '-' -> !how_many_direction_changes_west
        | 'L' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_west + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_west)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_west))
        | 'J' -> (match !changes_buffer with
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_west + 1)
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_west)
          | _ -> (changes_buffer := -1; !how_many_direction_changes_west))
        | '7' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_west + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_west)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_west))
        | 'F' -> (match !changes_buffer with
          | -1 -> (changes_buffer := 0; !how_many_direction_changes_west + 1)
          | 1 -> (changes_buffer := 0; !how_many_direction_changes_west)
          | _ -> (changes_buffer := 1; !how_many_direction_changes_west))
        | _ -> !how_many_direction_changes_west

    done;


    let is_north_X_odd = (!how_many_direction_changes_north mod 2) = 1 in
    let is_south_X_odd = (!how_many_direction_changes_south mod 2) = 1 in
    let is_east_X_odd = (!how_many_direction_changes_east mod 2) = 1 in
    let is_west_X_odd = (!how_many_direction_changes_west mod 2) = 1 in

    (* let () = print_endline ("" ^ (string_of_int !counter) ^ "(" ^ (string_of_int !how_many_direction_changes_north) ^ ", " ^ (string_of_int !how_many_direction_changes_south) ^ ", " ^ (string_of_int !how_many_direction_changes_east) ^ ", " ^ (string_of_int !how_many_direction_changes_west) ^ ")") in *)

    if grid_only_path.(i).(j) = '.' then
      if is_north_X_odd && is_south_X_odd && is_east_X_odd && is_west_X_odd then begin
        puzzle_string_list := !puzzle_string_list @ [" I  "];
        tiles_inside := !tiles_inside + 1
      end else begin
        puzzle_string_list := !puzzle_string_list @ [" " ^ zfill ("" ^ (string_of_int !counter)) 3];
        tiles_outside := !tiles_outside + 1
      end
    else
      puzzle_string_list := !puzzle_string_list @ ["   " ^ print_symbol];

    counter := !counter + 1

  done;
  puzzle_string_list := !puzzle_string_list @ ["\n"]
done

(* let () = List.iter (fun s -> print_string s) !puzzle_string_list *)

let () = print_endline ("Part 2 Answer: " ^ (string_of_int !tiles_inside))