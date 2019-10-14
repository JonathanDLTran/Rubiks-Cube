(*Need a data structure to store data relating to the rubik's cube
  I could use a record with all the surfaces 
  I could use a int list list list, a three dimensional list, where 
  there are lists of lists of ints with each surface 

  or more simply, i can use an int tuple tuple tuple, where each tuple has length 3 or 6*)

type 'a row = 'a * 'a * 'a
type 'a grid = {row_one : 'a row ; row_two : 'a row; row_three : 'a row}

type rotation = 
  | CCW
  | CW

type orientation = 
  | Front 
  | Back 
  | Left 
  | Right 
  | Top 
  | Bottom 

type color = 
  | Blue
  | Yellow
  | Red
  | Green
  | Orange
  | Purple

type 'a face = {face : orientation; grid : 'a grid}

let create_row (n : 'a) : 'a row = (n, n, n)

let create_grid (n : 'a) : 'a grid = (let row = create_row n in {row_one = row; row_two = row; row_three = row})

let create_face (n : 'a) : 'a grid =  create_grid n

type rubik = 
  {front : color grid; 
   back : color grid; 
   left : color grid; 
   right : color grid;
   top : color grid;
   bottom : color grid }

let faces = {front = create_face Blue; 
             back = create_face Yellow; 
             left = create_face Red; 
             right = create_face Green; 
             top = create_face Orange; 
             bottom = create_face Purple}


let color_to_string elt = 
  match elt with
  | Blue -> "B"
  | Yellow -> "Y"
  | Red -> "R"
  | Green -> "G"
  | Orange -> "O"
  | Purple -> "P"

let print_row (a, b, c) = 
  color_to_string a 
  ^ " " ^ 
  color_to_string b 
  ^ " " ^ 
  color_to_string c 
  ^ "\n"

let print_face face = 
  let {row_one = r1; row_two = r2; row_three = r3} = face in
  print_string(print_row r1 ^ print_row r2 ^ print_row r3);
  print_newline ()

let print_rubik (cube : rubik) = 
  let {front = front; 
       back = back;
       left = left;
       right = right;
       top = top;
       bottom = bottom;} 
    = cube in 
  print_string "front"; print_newline (); print_face front;
  print_string "back"; print_newline (); print_face back;
  print_string "left"; print_newline (); print_face left;
  print_string "right"; print_newline (); print_face right;
  print_string "top"; print_newline (); print_face top;
  print_string "bottom"; print_newline (); print_face bottom

let () = print_rubik faces


let modify_first (x, y, z) n = n, y, z

let modify_second (x, y, z) n = x, n, z

let modify_third (x, y, z) n = x, y, n

let modify_row (x, y, z) col n = 
  if col = 1 then modify_first (x, y, z) n
  else if col = 2 then modify_second (x, y, z) n
  else modify_third (x, y, z) n

let modify_grid grid row column n = 
  if row = 1 then {grid with row_one = modify_row grid.row_one column n}
  else if row = 2 then {grid with row_two = modify_row grid.row_two column n}
  else {grid with row_one = modify_row grid.row_three column n}

let modify_face_element faces face row column = 
  match face with
  | Front -> modify_grid (faces.front) row column
  | Back -> modify_grid (faces.back) row column
  | Left -> modify_grid (faces.left) row column
  | Right -> modify_grid (faces.right) row column
  | Top -> modify_grid (faces.top) row column
  | Bottom -> modify_grid (faces.bottom) row column

let modify_entire_row grid row row_num= 
  if row_num = 1 then {grid with row_one = row}
  else if row_num = 2 then {grid with row_two = row}
  else {grid with row_three = row}

let modify_entire_column {row_one = r1; row_two = r2; row_three = r3} (c1, c2, c3) column_num =
  {row_one = modify_row r1 column_num c1; row_two = modify_row r2 column_num c2; row_three = modify_row r3 column_num c3}

let get_column (x, y, z) column = 
  if column = 1 then x 
  else if column = 2 then y
  else z

let extract_column column {row_one = r1; row_two = r2; row_three = r3}  = 
  (get_column r1 column, get_column r2 column, get_column r3 column)

let extract_row row {row_one = r1; row_two = r2; row_three = r3} = 
  if row = 1 then r1
  else if row = 2 then r2
  else r3

let extract_grid faces face = 
  match face with
  | Front -> faces.front
  | Back -> faces.back
  | Left -> faces.left
  | Right -> faces.right
  | Top -> faces.top
  | Bottom -> faces.bottom

let left_right orientation direction =
  match orientation with
  | Front -> if direction = CW then Left else Right
  | Back -> if direction = CW then Right else Left
  | Left -> if direction = CW then Back else Front
  | Right -> if direction = CW then Front else Back
  | Top -> Top
  | Bottom -> Bottom

let up_down orientation direction = 
  match orientation with
  | Front -> if direction = CW then Bottom else Top
  | Back -> if direction = CW then Top else Bottom
  | Top -> if direction = CW then Front else Back
  | Bottom -> if direction = CW then Back else Front
  | Left -> Left
  | Right -> Right

let lateral orientation direction = 
  match orientation with
  | Top -> if direction = CW then Right else Left
  | Bottom -> if direction = CW then Left else Right
  | Left -> if direction = CW then Top else Bottom
  | Right -> if direction = CW then Bottom else Top
  | Front -> Front
  | Back -> Back

let reverse_row (x, y, z) = (z, y, x)

let lateral_move direction faces column = 
  let top_row = Top |> extract_grid faces |> extract_row column in
  let bottom_row = Bottom |> extract_grid faces |> extract_row column in
  let left_column = Left |> extract_grid faces |> extract_column column in 
  let right_column = Right |> extract_grid faces |> extract_column column in
  if direction = CW then begin
    let right = modify_entire_column (extract_grid faces Right) top_row column in
    let bottom = modify_entire_row (extract_grid faces Bottom) (reverse_row right_column) column in
    let left = modify_entire_column (extract_grid faces Left) (reverse_row bottom_row) column in
    let top = modify_entire_row (extract_grid faces Top) left_column column in
    {faces with top = top; bottom = bottom; left = left; right = right}
  end 
  else begin
    let right = modify_entire_column (extract_grid faces Right) (reverse_row bottom_row) column in
    let bottom = modify_entire_row (extract_grid faces Bottom) left_column column in
    let left = modify_entire_column (extract_grid faces Left) (reverse_row top_row) column in
    let top = modify_entire_row (extract_grid faces Top) right_column column in
    {faces with top = top; bottom = bottom; left = left; right = right}
  end


(*TODO : left_right_move and up_down_move*)
let left_right_move direction faces row = 
  let front_row = Front |> extract_grid faces |> extract_row row in 
  let back_row = Back |> extract_grid faces |> extract_row row in 
  let left_row = Left |> extract_grid faces |> extract_row row in 
  let right_row = Right |> extract_grid faces |> extract_row row in 
  if direction = CW then begin 
    let front = modify_entire_row (extract_grid faces Front) right_row row in 
    let back = modify_entire_row (extract_grid faces Back) (reverse_row left_row) row in 
    let left = modify_entire_row (extract_grid faces Left) front_row row in 
    let right = modify_entire_row (extract_grid faces Right) (reverse_row back_row) row in
    {faces with front = front; back = back; left = left; right = right}
  end
  else begin
    let front = modify_entire_row (extract_grid faces Front) left_row row in 
    let back = modify_entire_row (extract_grid faces Back) (reverse_row right_row) row in 
    let left = modify_entire_row (extract_grid faces Left) (reverse_row back_row) row in 
    let right = modify_entire_row (extract_grid faces Right) front_row row in
    {faces with front = front; back = back; left = left; right = right}
  end

let up_down_move direction faces column = 
  let front_column = Front |> extract_grid faces |> extract_column column in 
  let back_column = Back |> extract_grid faces |> extract_column column in 
  let top_column = Top |> extract_grid faces |> extract_column column in 
  let bottom_column = Bottom |> extract_grid faces |> extract_column column in 
  if direction = CW then begin 
    let front = modify_entire_row (extract_grid faces Front) (reverse_row bottom_column) column in 
    let back = modify_entire_row (extract_grid faces Back) (reverse_row top_column) column in 
    let top = modify_entire_row (extract_grid faces Top) front_column column in 
    let bottom = modify_entire_row (extract_grid faces Bottom) back_column column in
    {faces with front = front; back = back; top = top; bottom = bottom}
  end
  else begin
    let front = modify_entire_row (extract_grid faces Front) top_column column in 
    let back = modify_entire_row (extract_grid faces Back) bottom_column column in 
    let top = modify_entire_row (extract_grid faces Top) (reverse_row back_column) column in 
    let bottom = modify_entire_row (extract_grid faces Bottom) (reverse_row front_column) column in
    {faces with front = front; back = back; top = top; bottom = bottom}
  end

let r1 = lateral_move CW faces 2
let () = print_rubik r1
let r2 = lateral_move CCW faces 2
let () = print_rubik r2
let r3= lateral_move CCW faces 1
let () = print_rubik r3
let r4 = lateral_move CCW faces 3
let () = print_rubik r4

let r1 = lateral_move CW faces 2
let () = print_rubik r1

let r5 = left_right_move CW r1 2
let () = print_rubik r5
let r6 = up_down_move CCW r5 2
let () = print_rubik r6

type move = 
  | Error
  | UpDown of int * rotation
  | LeftRight of int * rotation
  | Lateral of int * rotation

let is_digit str = 
  if String.length str <> 1 then false
  else 
    let character = String.get str 0 in 
    print_endline (Char.escaped character);
    print_endline (string_of_bool (character > '0' && character < '4'));
    if character > '0' && character < '4' then true else false

let rotation_of_string str = 
  match str with
  | "ccw" -> `Counterclockwise
  | "cw" -> `Clockwise
  | _ -> `RotationError

let convert_rotation rotation = 
  match rotation with 
  | `Counterclockwise -> CCW
  | `Clockwise -> CW
  | `RotationError -> failwith "this is a bad failure for rotations"

let parse_phrase str_lst = 
  match str_lst with
  | move :: column :: rotation :: t -> 
    let rotation_converted = rotation_of_string rotation in 
    print_endline (string_of_bool (rotation_converted = `RotationError));
    if rotation_converted = `RotationError then Error 
    else if move = "updown" && is_digit column && (rotation_converted = `Counterclockwise || rotation_converted = `Clockwise) then UpDown (int_of_string column, convert_rotation rotation_converted)
    else if move = "leftright" && is_digit column && (rotation_converted = `Counterclockwise || rotation_converted = `Clockwise) then LeftRight (int_of_string column, convert_rotation rotation_converted)
    else if move = "lateral" && is_digit column && (rotation_converted = `Counterclockwise || rotation_converted = `Clockwise) then Lateral (int_of_string column, convert_rotation rotation_converted)
    else Error
  | _ -> Error

let parse str = 
  str 
  |> String.trim
  |> String.split_on_char ' '
  |> List.filter (fun elt -> elt <> " ")
  |> List.map String.lowercase_ascii
  |> parse_phrase

let rec event_loop rubiks_cube = 
  print_endline "This is the cube at the present moment.";
  print_rubik rubiks_cube;
  print_endline "Please enter your command to move the cube";
  print_endline ">  ";
  match parse (read_line ()) with 
  | UpDown (column, direction) -> event_loop (up_down_move direction rubiks_cube column)
  | LeftRight (column, direction) -> event_loop (left_right_move direction rubiks_cube column)
  | Lateral (column, direction) -> event_loop (lateral_move direction rubiks_cube column)
  | Error -> print_int 4; event_loop rubiks_cube

let main = 
  print_endline "Welcome to the CS 3110 Rubik's Cube!";
  let rubiks_cube = faces in 
  event_loop rubiks_cube














