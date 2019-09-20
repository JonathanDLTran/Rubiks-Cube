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

type 'a rubik = 
  {front : 'a grid; 
   back : 'a grid; 
   left : 'a grid; 
   right : 'a grid;
   top : 'a grid;
   bottom : 'a grid }

let faces = {front = create_face Blue; 
             back = create_face Yellow; 
             left = create_face Red; 
             right = create_face Green; 
             top = create_face Orange; 
             bottom = create_face Purple}

let modify_first (x, y, z) n = n, y, z

let modify_second (x, y, z) n = x, n, z

let modify_third (x, y, z) n = x, y, n

let modify_row (x, y, z) col n = 
  if col = 1 then modify_first (x, y, z) n
  else if col = 2 then modify_second (x, y, z) n
  else modify_third (x, y, z) n

let modify_grid grid row column n = 
  if row = 1 then {grid with row_one = modify_row grid.row_one column nan}
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
    let right = modify_entire_column (extract_grid faces Top) top_row column in
    let bottom = modify_entire_row (extract_grid faces Bottom) (reverse_row right_column) column in
    let left = modify_entire_column (extract_grid faces Left) (reverse_row bottom_row) column in
    let top = modify_entire_row (extract_grid faces Top) left_column column in
    {faces with top = top; bottom = bottom; left = left; right = right}
  end 
  else begin
    let right = modify_entire_column (extract_grid faces Top) (reverse_row bottom_row) column in
    let bottom = modify_entire_row (extract_grid faces Bottom) left_column column in
    let left = modify_entire_column (extract_grid faces Left) (reverse_row top_row) column in
    let top = modify_entire_row (extract_grid faces Top) right_column column in
    {faces with top = top; bottom = bottom; left = left; right = right}
  end











