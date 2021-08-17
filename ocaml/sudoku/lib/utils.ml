open Data

let grid_to_string grid =
  let separator1 = "        |         |        " in
  let separator2 = "--------+---------+--------" in

  let render_row row = 
    let c i = 
      grid |> Grid.get ~row ~col:i
      |> Option.map Int.to_string 
      |> Option.value ~default:"."  
    in Printf.sprintf "%s  %s  %s | %s  %s  %s | %s  %s  %s"
      (c 1) (c 2) (c 3) (c 4) (c 5) (c 6) (c 7) (c 8) (c 9)

  in
  [ render_row 1; separator1; render_row 2; separator1; render_row 3; separator2;
    render_row 4; separator1; render_row 5; separator1; render_row 6; separator2;
    render_row 7; separator1; render_row 8; separator1; render_row 9]
  |> String.concat "\n"

let grid_of_string s =
  let filter_line l = 
    String.to_seq l 
    |> Seq.filter (fun c -> c == '.' || Base.Char.is_digit c) 
    |> String.of_seq
  in

  let lines = 
    s 
    |> String.split_on_char '\n'
    |> List.map filter_line
    |> List.filter (fun s -> Bool.not @@ Base.String.is_empty s)
  in

  let digit_of_char d = Seq.return d |> String.of_seq |> int_of_string in

  let cells_with_digits =
    lines
    |> Base.List.concat_mapi ~f:(fun row_number row -> 
        row |> String.to_seq |> List.of_seq 
        |> Base.List.mapi ~f:(fun column_number c -> 
            ((row_number + 1, column_number + 1), c)
          )
      )
    |> List.filter (fun (_, c) -> Base.Char.is_digit c)
    |> List.map (fun (cell, c) -> (cell, digit_of_char c))
  in 

  Base.List.fold_left 
    cells_with_digits 
    ~init:Grid.empty 
    ~f:(fun grid ((row, col), (digit)) -> 
        grid |> Grid.set ~row ~col ~digit)
