open Data

let ( let* ) x f = Base.Sequence.bind x ~f

let guard x = Base.Sequence.(if x then singleton () else empty)

module Cell = struct
  type t = int * int

  let compare = compare
end

module Cell_set = Set.Make (Cell)

let unknown_cells grid =
  let open Base.Sequence in
  let* row = range 1 10 in
  let* col = range 1 10 in
  let* () = guard (grid |> Grid.get ~row ~col |> Option.is_none) in

  return (row, col)

let related_cells ~row ~col =
  let open Base.Sequence in
  let row_cells =
    range 1 10 |> map ~f:(fun col -> (row, col)) |> to_seq |> Cell_set.of_seq
  in

  let col_cells =
    range 1 10 |> map ~f:(fun row -> (row, col)) |> to_seq |> Cell_set.of_seq
  in

  let subgrid_first x = ((x - 1) / 3 * 3) + 1 in
  let subgrid_last x = subgrid_first x + 3 in

  let subgrid_cells =
    (let* row = range (subgrid_first row) (subgrid_last row) in
     let* col = range (subgrid_first col) (subgrid_last col) in

     return (row, col))
    |> to_seq |> Cell_set.of_seq
  in

  row_cells |> Cell_set.union col_cells |> Cell_set.union subgrid_cells

let initial_grid_constraints grid =
  let open Grid_constraints in
  let related_cell_values ~row ~col =
    related_cells ~row ~col |> Cell_set.to_seq
    |> Seq.flat_map (fun (row, col) ->
           grid |> Grid.get ~row ~col |> Option.to_seq)
    |> Digits.of_seq
  in

  let cell_constraint ~row ~col =
    Digits.diff
      Base.Sequence.(range 1 10 |> to_seq |> Digits.of_seq)
      (related_cell_values ~row ~col)
  in

  unknown_cells grid
  |> Base.Sequence.fold ~init:empty ~f:(fun grid (row, col) ->
         let digits = cell_constraint ~row ~col in
         grid |> set ~row ~col ~digits)

let fix_cell_digit ~row ~col ~digit grid_constraints =
  let open Grid_constraints in
  related_cells ~row ~col |> Cell_set.to_seq
  |> Seq.fold_left
       (fun grid (row, col) -> grid |> remove_digit ~row ~col ~digit)
       grid_constraints
  |> remove ~row ~col
