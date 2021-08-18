open Sudoku.Utils
open Sudoku.Solver

let grid = "
. . 6 . . . 4 . 7 
. 2 . 4 . . . . . 
8 . . . 1 6 . . 2
. . . . . . 5 . .
. . 9 2 . 4 7 . .
. . 8 . . . . . .
5 . . 1 3 . . . 8
. . . . . 8 . 4 .
3 . 2 . . . 9 . .
"

let () =
  grid |> grid_of_string |> solve |> Base.Sequence.hd_exn |> grid_to_string
  |> print_endline
