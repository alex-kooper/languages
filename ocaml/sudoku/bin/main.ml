open Sudoku.Utils
open Sudoku.Solver

let read_string () = in_channel_length stdin |> really_input_string stdin

let () =
  read_string () |> grid_of_string |> solve |> Base.Sequence.hd_exn
  |> grid_to_string |> print_endline
