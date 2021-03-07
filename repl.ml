open Base
open Stdio

let read x = x

let eval x = x

let print x = x

let _ =
  let open In_channel in
  let open Out_channel in
  let rec loop () =
    printf "(mal)> %!"; (* %! for flush before readline *)
    input_line_exn stdin
    |> read
    |> eval
    |> print
    |> print_endline;
    loop ()
  in
  try
    loop ()
  with End_of_file -> Caml.exit 0
