open Base
open Stdio

let env =
  let default_fns = [
    "+", Builtin.add;
    "-", Builtin.sub;
    "*", Builtin.mul;
    "/", Builtin.div;
  ] in
  Env.set_all Env.empty default_fns

let read s =
  Reader.read_str s

let eval ast =
  Evaluator.eval ast ~env

let print ast =
  Printer.print_str ast

let _ =
  let open In_channel in
  let open Out_channel in
  let rec loop () =
    try
      printf "(mal)> %!"; (* %! for flush before readline *)
      input_line_exn stdin
      |> read
      |> eval
      |> print
      |> print_endline;
      loop ()
    with Failure msg -> fprintf stderr "Error: %s\n%!" msg; loop()
  in
  try
    loop ()
  with End_of_file -> Caml.exit 0
