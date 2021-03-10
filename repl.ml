open Base
open Stdio

let setup_env () =
  Env.make () ~binds:Builtin.fns

let read s =
  Reader.read_str s

let eval ~env ast =
  Evaluator.eval ast ~env

let print ast =
  Printer.print_str ast

let prelude = [
  "(def! not (fn* (x) (if x false true)))";
]

let _ =
  let open In_channel in
  let open Out_channel in
  let env = setup_env () in
  (* load predefined functions *)
  List.iter ~f:(fun x -> ignore(read x |> eval ~env)) prelude;
  let rec loop () =
    try
      printf "(mal)> %!"; (* %! for flush before readline *)
      input_line_exn stdin
      |> read
      |> eval ~env
      |> print
      |> print_endline;
      loop ()
    with Failure msg -> fprintf stderr "Error: %s\n%!" msg; loop()
  in
  try
    loop ()
  with End_of_file -> Caml.exit 0
