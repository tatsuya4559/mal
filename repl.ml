open Base
open Stdio

let env =
  let default_fns = [
    "+", Builtin.add;
    (*
    "-", ( - );
    "*", ( * );
    "/", ( / );
    *)
  ] in
  Env.set_all Env.empty default_fns

let read s =
  let open Out_channel in
  match Reader.read_str s with
  | Ok x -> x
  | Error s -> fprintf stderr "%s\n" s; Caml.exit 1

let eval ast =
  Evaluator.eval ast ~env

let print ast =
  Printer.print_str ast

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
