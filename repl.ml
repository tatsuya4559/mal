open Base
open Stdio

let setup_env () =
  let env = Env.make () ~binds:Builtin.fns in
  (* use env to define eval function *)
  Env.set env "eval" (Builtin.make_eval env);
  env

let read s =
  Reader.read_str s

let eval ~env ast =
  Evaluator.eval ast ~env

let print ast =
  Printer.print_str ast

let prelude = [
  {|(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))|};
  {|(load-file "prelude.mal")|};
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
