open Base
open Stdio
open Printf

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

let setup_env () =
  let env = Env.make () ~binds:Builtin.fns in
  (* use env to define eval function *)
  Env.set env "eval" (Builtin.make_eval env);
  (* load predefined functions *)
  List.iter ~f:(fun x -> ignore(read x |> eval ~env)) prelude;
  env

let rep ~env x =
  let open Out_channel in
  x
  |> read
  |> eval ~env
  |> print
  |> print_endline;
  ()

let set_argv env argv =
  let argv = Array.to_list argv
    |> List.map ~f:(fun x -> Ast.String x)
  in
  Env.set env "*ARGV*" (Ast.List argv);
  ()

(* main *)
let _ =
  let open In_channel in
  let open Out_channel in
  let env = setup_env () in
  let rec loop () =
    try
      printf "(mal)> %!"; (* %! for flush before readline *)
      input_line_exn stdin |> rep ~env;
      loop ()
    with Failure msg -> fprintf stderr "Error: %s\n%!" msg; loop()
  in
  try
    let argv = Sys.get_argv () in
    if Array.length argv > 1 then begin
      let filename = argv.(1) in
      set_argv env (Array.subo argv ~pos:2);
      sprintf {|(load-file "%s")|} filename
      |> rep ~env;
    end else begin
      set_argv env [||];
      loop ()
    end
  with End_of_file -> Caml.exit 0
