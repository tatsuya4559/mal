open Printf

open Reader
open Evaluator
open Printer

let prelude = [
  {|(def! load-file (fn* (f) (eval (read-string (str "(do " (slurp f) "\nnil)")))))|};
  {|(load-file "prelude.mal")|};
]

let setup_env () =
  let env = Env.make () ~binds:Builtin.fns in
  (* use env to define eval function *)
  Env.set env "eval" (Builtin.make_eval env);
  (* load predefined functions *)
  List.iter (fun x -> ignore(read_str x |> eval ~env)) prelude;
  env

let set_argv env argv =
  let argv = List.map (fun x -> Ast.String x) argv in
  Env.set env "*ARGV*" (Ast.List argv)

let rep ~env x =
  try
    x |> read_str |> eval ~env |> print_str |> print_endline
  with
  | Failure msg -> fprintf stderr "Syntax Error: %s\n%!" msg
  | Ast.Mal_exception exn ->
      fprintf stderr "Mal Error: %s\n%!" (Printer.print_str exn)

let rec loop env =
  printf "(mal)> %!"; (* %! for flush before readline *)
  input_line stdin |> rep ~env;
  loop env

let eval_file ~env filename =
  sprintf {|(load-file "%s")|} filename |> rep ~env
