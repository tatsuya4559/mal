open Printf
open Mal

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
  List.iter (fun x -> ignore(read x |> eval ~env)) prelude;
  env

let rep ~env x =
  try
    x |> read |> eval ~env |> print |> print_endline
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


(* main *)
let _ =
  let env = setup_env () in
  try
    match Array.to_list Sys.argv with
    | [] -> assert false
    | _ :: [] ->
        Env.set_argv env [];
        loop env
    | _ :: filename :: argv ->
        Env.set_argv env argv;
        eval_file ~env filename
  with End_of_file -> Caml.exit 0
