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
    x |> read |> eval ~env |> print |> print_endline;
    ()
  with
  | Failure msg -> fprintf stderr "Syntax Error: %s\n%!" msg; ()
  | Ast.Mal_exception exn ->
      fprintf stderr "Mal Error: %s\n%!" (Printer.print_str exn); ()

let set_argv env argv =
  let argv = Array.to_list argv
    |> List.map (fun x -> Ast.String x)
  in
  Env.set env "*ARGV*" (Ast.List argv);
  ()

(* main *)
let _ =
  let env = setup_env () in
  let rec loop () =
    printf "(mal)> %!"; (* %! for flush before readline *)
    input_line stdin |> rep ~env;
    loop ()
  in
  try
    let argv = Sys.argv in
    if Array.length argv > 1 then begin
      let filename = argv.(1) in
      set_argv env (Array.sub argv 2 ((Array.length argv) - 2));
      sprintf {|(load-file "%s")|} filename
      |> rep ~env;
    end else begin
      set_argv env [||];
      loop ()
    end
  with End_of_file -> Caml.exit 0
