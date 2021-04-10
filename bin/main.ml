open Mal
open Repl

let _ =
  let env = setup_env () in
  try
    match Array.to_list Sys.argv with
    | [] -> assert false
    | _ :: [] ->
        set_argv env [];
        loop env
    | _ :: filename :: argv ->
        set_argv env argv;
        eval_file ~env filename
  with End_of_file -> Caml.exit 0
