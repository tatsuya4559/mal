open Base
open Printf

let apply fn args =
  match fn with
  | Ast.Fn fn -> fn args
  | _ -> failwith "not a function"


let rec eval_ast ~env ast =
  match ast with
  | Ast.Symbol x ->
    (match Env.find env x with
    | None -> failwith (sprintf "%s not found" x)
    | Some x -> x)
  | Ast.List lst ->
      Ast.List (List.map lst ~f:(eval ~env))
  | _ -> ast

and eval ~env ast =
  match ast with
  | Ast.List [] -> ast
  | Ast.List _ ->
    (match eval_ast ~env ast with
    | Ast.List (fn :: args) -> apply fn args
    | _ -> assert false)
  | _ -> eval_ast ~env ast
