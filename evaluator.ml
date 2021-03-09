open Base
open Printf

let apply operator operands =
  match operator with
  | Ast.Fn fn -> fn operands
  | _ -> failwith "cannot apply"


let rec eval_ast ~env ast =
  match ast with
  | Ast.Symbol x ->
    (match Env.get env x with
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
    | Ast.List (operator :: operands) -> apply operator operands
    | _ -> assert false)
  | _ -> eval_ast ~env ast
