open Base
open Printf

let apply fn args =
  match fn with
  | Ast.Fn fn -> fn args
  | _ -> failwith "not a function"

let is_truthy = function
  | Ast.Nil | Ast.Bool false -> false
  | _ -> true

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
  | Ast.List (Ast.Symbol "def!" :: tl) ->
      eval_def ~env tl
  | Ast.List (Ast.Symbol "let*" :: tl) ->
      eval_let ~env tl
  | Ast.List (Ast.Symbol "do" :: tl) ->
      eval_do ~env tl
  | Ast.List (Ast.Symbol "if" :: tl) ->
      eval_if ~env tl
  | Ast.List _ ->
    (match eval_ast ~env ast with
    | Ast.List (fn :: args) -> apply fn args
    | _ -> assert false)
  | _ -> eval_ast ~env ast

(** def! special form *)
and eval_def ~env = function
  (* def! requires just two arguments
   * first must be symbol and second will be
   * evaluated before associated to the first *)
  | Ast.Symbol sym :: value :: [] ->
      let value = (eval ~env value) in
      Env.set env sym value;
      value
  | _ -> failwith "syntax: use of 'def!'"

(** let* special form
 *  syntax:
 *  (let (foo (+ 1 1)
 *        bar (- 3 2))
 *    (+ foo bar))
 *)
and eval_let ~env = function
  | Ast.List binding_list :: expr :: [] ->
      let enclosed_env = Env.enclose env in
      let rec bind = function
        | [] -> ()
        | Ast.Symbol first :: second :: rest ->
            Env.set enclosed_env first (eval ~env:enclosed_env second);
            bind rest
        | _ -> failwith "syntax: number of binding list was odd"
      in
      bind binding_list;
      eval ~env:enclosed_env expr
  | _ -> failwith "syntax: use of 'let*'"

(** do special form
 * Evaluate all the elements of the list using eval_ast and return the
 * final evaluated element.
 *)
and eval_do ~env = function
  | [] -> failwith "syntax: no expr for do"
  | [ast] -> eval_ast ~env ast
  | hd :: tl -> ignore(eval_ast ~env hd); eval_do ~env tl

(** if special form *)
and eval_if ~env = function
  | condition :: consequence :: alternative ->
      if is_truthy (eval ~env condition) then
        eval ~env consequence
      else
        (match alternative with
        | [] -> Ast.Nil
        | [ast] -> eval ~env ast
        | _ -> failwith "syntax: if's alternative is more than 1 expr")
  | _ -> failwith "syntax: use of 'if'"
