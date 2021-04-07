open Printf

let apply fn args =
  match fn with
  | Ast.Fn { body; _ } -> body args
  | _ -> failwith "not a function"

let is_truthy = function
  | Ast.Nil | Ast.Bool false -> false
  | _ -> true

let make_binds params args =
  let rec loop acc params args =
    match (params, args) with
    | [], [] -> acc
    | [], _ | _, [] -> failwith "syntax: too many or less arguments"
    (* if a "&" symbol is encountered in the binds list, the next symbol
        in the binds list after the "&" is bound to the rest of the exprs list
        that has not been bound yet. *)
    | (Ast.Symbol "&") :: (Ast.Symbol s) :: _, args -> (s, Ast.List args) :: acc
    | (Ast.Symbol s) :: rest_params, arg :: rest_args ->
        loop ((s, arg) :: acc) rest_params rest_args
    | _, _ -> failwith "syntax: parameter must be symbol"
  in
  List.rev @@ loop [] params args

let rec quasiquote = function
  | Ast.List lst ->
    let rec loop = function
      | [] -> Ast.List []
      | Ast.Symbol "unquote" :: second :: _ -> second
      | Ast.List (Ast.Symbol "splice-unquote" :: second :: _) :: rest ->
          Ast.List [Ast.Symbol "concat"; second; (loop rest)]
      | first :: rest ->
          Ast.List [Ast.Symbol "cons"; (quasiquote first); (loop rest)]
    in
    loop lst
  | (Ast.Symbol _) as ast-> Ast.List [Ast.Symbol "quote"; ast]
  | _ as ast -> ast

(** This function takes arguments ast and env. It returns true if ast
    is a list that contains a symbol as the first element and that symbol
    refers to a function in the env environment and that function has the
    is_macro attribute set to true. Otherwise, it returns false. *)
let is_macro_call ~env = function
  | Ast.Symbol sym :: _ ->
      (match Env.get env sym with
      | Some Ast.Fn { is_macro; _ } -> is_macro
      | _ -> false)
  | _ -> false

(** This function takes arguments ast and env. It calls is_macro_call
    with ast and env and loops while that condition is true. Inside the
    loop, the first element of the ast list (a symbol), is looked up in
    the environment to get the macro function. This macro function is then
    called/applied with the rest of the ast elements (2nd through the last)
    as arguments. The return value of the macro call becomes the new value
    of ast. When the loop completes because ast no longer represents a macro
    call, the current value of ast is returned. *)
let macroexpand ~env ast =
  let rec loop ast =
    match ast with
    | Ast.List (Ast.Symbol sym :: rest) -> (
        match Env.get env sym with
        | Some Ast.Fn { is_macro; body } ->
            if is_macro then loop (body rest)
            else ast
        | _ -> ast
      )
    | _ -> ast
  in
  loop ast

let rec eval_ast ~env ast =
  match ast with
  | Ast.Symbol x ->
    (match Env.get env x with
    | None -> failwith (sprintf "%s not found" x)
    | Some x -> x)
  | Ast.List lst ->
      Ast.List (List.map (eval ~env) lst)
  | _ -> ast

(* I don't have to impl tail call optimization because I'm using OCaml.
 * Test:
 *   > (def! sum-to (fn* (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))
 *   > (sum-to 10000000)
 *   > Fatal error: exception Stack overflow
 *
 *   > (def! sum2 (fn* (n acc) (if (= n 0) acc (sum2 (- n 1) (+ n acc)))))
 *   > (sum2 10000000 0)
 *   > 50000005000000
 *)
and eval ~env ast =
  let ast = macroexpand ~env ast in
  match ast with
  | Ast.List [] -> ast
  | Ast.List (Ast.Symbol "def!" :: tl) ->
      eval_def ~env tl
  | Ast.List (Ast.Symbol "defmacro!" :: tl) ->
      eval_defmacro ~env tl
  | Ast.List (Ast.Symbol "let*" :: tl) ->
      eval_let ~env tl
  | Ast.List (Ast.Symbol "do" :: tl) ->
      eval_do ~env tl
  | Ast.List (Ast.Symbol "if" :: tl) ->
      eval_if ~env tl
  | Ast.List (Ast.Symbol "fn*" :: tl) ->
      eval_fn ~env tl
  | Ast.List (Ast.Symbol "try*" :: tl) ->
      eval_try ~env tl
  | Ast.List (Ast.Symbol "quote" :: ast :: _) ->
      ast (* just return argument *)
  | Ast.List (Ast.Symbol "quasiquoteexpand" :: ast :: _) ->
      quasiquote ast
  | Ast.List (Ast.Symbol "quasiquote" :: ast :: _) ->
      eval ~env (quasiquote ast)
  | Ast.List (Ast.Symbol "macroexpand" :: ast :: _) ->
      macroexpand ~env ast
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

(** This is very similar to the def! form, but before the evaluated
    value (mal function) is set in the environment, the is_macro attribute
    should be set to true. *)
and eval_defmacro ~env = function
  | Ast.Symbol sym :: value :: [] ->
      let value =
        match eval ~env value with
        | Ast.Fn fn -> Ast.Fn { fn with is_macro = true }
        | _ as x -> x
      in
      Env.set env sym value;
      value
  | _ -> failwith "syntax: use of 'defmacro!'"

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
  | [ast] -> eval ~env ast
  | hd :: tl -> ignore(eval ~env hd); eval_do ~env tl

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

(** fn* special form *)
and eval_fn ~env = function
  | Ast.List params :: expr :: [] ->
      let closure args =
        let binds = make_binds params args in
        let enclosed_env = Env.enclose env ~binds in
        eval ~env:enclosed_env expr
      in
      Ast.fn closure
  | _ -> failwith "syntax: use of 'fn*'"

(** try* special form *)
and eval_try ~env = function
  | expr :: Ast.List [Ast.Symbol "catch*"; Ast.Symbol sym; handler] :: [] -> (
      try eval ~env expr with
      | Ast.Mal_exception exn ->
          let binds = [sym, exn] in
          let enclosed_env = Env.enclose env ~binds in
          eval ~env:enclosed_env handler
    )
  | _ -> failwith "syntax: use of try*/catch*"


let%test_module "test evaluator" = (module struct

  (* is_macro_call *)
  let%test "when foo is macro" =
    let binds =
      ["foo", Ast.Fn { is_macro = true; body = (fun _ -> Ast.Nil) }]
    in
    let env = Env.make ~binds () in
    is_macro_call ~env [Ast.Symbol "foo"]

  let%test "when foo is not macro" =
    let binds =
      ["foo", Ast.Fn { is_macro = false; body = (fun _ -> Ast.Nil) }]
    in
    let env = Env.make ~binds () in
    not @@ is_macro_call ~env [Ast.Symbol "foo"]

  let%test "when foo is not defined" =
    let env = Env.make () in
    not @@ is_macro_call ~env [Ast.Symbol "foo"]

  let%test "make binds" =
    let params = [Ast.Symbol "a"; Ast.Symbol "b"] in
    let args = [Ast.Int 1; Ast.String "foo"] in
    match make_binds params args with
    | ["a", Ast.Int 1; "b", Ast.String "foo"] -> true
    | _ -> false

  let%test "make variadic binds" =
    let params = [Ast.Symbol "a"; Ast.Symbol "&"; Ast.Symbol "b"] in
    let args = [Ast.Int 1; Ast.String "foo"; Ast.String "bar"] in
    match make_binds params args with
    | ["a", Ast.Int 1; "b", Ast.List [Ast.String "foo"; Ast.String "bar"]] -> true
    | _ -> false

end)
