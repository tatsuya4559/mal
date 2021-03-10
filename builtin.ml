open Base

let add =
  let _add ast_list =
    if (List.length ast_list) = 0 then failwith "first argument is not a int" else
    let sum = List.fold ast_list ~init:0 ~f:(fun acc ast ->
      match ast with
      | Ast.Int x -> acc + x
      | _ -> failwith "not int" )
    in
    Ast.Int sum
  in
  Ast.Fn _add

let sub =
  let _sub = function
    | [] -> failwith "no operand"
    | Ast.Int init :: tl ->
      let diff = List.fold tl ~init ~f:(fun acc ast ->
        match ast with
        | Ast.Int x -> acc - x
        | _ -> failwith "not int" )
      in
      Ast.Int diff
    | _ -> failwith "first argument is not a int"
  in
  Ast.Fn _sub

let mul =
  let _mul ast_list =
    if (List.length ast_list) = 0 then failwith "first argument is not a int" else
    let product = List.fold ast_list ~init:1 ~f:(fun acc ast ->
      match ast with
      | Ast.Int x -> acc * x
      | _ -> failwith "not int" )
    in
    Ast.Int product
  in
  Ast.Fn _mul

let div =
  let _div = function
    | [] -> failwith "no operand"
    | Ast.Int init :: tl ->
      let quotient = List.fold tl ~init ~f:(fun acc ast ->
        match ast with
        | Ast.Int x -> acc / x
        | _ -> failwith "not int" )
      in
      Ast.Int quotient
    | _ -> failwith "first argument is not a int"
  in
  Ast.Fn _div

(** take the parameters and return them as a list. *)
let make_list =
  let _list elements = Ast.List elements in
  Ast.Fn _list

(** return true if the first parameter is a list, false otherwise. *)
let is_list =
  let _is_list = function
    | [] -> failwith "no arguments"
    | Ast.List _ :: _ -> Ast.Bool true
    | _ -> Ast.Bool false
  in
  Ast.Fn _is_list

(** treat the first parameter as a list and return true if the list is
    empty and false if it contains any elements. *)
let is_empty_list =
  let _is_empty_list = function
    | [] -> failwith "no arguments"
    | Ast.List [] :: _ -> Ast.Bool true
    | _ -> Ast.Bool false
  in
  Ast.Fn _is_empty_list

(** treat the first parameter as a list and return the number of
    elements that it contains. *)
let count =
  let _count = function
    | Ast.List lst :: _ -> Ast.Int (List.length lst)
    | _ -> failwith "count takes a list as argument"
  in
  Ast.Fn _count

let fns = [
  "+", add;
  "-", sub;
  "*", mul;
  "/", div;
  "list", make_list;
  "list?", is_list;
  "empty?", is_empty_list;
  "count", count;
]
