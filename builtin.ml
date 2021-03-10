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

let make_list =
  let _list elements = Ast.List elements in
  Ast.Fn _list

let is_list =
  let _is_list = function
    | Ast.List _ :: _ -> Ast.Bool true
    | _ -> Ast.Bool false
  in
  Ast.Fn _is_list
