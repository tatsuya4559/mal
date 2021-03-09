open Base

let add =
  let _add ast_list =
    let sum = List.fold ast_list ~init:0 ~f:(fun acc ast ->
      match ast with
      | Ast.Int x -> acc + x
      | _ -> failwith "not int" )
    in
    Ast.Int sum
  in
  Ast.Fn _add

  (*
let sub

let mul

let div
*)
