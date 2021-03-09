open Base
open Printf

let rec print_str = function
  | Ast.Symbol x -> x
  | Ast.Int x -> Int.to_string x
  | Ast.Bool x -> if x then "true" else "false"
  | Ast.Nil -> "nil"
  | Ast.Fn _ -> "<function>"
  | Ast.List lst ->
      List.map lst ~f:(fun x -> print_str x)
      |> String.concat ~sep:" "
      |> sprintf "(%s)"

