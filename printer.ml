open Base
open Printf

let rec print_str = function
  | Type.Symbol x -> x
  | Type.Int x -> Int.to_string x
  | Type.Bool x -> if x then "true" else "false"
  | Type.Nil -> "nil"
  | Type.List lst ->
      List.map lst ~f:(fun x -> print_str x)
      |> String.concat ~sep:" "
      |> sprintf "(%s)"

