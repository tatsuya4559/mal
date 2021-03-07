open Base
open Printf

let rec print_str = function
  | Type.Eof -> ""
  | Type.Symbol x -> x
  | Type.Int x -> Int.to_string x
  | Type.List lst ->
      let element_str = List.map lst ~f:(fun x -> print_str x) in
      sprintf "(%s)" (String.concat ~sep:" " element_str)

