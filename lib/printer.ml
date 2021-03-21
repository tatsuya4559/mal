open Core
open Printf

let escape s =
  String.substr_replace_all ~pattern:{|\|} ~with_:{|\\|} s
  |> String.substr_replace_all ~pattern:"\n" ~with_:{|\n|}
  |> String.substr_replace_all ~pattern:{|"|} ~with_:{|\"|}

let%test _ =
  let got = escape {|backslash:\,linefeed:
,doublequote:"|}
  in
  String.(got = {|backslash:\\,linefeed:\n,doublequote:\"|})

let rec print_str ?(readably=true) = function
  | Ast.Symbol x -> x
  | Ast.String x -> if readably then x else escape x
  | Ast.Int x -> Int.to_string x
  | Ast.Bool x -> if x then "true" else "false"
  | Ast.Nil -> ""
  | Ast.Fn _ -> "<function>"
  | Ast.List lst ->
      List.map lst ~f:(fun x -> print_str x)
      |> String.concat ~sep:" "
      |> sprintf "(%s)"
  | Ast.Atom ast -> print_str !ast
