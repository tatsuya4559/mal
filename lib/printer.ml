open Printf

let escape s =
  let open Batteries in
  String.replace_chars (function
    | '\\' -> {|\\|}
    | '\n' -> {|\n|}
    | '"' -> {|\"|}
    | x -> String.of_char x) s

let%test _ =
  let got = escape {|backslash:\,linefeed:
,doublequote:"|}
  in
  got = {|backslash:\\,linefeed:\n,doublequote:\"|}

let rec print_str ?(readably=true) = function
  | Ast.Symbol x -> x
  | Ast.String x -> if readably then x else escape x
  | Ast.Int x -> Int.to_string x
  | Ast.Bool x -> if x then "true" else "false"
  | Ast.Nil -> ""
  | Ast.Fn _ -> "<function>"
  | Ast.List lst ->
      List.map (fun x -> print_str x) lst
      |> String.concat " "
      |> sprintf "(%s)"
  | Ast.Atom ast -> print_str !ast
