(** state of lexer *)
type t = {
  tokens: string array;
  mutable curr_position: int;
}

(** next returns the token at the current position and increments the position. *)
let next ({tokens; curr_position} as t) =
  if curr_position >= Array.length tokens
  then None
  else begin
    t.curr_position <- curr_position + 1;
    Some tokens.(curr_position)
  end

(** peek just returns the token at the current position. *)
let peek {tokens; curr_position} =
  if curr_position >= Array.length tokens
  then None
  else Some tokens.(curr_position)


(** regular expression (PCRE) that will match all mal tokens *)
let rex = Pcre.regexp {|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|}

(** tokenize takes a single string and return an array of all the tokens (strings) in it. *)
let tokenize s =
  let open Batteries in
  Pcre.extract_all ~rex s
  |> Array.map (fun x -> x.(1))
  |> Array.filter (fun x -> not (String.starts_with x ";")) (* remove comment *)
  |> fun x -> Array.sub x 0 (Array.length x - 1) (* remove trailing empty string *)

let is_numeric s =
  try ignore(int_of_string s); true
  with Failure _ -> false

let%test _ = is_numeric "12"
let%test _ = not (is_numeric "foo")

let is_string s =
  let open Batteries in
  String.starts_with s "\""
  && String.ends_with s "\""

let%test _ = is_string {|"foo"|}
let%test _ = not (is_string {|"foo|})
let%test _ = not (is_string {|foo"|})
let%test _ = not (is_string {|foo|})

let is_keyword s =
  BatString.starts_with s ":"

let unescape s =
  let open Batteries in
  let s = String.strip ~chars:{|"|} s in
  let s = String.nreplace ~str:s ~sub:{|\"|} ~by:{|"|} in
  let s = String.nreplace ~str:s ~sub:{|\n|} ~by:"\n" in
  String.nreplace ~str:s ~sub:{|\\|} ~by:{|\|}

(** look at the contents of the token and return the appropriate scalar
    (simple/single) data type value. *)
let rec read_atom t =
  match next t with
  | None -> assert false
  | Some "true" -> Ast.Bool true
  | Some "false" -> Ast.Bool false
  | Some "nil" -> Ast.Nil
  (* a reader macro @ which will serve as a short form for deref. *)
  | Some "@" -> Ast.List [Ast.Symbol "deref"; read_form t]
  (* a reader macro ' which will serve as a short form for quote. *)
  | Some "'" -> Ast.List [Ast.Symbol "quote"; read_form t]
  (* a reader macro ` which will serve as a short form for quasiquote. *)
  | Some "`" -> Ast.List [Ast.Symbol "quasiquote"; read_form t]
  (* a reader macro ~ which will serve as a short form for unquote. *)
  | Some "~" -> Ast.List [Ast.Symbol "unquote"; read_form t]
  (* a reader macro ~@ which will serve as a short form for splice-unquote. *)
  | Some "~@" -> Ast.List [Ast.Symbol "splice-unquote"; read_form t]
  | Some x when is_numeric x -> Ast.Int (int_of_string x)
  | Some x when is_string x -> Ast.String (unescape x)
  | Some x when is_keyword x -> Ast.Keyword x
  | Some x -> Ast.Symbol x

(** repeatedly call read_form with the lexer object until it encounters
    a ')' token (if it reach EOF before reading a ')' then that is an error). *)
and read_list t =
  let rec read_element t acc_list =
    match peek t with
    | Some ")" -> ignore(next t); acc_list (* consume ")" *)
    | _ -> read_element t ((read_form t) :: acc_list)
  in
  ignore(next t); (* consume "(" *)
  Ast.List (List.rev (read_element t []))

(** peek at the first token in the lexer object and switch on the first
    character of that token. *)
and read_form t =
  match peek t with
  | None -> failwith "got EOF while parsing"
  | Some "(" -> read_list t
  | Some _ -> read_atom t

(** tokenize a given string and then convert to Ast.t. *)
let read_str str =
  let tokens = tokenize str in
  if Array.length tokens = 0 then Ast.Nil else
  let t = { tokens; curr_position = 0 } in
  read_form t
