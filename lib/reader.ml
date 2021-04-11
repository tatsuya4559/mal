module Lexer : sig
  (** lexer state *)
  type t

  (** create new state from token array *)
  val create : string array -> t

  (** next returns the token at the current position and increments the position. *)
  val next : t -> string option

  (** peek just returns the token at the current position. *)
  val peek : t -> string option
end = struct
  type t = {
    tokens: string array;
    mutable curr_position: int;
  }

  let create tokens =
    { tokens; curr_position = 0 }

  let next ({tokens; curr_position} as t) =
    if curr_position >= Array.length tokens then None
    else begin
      t.curr_position <- curr_position + 1;
      Some tokens.(curr_position)
    end

  let peek {tokens; curr_position} =
    if curr_position >= Array.length tokens then None
    else Some tokens.(curr_position)
end

(** regular expression (PCRE) that will match all mal tokens *)
let rex = Pcre.regexp {|[\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)|}

(** tokenize takes a single string and return an array of all the tokens (strings) in it. *)
let tokenize s =
  let open Batteries in
  Pcre.extract_all ~rex s
  |> Array.map (fun x -> x.(1))
  |> Array.filter (fun x -> not (String.starts_with x ";")) (* remove comment *)
  |> fun x -> ArrayLabels.sub x ~pos:0 ~len:(Array.length x - 1) (* remove trailing empty string *)

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
  BatString.strip ~chars:{|"|} s
  |> Util.Strings.replace_all ~sub:{|\"|} ~by:{|"|}
  |> Util.Strings.replace_all ~sub:{|\n|} ~by:"\n"
  |> Util.Strings.replace_all ~sub:{|\\|} ~by:{|\|}

(** look at the contents of the token and return the appropriate scalar
    (simple/single) data type value. *)
let rec read_atom t =
  match Lexer.next t with
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
    match Lexer.peek t with
    | Some ")" -> ignore(Lexer.next t); acc_list (* consume ")" *)
    | _ -> read_element t ((read_form t) :: acc_list)
  in
  ignore(Lexer.next t); (* consume "(" *)
  Ast.List (List.rev (read_element t []))

and read_hashmap t =
  let rec read_element t hashmap =
    match Lexer.peek t with
    | Some "}" -> ignore(Lexer.next t); hashmap
    | _ ->
        let key =
          match read_form t with
          | Ast.List _ -> failwith "cannot use list as a hashmap key"
          | Ast.Nil -> failwith "cannot use nil as a hashmap key"
          | Ast.Fn _ -> failwith "cannot use function as a hashmap key"
          | Ast.Atom _ -> failwith "cannot use atom as a hashmap key"
          | Ast.Hash_map _ -> failwith "cannot use hashmap as a hashmap key"
          | Ast.Int _ | Ast.String _ | Ast.Symbol _ | Ast.Bool _ | Ast.Keyword _ as k -> k
        in
        let value = read_form t in
        Hashtbl.add hashmap key value;
        read_element t hashmap
  in
  ignore(Lexer.next t);
  Ast.Hash_map (read_element t (Hashtbl.create 1))

(** peek at the first token in the lexer object and switch on the first
    character of that token. *)
and read_form t =
  match Lexer.peek t with
  | None -> failwith "got EOF while parsing"
  | Some "(" -> read_list t
  | Some "{" -> read_hashmap t
  | Some _ -> read_atom t

(** tokenize a given string and then convert to Ast.t. *)
let read_str str =
  let tokens = tokenize str in
  if Array.length tokens = 0 then Ast.Nil else
  let t = Lexer.create tokens in
  read_form t
