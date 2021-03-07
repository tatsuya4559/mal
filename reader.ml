open Base

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
  let matches = Pcre.extract_all ~rex s in
  Array.map matches ~f:(fun x -> x.(1))

exception Cannot_parse

let is_numeric s =
  try ignore(Int.of_string s); true
  with Failure _ -> false

(** look at the contents of the token and return the appropriate scalar
    (simple/single) data type value. *)
let read_atom t =
  match next t with
  | None -> assert false
  | Some x ->
      if is_numeric x then Type.Int (Int.of_string x)
      else Type.Symbol x

(** repeatedly call read_form with the lexer object until it encounters
    a ')' token (if it reach EOF before reading a ')' then that is an error). *)
let rec read_list t =
  let rec iter t acc_list =
    match peek t with
    | Some ")" -> ignore(next t); acc_list (* consume ")" *)
    | _ -> iter t ((read_form t) :: acc_list)
  in
  ignore(next t); (* consume "(" *)
  Type.List (List.rev (iter t []))

(** peek at the first token in the lexer object and switch on the first
    character of that token. *)
and read_form t =
  match peek t with
  | None -> raise Cannot_parse (* got EOF while parsing *)
  | Some "(" -> read_list t
  | Some _ -> read_atom t

(** tokenize a given string and then convert to Type.t.
 *  raises Cannot_parse when got EOF while parsing input.*)
let read_str str =
  let tokens = tokenize str in
  let t = { tokens; curr_position = 0 } in
  read_form t
