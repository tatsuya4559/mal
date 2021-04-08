type t =
  | List of t list
  | Int of int
  | String of string
  | Symbol of string
  | Bool of bool
  | Nil
  | Fn of { is_macro: bool; body: (t list -> t) }
  | Atom of t ref
  | Keyword of string
  | Hash_map of (t, t) Hashtbl.t

let fn body = Fn { is_macro = false; body }

exception Mal_exception of t
