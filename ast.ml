type t =
  | List of t list
  | Int of int
  | String of string
  | Symbol of string
  | Bool of bool
  | Nil
  | Fn of (t list -> t)
  | Atom of t ref
