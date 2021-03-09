(** type of environment *)
type t

(** empty environment *)
val empty : t

val get : t -> string -> Ast.t option

val set : t -> string -> Ast.t -> t

val set_all : t -> (string * Ast.t) list -> t
