(** type of environment. this is mutable! *)
type t

(** make a new environment *)
val make : unit -> t

(** make a enclosed environment in given one *)
val enclose : t -> t

val get : t -> string -> Ast.t option

val set : t -> string -> Ast.t -> unit
