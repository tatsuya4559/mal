(** type of environment. this is mutable! *)
type t

val get : t -> string -> Ast.t option

val set : t -> string -> Ast.t -> unit

(** make a new environment with given bindings *)
val make : ?binds:(string * Ast.t) list -> unit -> t

(** make a enclosed environment in given one *)
val enclose : ?binds:(string * Ast.t) list -> t -> t

val set_argv : t -> string list -> unit
