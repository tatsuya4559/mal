val add : Ast.t
val sub : Ast.t
val mul : Ast.t
val div : Ast.t

(** take the parameters and return them as a list. *)
val make_list : Ast.t

(** return true if the first parameter is a list, false otherwise. *)
val is_list : Ast.t

(** treat the first parameter as a list and return true if the list is
    empty and false if it contains any elements. *)
val is_empty_list : Ast.t
