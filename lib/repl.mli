val setup_env : unit -> Env.t

val set_argv : Env.t -> string list -> unit

val loop : Env.t -> 'a

val eval_file : env:Env.t -> string -> unit
