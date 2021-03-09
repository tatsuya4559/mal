open Base

(* TODO: re-implement using Map. I don't understand functor yet... *)
type t = (string, Ast.t) List.Assoc.t

let empty = []

let find t key =
  List.Assoc.find ~equal:String.equal t key

let set t key value =
  List.Assoc.add ~equal:String.equal t key value

let set_all t assoc_list =
  List.fold ~init:t ~f:(fun t (key, value) -> set t key value) assoc_list
