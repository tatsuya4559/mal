open Base

module Assoc = List.Assoc

(* TODO: re-implement using Map. I don't understand functor yet... *)
type t = {
  mutable store: (string, Ast.t) Assoc.t;
  outer: t option;
}

let make () = { store = []; outer = None }

let enclose t = { store = []; outer = Some t }

let rec get t key =
  let open Option.Monad_infix in
  match Assoc.find ~equal:String.equal t.store key with
  | Some value -> Some value
  | None -> t.outer >>= fun outer -> get outer key


let set t key value =
  t.store <- Assoc.add ~equal:String.equal t.store key value
