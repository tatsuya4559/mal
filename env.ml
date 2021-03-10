open Base

module Assoc = List.Assoc

(* TODO: re-implement using Map. I don't understand functor yet... *)
type t = {
  mutable store: (string, Ast.t) Assoc.t;
  outer: t option;
}

let rec get t key =
  let open Option.Monad_infix in
  match Assoc.find ~equal:String.equal t.store key with
  | Some value -> Some value
  | None -> t.outer >>= fun outer -> get outer key


let set t key value =
  t.store <- Assoc.add ~equal:String.equal t.store key value

let make ?(binds=[]) () =
  let env = { store = []; outer = None } in
  List.iter binds ~f:(fun (key, value) -> set env key value);
  env

let enclose ?(binds=[]) t =
  let env = { store = []; outer = Some t } in
  List.iter binds ~f:(fun (key, value) -> set env key value);
  env
