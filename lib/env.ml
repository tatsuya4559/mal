open Core

type t = {
  mutable store : (string, Ast.t, String.comparator_witness) Map.t;
  outer: t option;
}

let rec get t key =
  let open Option.Monad_infix in
  match Map.find t.store key with
  | Some value -> Some value
  | None -> t.outer >>= fun outer -> get outer key

let set t key value =
  let store = Map.update t.store key ~f:(fun _ -> value) in
  t.store <- store

let make ?(binds=[]) () =
  let store = Map.empty (module String) in
  let env = { store; outer = None } in
  List.iter binds ~f:(fun (key, value) -> set env key value);
  env

let enclose ?(binds=[]) t =
  let store = Map.empty (module String) in
  let env = { store; outer = Some t } in
  List.iter binds ~f:(fun (key, value) -> set env key value);
  env
