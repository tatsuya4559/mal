module Store = Map.Make(String)

type t = {
  mutable store : Ast.t Store.t;
  outer: t option;
}

let rec get t key =
  match Store.find_opt key t.store with
  | Some value -> Some value
  | None ->
      let open Option_monad in
      let* outer = t.outer in
      get outer key

let set t key value =
  let store = Store.update key (fun _ -> Some value) t.store in
  t.store <- store

let make ?(binds=[]) () =
  let store = Store.empty in
  let env = { store; outer = None } in
  List.iter (fun (key, value) -> set env key value) binds;
  env

let enclose ?(binds=[]) t =
  let store = Store.empty in
  let env = { store; outer = Some t } in
  List.iter (fun (key, value) -> set env key value) binds;
  env
