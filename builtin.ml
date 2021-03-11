open Base
open Stdio

let add =
  let _add ast_list =
    let sum = List.fold ast_list ~init:0 ~f:(fun acc ast ->
      match ast with
      | Ast.Int x -> acc + x
      | _ -> failwith "not int" )
    in
    Ast.Int sum
  in
  Ast.Fn _add

let sub =
  let _sub = function
    | [] -> failwith "no operand"
    | Ast.Int init :: tl ->
      let diff = List.fold tl ~init ~f:(fun acc ast ->
        match ast with
        | Ast.Int x -> acc - x
        | _ -> failwith "not int" )
      in
      Ast.Int diff
    | _ -> failwith "first argument is not a int"
  in
  Ast.Fn _sub

let mul =
  let _mul ast_list =
    let product = List.fold ast_list ~init:1 ~f:(fun acc ast ->
      match ast with
      | Ast.Int x -> acc * x
      | _ -> failwith "not int" )
    in
    Ast.Int product
  in
  Ast.Fn _mul

let div =
  let _div = function
    | [] -> failwith "no operand"
    | Ast.Int init :: tl ->
      let quotient = List.fold tl ~init ~f:(fun acc ast ->
        match ast with
        | Ast.Int x -> acc / x
        | _ -> failwith "not int" )
      in
      Ast.Int quotient
    | _ -> failwith "first argument is not a int"
  in
  Ast.Fn _div

(** take the parameters and return them as a list. *)
let make_list =
  let _list elements = Ast.List elements in
  Ast.Fn _list

(** return true if the first parameter is a list, false otherwise. *)
let is_list =
  let _is_list = function
    | [] -> failwith "no arguments"
    | Ast.List _ :: _ -> Ast.Bool true
    | _ -> Ast.Bool false
  in
  Ast.Fn _is_list

(** treat the first parameter as a list and return true if the list is
    empty and false if it contains any elements. *)
let is_empty_list =
  let _is_empty_list = function
    | [] -> failwith "no arguments"
    | Ast.List [] :: _ -> Ast.Bool true
    | _ -> Ast.Bool false
  in
  Ast.Fn _is_empty_list

(** treat the first parameter as a list and return the number of
    elements that it contains. *)
let count =
  let _count = function
    | Ast.List lst :: _ -> Ast.Int (List.length lst)
    | _ -> failwith "count takes a list as argument"
  in
  Ast.Fn _count

(** compare the first two parameters and return true if they are the
    same type and contain the same value. In the case of equal length lists,
    each element of the list should be compared for equality and if they are
    the same return true, otherwise false. *)
let equal =
  let rec _equal = function
    | Ast.Nil :: Ast.Nil :: _ -> true
    | Ast.Bool a :: Ast.Bool b :: _ -> Bool.(a = b)
    | Ast.String a :: Ast.String b :: _ -> String.(a = b)
    | Ast.Int a :: Ast.Int b :: _ -> a = b
    | Ast.List a :: Ast.List b :: _ ->
        if List.length a <> List.length b then
          false
        else
          (* cannot raise exn because lengths have been checked *)
          List.for_all2_exn a b ~f:(fun x y -> _equal [x; y])
    | Ast.Fn _ :: Ast.Fn _ :: _ -> failwith "cannot compare function value"
    | _ -> false
  in
  Ast.Fn (fun x -> Ast.Bool (_equal x))

let lt =
  let _lt = function
    | Ast.Int a :: Ast.Int b :: _ -> a < b
    | _ -> failwith "cannot compare"
  in
  Ast.Fn (fun x -> Ast.Bool (_lt x))

let gt =
  let _gt = function
    | Ast.Int a :: Ast.Int b :: _ -> a > b
    | _ -> failwith "cannot compare"
  in
  Ast.Fn (fun x -> Ast.Bool (_gt x))

let lte =
  let _lte = function
    | Ast.Int a :: Ast.Int b :: _ -> a <= b
    | _ -> failwith "cannot compare"
  in
  Ast.Fn (fun x -> Ast.Bool (_lte x))

let gte =
  let _gte = function
    | Ast.Int a :: Ast.Int b :: _ -> a >= b
    | _ -> failwith "cannot compare"
  in
  Ast.Fn (fun x -> Ast.Bool (_gte x))

let prn =
  let open Out_channel in
  let _prn ast_list =
    List.map ast_list ~f:(fun x -> Printer.print_str x)
    |> String.concat ~sep:" "
    |> printf "%s%!";
    Ast.Nil
  in
  Ast.Fn _prn

let println =
  let open Out_channel in
  let _println ast_list =
    List.map ast_list ~f:(fun x -> Printer.print_str ~readably:false x)
    |> String.concat ~sep:" "
    |> printf "%s%!";
    Ast.Nil
  in
  Ast.Fn _println

let str =
  let _str ast_list =
    let s = List.map ast_list ~f:(fun x -> Printer.print_str x)
    |> String.concat ~sep:" " in
    Ast.String s
  in
  Ast.Fn _str

(** this function just exposes the read_str function from the reader. *)
let read_string =
  let _read_string = function
    | Ast.String s :: _ -> Reader.read_str s
    | _ -> failwith "argument must be type of string"
  in
  Ast.Fn _read_string

(** this function takes a file name (string) and returns the contents of
    the file as a string. *)
let slurp =
  let _slurp = function
    | Ast.String filename :: _ ->
        let open In_channel in
        Ast.String (read_all filename)
    | _ -> failwith "argument must be type of string"
  in
  Ast.Fn _slurp

(** Takes a Mal value and returns a new atom which points to that Mal value. *)
let atom =
  let _atom = function
    | [ast] -> Ast.Atom (ref ast)
    | _ -> failwith "wrong number of arguments to 'atom'"
  in
  Ast.Fn _atom

(** Takes an argument and returns true if the argument is an atom. *)
let is_atom =
  let _is_atom = function
    | [Ast.Atom _] -> Ast.Bool true
    | [_] -> Ast.Bool false
    | _ -> failwith "wrong number of arguments to 'atom'"
  in
  Ast.Fn _is_atom

(** Takes an atom argument and returns the Mal value referenced by this atom. *)
let deref =
  let _deref = function
    | [Ast.Atom x] -> !x
    | [_] -> failwith "argument must be atom"
    | _ -> failwith "wrong number of arguments to 'atom'"
  in
  Ast.Fn _deref

(** Takes an atom and a Mal value; the atom is modified to refer to the
    given Mal value. The Mal value is returned. *)
let reset =
  let _reset = function
    | Ast.Atom x :: value :: [] -> x := value; value
    | _ :: _ :: [] -> failwith "first argument must be atom"
    | _ -> failwith "wrong number of arguments to 'atom'"
  in
  Ast.Fn _reset

(** Takes an atom, a function, and zero or more function arguments. The
    atom's value is modified to the result of applying the function with the
    atom's value as the first argument and the optionally given function
    arguments as the rest of the arguments. *)
let swap =
  let _swap = function
    | Ast.Atom x :: Ast.Fn fn :: args ->
        x := fn (!x :: args);
        !x
    | _ -> failwith "the first arg must be an atom and the second must be a function"
  in
  Ast.Fn _swap

let fns = [
  "+", add;
  "-", sub;
  "*", mul;
  "/", div;
  "list", make_list;
  "list?", is_list;
  "empty?", is_empty_list;
  "count", count;
  "=", equal;
  "<", lt;
  ">", gt;
  "<=", lte;
  ">=", gte;
  "prn", prn;
  "println", println;
  "str", str;
  "read-string", read_string;
  "slurp", slurp;
  "atom", atom;
  "atom?", is_atom;
  "deref", deref;
  "reset!", reset;
  "swap!", swap;
]

let make_eval env =
  let _eval = function
    | ast :: []  -> Evaluator.eval ast ~env
    | _ -> failwith "wrong number of arguments to 'eval'";
  in
  Ast.Fn _eval
