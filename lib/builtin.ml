open Core

let add ast_list =
  let sum = List.fold ast_list ~init:0 ~f:(fun acc ast ->
    match ast with
    | Ast.Int x -> acc + x
    | _ -> failwith "not int" )
  in
  Ast.Int sum

let sub = function
  | [] -> failwith "no operand"
  | Ast.Int init :: tl ->
    let diff = List.fold tl ~init ~f:(fun acc ast ->
      match ast with
      | Ast.Int x -> acc - x
      | _ -> failwith "not int" )
    in
    Ast.Int diff
  | _ -> failwith "first argument is not a int"

let mul ast_list =
  let product = List.fold ast_list ~init:1 ~f:(fun acc ast ->
    match ast with
    | Ast.Int x -> acc * x
    | _ -> failwith "not int" )
  in
  Ast.Int product

let div = function
  | [] -> failwith "no operand"
  | Ast.Int init :: tl ->
    let quotient = List.fold tl ~init ~f:(fun acc ast ->
      match ast with
      | Ast.Int x -> acc / x
      | _ -> failwith "not int" )
    in
    Ast.Int quotient
  | _ -> failwith "first argument is not a int"

(** take the parameters and return them as a list. *)
let make_list elements = Ast.List elements

(** return true if the first parameter is a list, false otherwise. *)
let is_list = function
  | [] -> failwith "no arguments"
  | Ast.List _ :: _ -> Ast.Bool true
  | _ -> Ast.Bool false

(** treat the first parameter as a list and return true if the list is
    empty and false if it contains any elements. *)
let is_empty_list = function
  | [] -> failwith "no arguments"
  | Ast.List [] :: _ -> Ast.Bool true
  | _ -> Ast.Bool false

(** treat the first parameter as a list and return the number of
    elements that it contains. *)
let count = function
  | Ast.List lst :: _ -> Ast.Int (List.length lst)
  | _ -> failwith "count takes a list as argument"

(** compare the first two parameters and return true if they are the
    same type and contain the same value. In the case of equal length lists,
    each element of the list should be compared for equality and if they are
    the same return true, otherwise false. *)
let equal args =
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
  Ast.Bool (_equal args)

let lt args =
  let _lt = function
    | Ast.Int a :: Ast.Int b :: _ -> a < b
    | _ -> failwith "cannot compare"
  in
  Ast.Bool (_lt args)

let gt args =
  let _gt = function
    | Ast.Int a :: Ast.Int b :: _ -> a > b
    | _ -> failwith "cannot compare"
  in
  Ast.Bool (_gt args)

let lte args =
  let _lte = function
    | Ast.Int a :: Ast.Int b :: _ -> a <= b
    | _ -> failwith "cannot compare"
  in
  Ast.Bool (_lte args)

let gte args =
  let _gte = function
    | Ast.Int a :: Ast.Int b :: _ -> a >= b
    | _ -> failwith "cannot compare"
  in
  Ast.Bool (_gte args)

let prn ast_list =
  let open Out_channel in
  List.map ast_list ~f:(fun x -> Printer.print_str x)
  |> String.concat ~sep:" "
  |> printf "%s%!";
  Ast.Nil

let println ast_list =
  let open Out_channel in
  List.map ast_list ~f:(fun x -> Printer.print_str ~readably:false x)
  |> String.concat ~sep:" "
  |> printf "%s%!";
  Ast.Nil

let str ast_list =
  let s = List.map ast_list ~f:(fun x -> Printer.print_str x)
  |> String.concat ~sep:" " in
  Ast.String s

(** this function just exposes the read_str function from the reader. *)
let read_string = function
  | Ast.String s :: _ -> Reader.read_str s
  | _ -> failwith "argument must be type of string"

(** this function takes a file name (string) and returns the contents of
    the file as a string. *)
let slurp = function
  | Ast.String filename :: _ ->
      let open In_channel in
      Ast.String (read_all filename)
  | _ -> failwith "argument must be type of string"

(** Takes a Mal value and returns a new atom which points to that Mal value. *)
let atom = function
  | [ast] -> Ast.Atom (ref ast)
  | _ -> failwith "wrong number of arguments to 'atom'"

(** Takes an argument and returns true if the argument is an atom. *)
let is_atom = function
  | [Ast.Atom _] -> Ast.Bool true
  | [_] -> Ast.Bool false
  | _ -> failwith "wrong number of arguments to 'atom'"

(** Takes an atom argument and returns the Mal value referenced by this atom. *)
let deref = function
  | [Ast.Atom x] -> !x
  | [_] -> failwith "argument must be atom"
  | _ -> failwith "wrong number of arguments to 'atom'"

(** Takes an atom and a Mal value; the atom is modified to refer to the
    given Mal value. The Mal value is returned. *)
let reset = function
  | Ast.Atom x :: value :: [] -> x := value; value
  | _ :: _ :: [] -> failwith "first argument must be atom"
  | _ -> failwith "wrong number of arguments to 'atom'"

(** Takes an atom, a function, and zero or more function arguments. The
    atom's value is modified to the result of applying the function with the
    atom's value as the first argument and the optionally given function
    arguments as the rest of the arguments. *)
let swap = function
  | Ast.Atom x :: Ast.Fn fn :: args ->
      x := fn (!x :: args);
      !x
  | _ -> failwith "the first arg must be an atom and the second must be a function"

(** this function takes a list as its second parameter and returns a new
    list that has the first argument prepended to it. *)
let cons = function
  | x :: Ast.List lst :: [] ->
      Ast.List (x :: lst)
  | _ :: _ :: [] -> failwith "the second argument must be a list"
  | _ -> failwith "wrong number of arguments to 'cons'"

(** this functions takes 0 or more lists as parameters and returns a new
    list that is a concatenation of all the list parameters. *)
let rec concat = function
  | [] -> Ast.List []
  | (Ast.List _) as l :: [] -> l
  | Ast.List first :: Ast.List second :: rest ->
      concat @@ Ast.List (first @ second) :: rest
  | _ -> failwith "argument must be list"

let fns = [
  "+", Ast.Fn add;
  "-", Ast.Fn sub;
  "*", Ast.Fn mul;
  "/", Ast.Fn div;
  "list", Ast.Fn make_list;
  "list?", Ast.Fn is_list;
  "empty?", Ast.Fn is_empty_list;
  "count", Ast.Fn count;
  "=", Ast.Fn equal;
  "<", Ast.Fn lt;
  ">", Ast.Fn gt;
  "<=", Ast.Fn lte;
  ">=", Ast.Fn gte;
  "prn", Ast.Fn prn;
  "println", Ast.Fn println;
  "str", Ast.Fn str;
  "read-string", Ast.Fn read_string;
  "slurp", Ast.Fn slurp;
  "atom", Ast.Fn atom;
  "atom?", Ast.Fn is_atom;
  "deref", Ast.Fn deref;
  "reset!", Ast.Fn reset;
  "swap!", Ast.Fn swap;
  "cons", Ast.Fn cons;
  "concat", Ast.Fn concat;
]

let make_eval env =
  let _eval = function
    | ast :: []  -> Evaluator.eval ast ~env
    | _ -> failwith "wrong number of arguments to 'eval'";
  in
  Ast.Fn _eval

let%test_module "test builtin functions" = (module struct

  (* helpers *)
  let is_int i = function
    | Ast.Int x -> x = i
    | _ -> false

  let is_true = function
    | Ast.Bool true -> true
    | _ -> false

  let is_false = function
    | Ast.Bool false -> true
    | _ -> false

  (* calculations *)
  let%test "(= (+ 1 2) 3)" = add [Ast.Int 1; Ast.Int 2] |> is_int 3
  let%test "(= (+) 0)" = add [] |> is_int 0
  let%test "(= (- 5 2) 3)" = sub [Ast.Int 5; Ast.Int 2] |> is_int 3
  let%test "(= (* 2 3) 6)" = mul [Ast.Int 2; Ast.Int 3] |> is_int 6
  let%test "(= (*) 1)" = mul [] |> is_int 1
  let%test "(= (/ 6 3) 2)" = div [Ast.Int 6; Ast.Int 3] |> is_int 2

  (* list *)
  let%test "'() is a list" = is_list [Ast.List []] |> is_true
  let%test "3 is not a list" = is_list [Ast.Int 3] |> is_false
  let%test "'() is empty" = is_empty_list [Ast.List []] |> is_true
  let%test "'(3) is not empty" = is_empty_list [Ast.List [Ast.Int 3]] |> is_false
  let%test "(count '(1 2)) is 2" = count [Ast.List [Ast.Int 1; Ast.Int 2]] |> is_int 2

  (* equality *)
  let%test "nil = nil" = equal [Ast.Nil; Ast.Nil] |> is_true
  let%test "true = true" = equal [Ast.Bool true; Ast.Bool true] |> is_true
  let%test "false = false" = equal [Ast.Bool false; Ast.Bool false] |> is_true
  let%test "true <> false" = equal [Ast.Bool true; Ast.Bool false] |> is_false
  let%test "\"foo\" = \"foo\"" = equal [Ast.String "foo"; Ast.String "foo"] |> is_true
  let%test "\"foo\" <> \"bar\"" = equal [Ast.String "foo"; Ast.String "bar"] |> is_false
  let%test "3 = 3" = equal [Ast.Int 3; Ast.Int 3] |> is_true
  let%test "3 <> 4" = equal [Ast.Int 3; Ast.Int 4] |> is_false
  let%test "'(1 2) = '(1 2)" =
    equal [
      Ast.List [Ast.Int 1; Ast.Int 2];
      Ast.List [Ast.Int 1; Ast.Int 2];
    ] |> is_true
  let%test "'(1 2) <> '(1 3)" =
    equal [
      Ast.List [Ast.Int 1; Ast.Int 2];
      Ast.List [Ast.Int 1; Ast.Int 3];
    ] |> is_false
  let%test "3 <> false" = equal [Ast.Int 3; Ast.Bool false] |> is_false

  (* comparison *)
  let%test "3 < 4" = lt [Ast.Int 3; Ast.Int 4] |> is_true
  let%test "4 < 4" = lt [Ast.Int 4; Ast.Int 4] |> is_false
  let%test "4 < 3" = lt [Ast.Int 4; Ast.Int 3] |> is_false
  let%test "4 > 3" = gt [Ast.Int 4; Ast.Int 3] |> is_true
  let%test "4 > 4" = gt [Ast.Int 4; Ast.Int 4] |> is_false
  let%test "3 > 4" = gt [Ast.Int 3; Ast.Int 4] |> is_false
  let%test "3 <= 4" = lte [Ast.Int 3; Ast.Int 4] |> is_true
  let%test "4 <= 4" = lte [Ast.Int 4; Ast.Int 4] |> is_true
  let%test "4 <= 3" = lte [Ast.Int 4; Ast.Int 3] |> is_false
  let%test "4 >= 3" = gte [Ast.Int 4; Ast.Int 3] |> is_true
  let%test "4 >= 4" = gte [Ast.Int 4; Ast.Int 4] |> is_true
  let%test "3 >= 4" = gte [Ast.Int 3; Ast.Int 4] |> is_false

  let%test "(cons 1 '())" =
    match cons [Ast.Int 1; Ast.List []] with
    | Ast.List [Ast.Int 1] -> true
    | _ -> false

  let%test "(concat '(1 2) '(3 4))" =
    let lst1 = Ast.List [Ast.Int 1; Ast.Int 2] in
    let lst2 = Ast.List [Ast.Int 3; Ast.Int 4] in
    match concat [lst1; lst2] with
    | Ast.List [Ast.Int 1; Ast.Int 2; Ast.Int 3; Ast.Int 4] -> true
    | _ -> false

end)

