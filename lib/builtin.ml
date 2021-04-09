open Printf

let add ast_list =
  let sum = List.fold_left (fun acc ast ->
    match ast with
    | Ast.Int x -> acc + x
    | _ -> failwith "not int" ) 0 ast_list
  in
  Ast.Int sum

let sub = function
  | [] -> failwith "no operand"
  | Ast.Int init :: tl ->
    let diff = List.fold_left (fun acc ast ->
      match ast with
      | Ast.Int x -> acc - x
      | _ -> failwith "not int" ) init tl
    in
    Ast.Int diff
  | _ -> failwith "first argument is not a int"

let mul ast_list =
  let product = List.fold_left (fun acc ast ->
    match ast with
    | Ast.Int x -> acc * x
    | _ -> failwith "not int" ) 1 ast_list
  in
  Ast.Int product

let div = function
  | [] -> failwith "no operand"
  | Ast.Int init :: tl ->
    let quotient = List.fold_left (fun acc ast ->
      match ast with
      | Ast.Int x -> acc / x
      | _ -> failwith "not int" ) init tl
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
    | Ast.Bool a :: Ast.Bool b :: _ -> a = b
    | Ast.String a :: Ast.String b :: _ -> a = b
    | Ast.Int a :: Ast.Int b :: _ -> a = b
    | Ast.List a :: Ast.List b :: _ ->
        if List.length a <> List.length b then
          false
        else
          (* cannot raise exn because lengths have been checked *)
          List.for_all2 (fun x y -> _equal [x; y]) a b
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
  List.map (fun x -> Printer.print_str x) ast_list
  |> String.concat " "
  |> printf "%s%!";
  Ast.Nil

let println ast_list =
  List.map (fun x -> Printer.print_str ~readably:false x) ast_list
  |> String.concat " "
  |> printf "%s%!";
  Ast.Nil

let str ast_list =
  let s = List.map (fun x -> Printer.print_str x) ast_list
  |> String.concat " " in
  Ast.String s

(** this function just exposes the read_str function from the reader. *)
let read_string = function
  | Ast.String s :: _ -> Reader.read_str s
  | _ -> failwith "argument must be type of string"

(** this function takes a file name (string) and returns the contents of
    the file as a string. *)
let slurp = function
  | Ast.String filename :: _ ->
      Ast.String (Util.File.read_all filename)
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
  | Ast.Atom x :: Ast.Fn { body; _ } :: args ->
      x := body (!x :: args);
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

let car = function
  | Ast.List (hd :: _) :: [] -> hd
  | _ -> failwith "argument must be list"

let cdr = function
  | Ast.List (_ :: tl) :: [] -> Ast.List tl
  | _ -> failwith "argument must be list"

(** takes a single argument and returns true (mal true value) if the
    argument is a symbol (mal symbol value). *)
let is_symbol = function
  | [Ast.Symbol _] -> Ast.Bool true
  | [_] -> Ast.Bool false
  | _ -> failwith "wrong number of arguments"

let throw = function
  | exn :: [] -> raise (Ast.Mal_exception exn)
  | _ -> failwith "wrong number of arguments"

(** takes a variable but even number of arguments and returns a new mal
    hash-map value with keys from the odd arguments and values from the even
    arguments respectively. This is basically the functional form of the {}
    reader literal syntax. *)
let hash_map args =
  let rec hash_map' hashmap = function
    | [] -> hashmap
    | _ :: [] -> failwith "wrong number of arguments"
    | Ast.List _ :: _ -> failwith "cannot use list as hashmap key"
    | Ast.Nil :: _ -> failwith "cannot use nil as hashmap key"
    | Ast.Fn _ :: _ -> failwith "cannot use function as hashmap key"
    | Ast.Atom _ :: _ -> failwith "cannot use atom as hashmap key"
    | Ast.Hash_map _ :: _ -> failwith "cannot use hashmap as hashmap key"
    | ((Ast.Bool _) as key) :: value :: tl
    | ((Ast.Int _) as key) :: value :: tl
    | ((Ast.String _) as key) :: value :: tl
    | ((Ast.Symbol _) as key) :: value :: tl
    | ((Ast.Keyword _) as key) :: value :: tl -> Hashtbl.add hashmap key value; hash_map' hashmap tl
  in
  let size = (List.length args) / 2 in
  Ast.Hash_map (hash_map' (Hashtbl.create size) args)

(** takes a single argument and returns true (mal true value) if the
    argument is a hash-map, otherwise returns false (mal false value). *)
let is_hash_map = function
  | Ast.Hash_map _ :: [] -> Ast.Bool true
  | _ -> Ast.Bool false

(** takes a hash-map as the first argument and the remaining arguments
    are odd/even key/value pairs to "associate" (merge) into the hash-map.
    Note that the original hash-map is unchanged (remember, mal values are
    immutable), and a new hash-map containing the old hash-maps key/values
    plus the merged key/value arguments is returned. *)
let assoc args =
  let rec merge hashmap = function
    | [] -> hashmap
    | [_] -> failwith "wrong number of arguments"
    | Ast.List _ :: _ -> failwith "cannot use list as hashmap key"
    | Ast.Nil :: _ -> failwith "cannot use nil as hashmap key"
    | Ast.Fn _ :: _ -> failwith "cannot use function as hashmap key"
    | Ast.Atom _ :: _ -> failwith "cannot use atom as hashmap key"
    | Ast.Hash_map _ :: _ -> failwith "cannot use hashmap as hashmap key"
    | ((Ast.Bool _) as key) :: value :: tl
    | ((Ast.Int _) as key) :: value :: tl
    | ((Ast.String _) as key) :: value :: tl
    | ((Ast.Symbol _) as key) :: value :: tl
    | ((Ast.Keyword _) as key) :: value :: tl ->
        Hashtbl.add hashmap key value; merge hashmap tl
  in
  match args with
  | Ast.Hash_map hashmap :: tl -> Ast.Hash_map (merge (Hashtbl.copy hashmap) tl)
  | _ -> failwith "first argument must be a hashmap"

(** takes a hash-map and a list of keys to remove from the hash-map.
    Again, note that the original hash-map is unchanged and a new hash-map
    with the keys removed is returned. Key arguments that do not exist in
    the hash-map are ignored. *)
let dissoc = function
  | Ast.Hash_map hashmap :: keys ->
      let hashmap = Hashtbl.copy hashmap in
      List.iter (fun key -> Hashtbl.remove hashmap key) keys;
      Ast.Hash_map hashmap
  | _ -> failwith "first argument must be a hashmap"

(** takes a hash-map and a key and returns the value of looking up that
    key in the hash-map. If the key is not found in the hash-map then nil is
    returned. *)
let get = function
  | Ast.Hash_map hashmap :: key :: [] ->
      Hashtbl.find_opt hashmap key |> Option.value ~default:Ast.Nil
  | _ -> failwith "get takes a hashmap and a key"

(** takes a hash-map and a key and returns true (mal true value) if the
    key exists in the hash-map and false (mal false value) otherwise. *)
let contains = function
  | Ast.Hash_map hashmap :: key :: [] ->
      Ast.Bool (Hashtbl.mem hashmap key)
  | _ -> failwith "contains? takes a hashmap and a key"

(** takes a hash-map and returns a list (mal list value) of all the keys in the hash-map. *)
let keys = function
  | Ast.Hash_map hashmap :: [] ->
      Ast.List (Hashtbl.to_seq_keys hashmap |> List.of_seq)
  | _ -> failwith "keys takes a hashmap"

(** takes a hash-map and returns a list (mal list value) of all the values in the hash-map. *)
let vals = function
  | Ast.Hash_map hashmap :: [] ->
      Ast.List (Hashtbl.to_seq_values hashmap |> List.of_seq)
  | _ -> failwith "vals takes a hashmap"

let fns = [
  "+", Ast.fn add;
  "-", Ast.fn sub;
  "*", Ast.fn mul;
  "/", Ast.fn div;
  "list", Ast.fn make_list;
  "list?", Ast.fn is_list;
  "empty?", Ast.fn is_empty_list;
  "count", Ast.fn count;
  "=", Ast.fn equal;
  "<", Ast.fn lt;
  ">", Ast.fn gt;
  "<=", Ast.fn lte;
  ">=", Ast.fn gte;
  "prn", Ast.fn prn;
  "println", Ast.fn println;
  "str", Ast.fn str;
  "read-string", Ast.fn read_string;
  "slurp", Ast.fn slurp;
  "atom", Ast.fn atom;
  "atom?", Ast.fn is_atom;
  "deref", Ast.fn deref;
  "reset!", Ast.fn reset;
  "swap!", Ast.fn swap;
  "cons", Ast.fn cons;
  "concat", Ast.fn concat;
  "car", Ast.fn car;
  "cdr", Ast.fn cdr;
  "symbol?", Ast.fn is_symbol;
  "throw", Ast.fn throw;
  "hash-map", Ast.fn hash_map;
  "map?", Ast.fn is_hash_map;
  "assoc", Ast.fn assoc;
  "dissoc", Ast.fn dissoc;
  "get", Ast.fn get;
  "contains?", Ast.fn contains;
  "keys", Ast.fn keys;
  "vals", Ast.fn vals;
]

let make_eval env =
  let _eval = function
    | ast :: []  -> Evaluator.eval ast ~env
    | _ -> failwith "wrong number of arguments to 'eval'";
  in
  Ast.fn _eval

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

  let%test "(car '(1 2 3))" =
    car [Ast.List [Ast.Int 1; Ast.Int 2; Ast.Int 3]]
    |> is_int 1

  let%test "(cdr '(1 2 3))" =
    match cdr [Ast.List [Ast.Int 1; Ast.Int 2; Ast.Int 3]] with
    | Ast.List [Ast.Int 2; Ast.Int 3] -> true
    | _ -> false

  let%test "(throw not-found)" =
    try throw [Ast.Symbol "not-found"] with
    | Ast.Mal_exception (Ast.Symbol "not-found") -> true
    | _ -> false

end)

