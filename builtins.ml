open Core

(* built in int functions  *)
let plus lst = List.fold lst ~f:(fun x y -> x + y) ~init:0
let minus lst = 2 * List.hd_exn lst - (List.fold lst ~f:(fun x y -> x + y) ~init:0)
let mul lst = List.fold lst ~f:(fun x y -> x * y) ~init:1

let cons e1 e2 = Types.Pair (e1, e2)

let car expr = 
  match expr with
  | Types.Pair (e1, e2) -> e1
  | _             -> failwith "Non pair passed to car"

let cdr expr = 
  match expr with
  | Types.Pair (e1, e2) -> e2
  | _             -> failwith "Non pair passed to cdr"

let rec equal (exprs : Types.expr List.t) : bool = 
  match exprs with
  | e1 :: e2 :: [] ->
    (match e1, e2 with
    | Atom (Int i1), Atom (Int i2) -> i1 = i2
    | Atom (Bool b1), Atom (Bool b2) -> b1 = b2
    | Symbol s1, Symbol s2 -> s1 = s2
    | Pair (e11, e12), Pair (e21, e22) ->
      (match (e11, e12), (e21, e22) with
       | (e11, Types.Nill), (e21, Types.Nill) -> (equal [e11; e12])
       | (Types.Nill, e12), (Types.Nill, e22) -> (equal [e12; e22])
       | (_, Types.Nill), (Types.Nill, _) 
       | (Types.Nill, _), (_, Types.Nill) -> false
       | (e11, e12), (e21, e22) -> 
         (equal [e11; e12]) && (equal [e12; e22]))
    | _, _ -> false)
  | _ -> failwith "equals called with wrong number of arguments"

let rec scheme_and (exprs : Types.expr List.t) : Types.expr =
  match exprs with
  | [] -> Atom (Bool true)
  | hd :: [] -> hd
  | Atom (Bool false) :: tl -> Atom (Bool false)
  | _ :: tl -> scheme_and tl

let rec scheme_or (exprs : Types.expr List.t) : Types.expr =
  match exprs with
  | [] -> Atom (Bool false)
  | Atom (Bool false) :: tl -> scheme_or tl
  | hd :: _ -> hd

let rec compare ~cmp (exprs : Types.expr List.t) =
  match exprs with
  | [] -> true
  | [Atom (Int _)] -> true
  | (Atom (Int i1)) :: (Atom (Int i2)) :: tl ->
    (cmp i1 i2) && (compare ~cmp (Atom (Int i2) :: tl))
| _ -> failwith "invalid arguments"

let bool_return_to_expr name f = Types.Func (name, (fun exprs -> Types.Atom (Types.Bool (f exprs))))
let int_fn_to_expr_fn name f = Types.Func (name, (fun exprs -> Types.int_to_expr (f (List.map exprs Types.expr_to_int))))

let less_than             = compare ~cmp:Int.(<)
let less_than_equal_to    = compare ~cmp:Int.(<=)
let greater_than          = compare ~cmp:Int.(>)
let greater_than_equal_to = compare ~cmp:Int.(>=)
let equal_to              = compare ~cmp:Int.(=)

let new_env () : Types.expr Environment.t = 
  let built_in_funcs = [
    ("+"      ,(int_fn_to_expr_fn "+" plus));
    ("*"      ,(int_fn_to_expr_fn "*" mul));
    ("-"      ,(int_fn_to_expr_fn "-" minus));
    ("equal?" ,(bool_return_to_expr "equal?" equal));
    ("<"      ,(bool_return_to_expr "<" less_than));
    ("<="     ,(bool_return_to_expr "<=" less_than_equal_to));
    (">"      ,(bool_return_to_expr ">" greater_than));
    (">="     ,(bool_return_to_expr ">=" greater_than_equal_to));
    ("="      ,(bool_return_to_expr "=" equal_to));
    ("and"    ,Types.Func ("and", scheme_and));
    ("or"     ,Types.Func ("or", scheme_or));
    ("list"   ,Types.Func ("list", Types.scheme_list));
    ("cons"   ,Types.Func ("cons", Types.cons));] in 
  Environment.of_alist_exn built_in_funcs
;;
