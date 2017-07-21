open Core

let foldl1 f lst = match lst with
  | [] -> failwith "Empty list"
  | x :: xs -> List.fold_left lst ~init:x ~f:f
;;
  
let plus nums = 
  List.fold nums ~init:0  ~f:(fun accum expr -> 
      accum + (Types.expr_to_int expr))
;;

let minus nums = match nums with
  | [] -> 0
  | hd :: tl -> Types.expr_to_int hd - plus tl
;;

let mul nums = 
  List.fold nums ~init:1  ~f:(fun accum expr -> 
      accum * (Types.expr_to_int expr))
;;

let cons e1 e2 = Types.Pair (e1, e2)

let car expr = 
  match expr with
  | Types.Pair (e1, e2) -> e1
  | _             -> failwith "Non pair passed to car"

let cdr expr = 
  match expr with
  | Types.Pair (e1, e2) -> e2
  | _             -> failwith "Non pair passed to cdr"

let list_ exprs = 
  List.fold_right exprs ~init:Types.Nill ~f:cons

let rec equal (exprs : Types.expr List.t) : bool = 
  match exprs with
  | e1 :: e2 :: [] ->
    (match e1, e2 with
    | Atom (Int i1), Atom (Int i2) -> i1 = i2
    | Atom (Bool b1), Atom (Bool b2) -> b1 = b2
(*     | Symbol s1, Symbol s1 -> s1 = s2 *)
    | List l1, List l2 ->
      (match l1, l2 with
       | [], [] -> true
       | _ :: _, [] | [], _ :: _ -> false
       | hd1 :: tl1, hd2 :: tl2 -> 
         (equal [hd1; hd2]) && (equal [Types.List tl1; Types.List tl2]))
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
;;

let rec logical_and (exprs : Types.expr List.t) : bool =
  match exprs with
  | [] -> true
  | Atom (Bool b) :: tl -> b && logical_and tl
  | _ :: tl -> logical_and tl
;;

let rec logical_or (exprs : Types.expr List.t) : bool =
  match exprs with
  | [] -> false
  | Atom (Bool b) :: tl -> b || logical_and tl
  | _ -> false
;;

let rec compare ~cmp (exprs : Types.expr List.t) =
  match exprs with
  | [] -> true
  | [ Atom (Int _) ] -> true
  | (Atom (Int i1)) :: (Atom (Int i2)) :: tl ->
    (cmp i1 i2) && (compare ~cmp (Atom (Int i2) :: tl))
| _ -> failwith "invalid arguments"
;;

(* let int_to_expr f ints = Types.Func (Types.Atom (Types.Int (f ints))) *)

let int_to_expr f = Types.Func (fun ints -> Types.Atom (Types.Int (f ints)))

let bool_to_expr f = Types.Func (fun exprs -> Types.Atom (Types.Bool (f exprs)))

let less_than             = compare ~cmp:Int.(<)
let less_than_equal_to    = compare ~cmp:Int.(<=)
let greater_than          = compare ~cmp:Int.(>)
let greater_than_equal_to = compare ~cmp:Int.(>=)
let equal_to              = compare ~cmp:Int.(=)

let new_env () =
  let built_in_funcs = [
    ("+"      ,(int_to_expr plus));
    ("*"      ,(int_to_expr mul));
    ("-"      ,(int_to_expr minus));
    ("equal?" ,(bool_to_expr equal));
    ("and"    ,(bool_to_expr logical_and));
    ("or"     ,(bool_to_expr logical_or));
    ("<"      ,(bool_to_expr less_than));
    ("<="     ,(bool_to_expr less_than_equal_to));
    (">"      ,(bool_to_expr greater_than));
    (">="     ,(bool_to_expr greater_than_equal_to));
    ("="      ,(bool_to_expr equal_to));] in 
  [String.Table.of_alist_exn built_in_funcs]
;;
