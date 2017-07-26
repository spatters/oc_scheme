open Core

type symbol = string

type atom = 
  | Int of int
  | Bool of bool

type expr =
  | Nill
  | Atom of atom
  | Symbol of symbol
  | Func of symbol * (expr List.t -> expr)
  | UserFunc of symbol * symbol List.t * expr List.t * expr Environment.t
  | Pair of expr * expr
  | If of expr * expr * expr
  | Define of definition 
  | Assign of definition 
  | Lambda of symbol List.t * expr List.t
  | Quote of expr

and definition = 
  | VarDef of symbol * expr
  | FuncDef of symbol * symbol List.t * expr List.t

let int_to_expr i = Atom (Int i)

let expr_to_int e = 
  match e with
  | Atom (Int i) -> i
  | _ -> failwith "Can't convert non-int"

let bool_to_expr b = Atom (Bool b)

let expr_to_bool e =
  match e with
  | Atom (Bool false) -> false
  | _ -> true

let func_to_expr name f = Func (name, f)

let rec scheme_list (exprs : expr List.t) = 
  match exprs with 
  | [] -> Nill
  | e :: es -> Pair (e, scheme_list es)

let rec dotted_list (exprs : expr List.t)  (last_expr : expr) = 
  match exprs with 
  | []      -> last_expr
  | e :: es -> Pair (e, dotted_list es last_expr)

let cons (exprs : expr List.t) = 
  match exprs with 
  | hd :: tl :: [] -> Pair (hd, tl)
  | _              -> failwith "cons called with more/less than two args"

let car expr =
  match expr with
  | Pair(hd, _) -> hd
  | _ -> failwith "Non-pair passed to car"

let cdr expr =
  match expr with
  | Pair(_, tl) -> tl
  | _ -> failwith "Non-pair passed to cdr"

let rec is_proper_list pair = 
  match pair with 
  | Pair (car, Nill) -> true
  | Pair (car, cdr) -> is_proper_list cdr
  | _ -> false

let rec list_to_pair (expr_list : expr List.t) = 
  match expr_list with
  | [] -> Nill
  | hd :: tl -> Pair (hd, list_to_pair tl)

let rec pair_to_list_top_level (pair : expr) = 
  match pair with
  | Nill -> []
  | Pair(hd, tl) -> hd :: (pair_to_list_top_level tl)
  | pair -> [pair]
(*   | _ -> failwith "Non-proper list passed to pair_to_list_top_level" *)

