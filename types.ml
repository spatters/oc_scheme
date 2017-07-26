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
  | List of expr List.t
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

