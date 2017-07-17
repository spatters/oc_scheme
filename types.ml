open Core

type symbol = string

type definition = VarDef of symbol | FuncDef of symbol * symbol List.t

type atom = 
  | Int of int
  | Bool of bool

type expr =
  | Atom of atom
  | Symbol of symbol
  | Func of (expr List.t -> expr)
  | UserFunc of symbol List.t * expr * env
  | List of expr List.t
  | If of expr * expr * expr
  | Define of definition * expr
  | Lambda of symbol List.t * expr

and env = (expr String.Map.t) List.t 

let int_to_expr i = Atom (Int i)

let expr_to_int e = 
  match e with
  | Atom (Int i) -> i
  | _ -> failwith "Can't convert non-int"

let func_to_expr f = Func f

