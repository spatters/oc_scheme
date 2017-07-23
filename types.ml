open Core

type symbol = string

type atom = 
  | Int of int
  | Bool of bool

type expr =
  | Nill
  | Atom of atom
  | Symbol of symbol
  | Func of (expr List.t -> expr)
  | UserFunc of user_func 
  | List of expr List.t
  | Pair of expr * expr
  | If of expr * expr * expr
  | Define of definition 
  | Assign of definition 
  | Lambda of symbol List.t * expr List.t

and env = (expr String.Table.t) List.t 
  
and definition = 
  | VarDef of symbol * expr
  | FuncDef of symbol * symbol List.t * expr List.t

and user_func = 
  {arg_names : symbol List.t;
   body : expr List.t;
   environment : env }

let int_to_expr i = Atom (Int i)

let expr_to_int e = 
  match e with
  | Atom (Int i) -> i
  | _ -> failwith "Can't convert non-int"

let func_to_expr f = Func f

