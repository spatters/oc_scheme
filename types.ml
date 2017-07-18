open Core

type symbol = string

type atom = 
  | Int of int
  | Bool of bool

type expr =
  | Atom of atom
  | Symbol of symbol
  | Func of (expr List.t -> expr)
(*   | UserFunc of symbol List.t * expr * env *)
  | UserFunc of user_func 
  | List of expr List.t
  | If of expr * expr * expr
  | Define of definition 
  | Lambda of symbol List.t * expr List.t

and env = (expr String.Map.t) List.t 
  
and definition = 
  | VarDef of symbol * expr
  | FuncDef of symbol * symbol List.t * expr List.t

and user_func = 
  {arg_names : symbol List.t;
   body : expr List.t;
   mutable environment : env }


let int_to_expr i = Atom (Int i)

let expr_to_int e = 
  match e with
  | Atom (Int i) -> i
  | _ -> failwith "Can't convert non-int"

let func_to_expr f = Func f

