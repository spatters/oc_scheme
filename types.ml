open Core

type symbol = string

type atom = 
  | Int of int

(* type built_in_func = *)
(*   | Atom of atom *)
(*   | Func of int List.t -> int *)

type expr =
  | Atom of atom
(*
  | Float of float
*)
  | Symbol of symbol
  | Func of (expr List.t -> expr)
(*   | String of string *)
  | List of expr List.t

type env = (expr String.Map.t) List.t

let int_to_expr i = Atom (Int i)

let expr_to_int e = 
  match e with
  | Atom (Int i) -> i
  | _ -> failwith "Can't convert non-int"

let func_to_expr f = Func f

