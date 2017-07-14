open Core

type symbol = string

type atom = 
  | Int of int

type built_in_func = int List.t -> int

type expr =
  | Atom of atom
(*
  | Float of float
*)
  | Symbol of symbol
  | Func of built_in_func
(*   | String of string *)
  | List of expr List.t

type env = (expr String.Map.t) List.t

