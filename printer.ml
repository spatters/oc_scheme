open Core

let output_atom outc (atom : Types.atom) = 
  match atom with
  | Int i -> printf "%d" i
  | Bool b -> match b with
    | true -> printf "#t"
    | false -> printf "#f"

let rec output_value outc (expr : Types.expr) = 
  match expr with
  | Atom a         -> output_atom outc a
  | Func f         -> printf "FUNCTION"
  | UserFunc _       -> printf "USER DEFINED FUNCTION"
  | Symbol s       -> printf "%s" s

(*
  | Float x    -> printf "%f" x
*)
  | List l     -> print_list outc l
  | If (p, c, a)     -> print_list outc [Types.Symbol "if"; p; c; a;]
  | Define defn  -> 
    (match defn with
    | Types.VarDef (s, expr) ->
      print_list outc [Types.Symbol "define"; Types.Symbol s; expr]
    | Types.FuncDef (name, args, body) -> 
      print_list 
        outc 
        ([Types.Symbol "define"; Types.List ([Types.Symbol name] @ List.map args (fun a -> Types.Symbol a))] @ body))
  | Lambda (args, body) -> 
    print_list outc ([Types.Symbol "lambda"; Types.List (List.map args (fun a -> Types.Symbol a))] @ body)


and print_list outc arr =
  Out_channel.output_string outc "(";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc " ";
      output_value outc v) arr;
  Out_channel.output_string outc ")"



