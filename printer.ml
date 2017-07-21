open Core

let output_atom outc (atom : Types.atom) = 
  match atom with
  | Int i -> printf "%d" i
  | Bool b -> match b with
    | true -> printf "#t"
    | false -> printf "#f"

let rec output_value outc (expr : Types.expr) = 
  match expr with
  | Nill           -> printf "()"
  | Atom a         -> output_atom outc a
  | Func f         -> printf "FUNCTION"
  | UserFunc _     -> printf "USER DEFINED FUNCTION"
  | Symbol s       -> printf "%s" s
  | List l         -> print_list outc l
  | Pair (e1, e2)         -> print_pair outc e1 e2
  | If (p, c, a)   -> print_list outc [Types.Symbol "if"; p; c; a;]
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

and print_pair outc e1 e2 = 
  Out_channel.output_string outc "(";
  match e1, e2 with 
  | e1, Nill -> 
    output_value outc e1;
    Out_channel.output_string outc ")"
  | e1, e2 -> 
    output_value outc e1;
    Out_channel.output_string outc " . ";
    output_value outc e2;
    Out_channel.output_string outc ")"



