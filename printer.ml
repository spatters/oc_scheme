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
  | Quote q        -> printf "'"; output_value outc q
  | Func (name, f)         -> printf "Func:%s" name
  | UserFunc (name, _, _, _)     -> printf "UserFunc:%s " name
  | Symbol s       -> printf "%s" s
  | Pair (e1, e2) as pair -> print_pair outc pair
  | If (p, c, a)   -> print_pair outc (Types.scheme_list [Types.Symbol "if"; p; c; a;])
  | Assign defn  -> 
    (match defn with
     | Types.VarDef (s, expr) ->
       print_pair outc (Types.scheme_list ((List.map ["set!"; s] (fun s -> Types.Symbol s)) @ [expr]))
     | Types.FuncDef _ -> failwith "can't assign FuncDef")
  | Define defn  -> 
    (match defn with
     | Types.VarDef (s, expr) ->
       print_pair outc (Types.scheme_list ((List.map ["define"; s] (fun s -> Types.Symbol s)) @ [expr]))
     | Types.FuncDef (name, args, body) -> 
       print_pair
         outc 
         (Types.scheme_list ([Types.Symbol "define"; (Types.scheme_list ([Types.Symbol name] @ List.map args (fun a -> Types.Symbol a)))] @ body)))
  | Lambda (args, body) -> 
    print_pair outc (Types.scheme_list ([Types.Symbol "lambda"; Types.scheme_list (List.map args (fun a -> Types.Symbol a))] @ body))

(*
and print_pair outc (pair: Types.expr) = 
  if Types.is_proper_list pair
  then print_list outc (Types.pair_to_list_top_level pair)
  else
    (Out_channel.output_string outc "(";
     match pair with 
     | Types.Pair (e1, Types.Nill) -> 
       output_value outc e1;
       Out_channel.output_string outc ")"
     | Types.Pair (e1, e2) -> 
       output_value outc e1;
       Out_channel.output_string outc " . ";
       output_value outc e2;
       Out_channel.output_string outc ")"
     | _ -> failwith "really, again?")
*)

(*
and print_pair outc (pair : Types.expr) =
  (Out_channel.output_string outc "(";
   match pair with 
   | Types.Pair (e1, Types.Nill) ->
     output_value outc e1;
     Out_channel.output_string outc ")"
   | Types.Pair (e1, Types.Pair (e2, e3)) ->
     output_value outc e1;
     Out_channel.output_string outc " ";
     output_value outc (Types.Pair (e2, e3))
   | Types.Pair (e1, e2) ->
     output_value outc e1;
     Out_channel.output_string outc " . ";
     output_value outc e2;
     Out_channel.output_string outc ")"
   | _ -> failwith "why are you calling print_pair on a non-pair? why would you do that, why?")
*)

and print_pair outc (pair : Types.expr) =
  if Types.is_proper_list pair
  then print_proper_list outc (Types.pair_to_list_top_level pair)
  else print_dotted_list outc (Types.pair_to_list_top_level pair)
  
and print_proper_list outc (arr : Types.expr List.t) =
  Out_channel.output_string outc "(";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc " ";
      output_value outc v) arr;
  Out_channel.output_string outc ")"

and print_dotted_list outc arr =
  Out_channel.output_string outc "(";
  let len = List.length arr in 
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc " ";
      if i = len - 1 then
        Out_channel.output_string outc ". ";
      output_value outc v) arr;
  Out_channel.output_string outc ")"





