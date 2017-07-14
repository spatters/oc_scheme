open Core

let output_atom outc (atom : Types.atom) = 
  match atom with
  | Int i -> printf "%d" i
  | Bool b -> printf "%b" b

let rec output_value outc (expr : Types.expr) = 
  match expr with
  | Atom a         -> output_atom outc a
  | Func f         -> printf "FUNCTION"
  | Symbol s       -> printf "%s" s

(*
  | Float x    -> printf "%f" x
*)
  | List l     -> print_list outc l

and print_list outc arr =
  Out_channel.output_string outc "(";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc " ";
      output_value outc v) arr;
  Out_channel.output_string outc ")"



