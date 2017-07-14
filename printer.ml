open Core

let output_atom outc (atom : Types.atom) = 
  match atom with
  | Int i -> printf "%d" i

let rec output_value outc (expr : Types.expr) = 
  match expr with
  | Atom a     -> output_atom outc a
(*
  | Float x    -> printf "%f" x
  | Symbol s   -> printf "%s" s
  j String s   -> printf "%s" s
*)
  | List l     -> print_list outc l

and print_list outc arr =
  Out_channel.output_string outc "LIST (";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        Out_channel.output_string outc " ";
      output_value outc v) arr;
  Out_channel.output_string outc ")"



