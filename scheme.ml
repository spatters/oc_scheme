open Core

type expr =
  | Int of int
  | Float of float
  | Symbol of string
  | String of string
  | List of expr list 

let rec output_value outc = function
  | Int i      -> printf "INT %d" i
  | Float x    -> printf "FLOAT %f" x
  | Symbol s   -> printf "SYMBOL %s" s
  | String s   -> printf "STRING %s" s
  | List l     -> print_list outc l

and print_list outc arr =
  output_string outc "LIST (";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc " ";
      output_value outc v) arr;
  output_string outc ")"

(*
  | Symbol of string
  | Quote of expr
  | Assignment of expr
  | Definition of expr
  | If of expr
  | Lambda of expr
  | Begin of expr
  | Cond of expr
  | Application of expr
*)

(*
let () = 
  while true do 
    Out_channel.output_string stdout ">: ";
    Out_channel.flush stdout;
    match In_channel.input_line In_channel.stdin with
    | None -> failwith "No input"
    | Some input_str -> 
      Out_channel.output_string stdout 
        (String.concat 
           [">: "; input_str; "\n"]);
      Out_channel.flush stdout;
  done
*)
