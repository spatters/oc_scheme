open Core
open Lexer
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.prog Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)

(* part 1 *)
let rec parse_and_print ~env lexbuf =
  match parse_with_error lexbuf with
  | Some value ->
    printf "%a =>  " Printer.output_value value;
    printf "%a \n" Printer.output_value (Scheme.eval env value);
    parse_and_print ~env lexbuf
  | None -> ()

let new_env () =
  let plus nums : Types.expr = 
    (List.fold nums ~init:0 
      ~f:(fun accum expr -> accum + (Types.expr_to_int expr)) 
    |> Types.int_to_expr ) 
  in 
  let plus_func = Types.func_to_expr plus in 
  let built_in_funcs =
    String.Map.empty
    |> String.Map.add ~key:"+" ~data:plus_func
  in
  [ built_in_funcs ]
;;

let loop filename () =
  let env = new_env () in 
  let inx = In_channel.create filename in
  let lexbuf = Lexing.from_channel inx in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_and_print ~env lexbuf;
  In_channel.close inx

(* part 2 *)
let () =
  Command.basic ~summary:"Parse and display Sexp"
    Command.Spec.(empty +> anon ("filename" %: file))
   loop
  |> Command.run
