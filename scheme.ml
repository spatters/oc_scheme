open Core

let rec lookup_symbol env symbol =
  match env with
  | [] -> failwith "Unknown symbol"
  | hd :: tl -> 
    match String.Map.find hd symbol with
    | None -> lookup_symbol tl symbol
    | Some e -> e
;;



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
