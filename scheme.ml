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

let new_env () =
  let plus nums = List.fold nums ~init:0 ~f:Int.(+) in
  let built_in_funcs =
    String.Map.empty
  |> String.Map.add ~key:"+" ~data:plus
  in
  [ built_in_funcs ]
;;

let rec eval env (expr : Types.expr) =
  match expr with
  | Atom a -> env, expr
  | Symbol s -> env, lookup_symbol env s
  | Func f -> env, expr
  | If (pred, consq, alt) ->
    (let env, expr = eval env pred in
     match expr with
    | Atom (Bool b) ->
      if b then eval env consq else eval env alt
    | _ -> failwith "pred not bool")
  | Define (sym, expr) -> 
    let env, value = eval env expr in
    (match env with
    | [] -> failwith "No environment"
    | hd :: tl -> 
    let env = String.Map.add hd ~key:sym ~data:value in
    (env :: tl), Types.Symbol sym)
  | List exprs -> 
    let env, (vals : Types.expr List.t) = 
    List.fold exprs ~init:(env,[]) 
      ~f:(fun (env, vals) expr ->
          let env, value = eval env expr in
          env, (value :: vals)) 
    in 
    env, apply (List.rev vals)

and apply (exprs : Types.expr List.t) =  
  match exprs with
  | hd :: tl ->
    (
      match hd with
      | Func f -> f tl 
      | _ -> failwith "deal later in apply 1"
    )
  | [] -> failwith "deal later in apply 2"

;;

(* let () =  *)
  (* let env = new_env () in *)
  (* while true do  *)
  (*   Out_channel.output_string stdout ">: "; *)
  (*   Out_channel.flush stdout; *)
  (*   match In_channel.input_line In_channel.stdin with *)
  (*   | None -> failwith "No input" *)
  (*   | Some input_str ->  *)
  (*     Parser. *)
  (*     Out_channel.output_string stdout  *)
  (*       (String.concat  *)
  (*          [">: "; input_str; "\n"]); *)
  (*     Out_channel.flush stdout; *)
  (* done *)
