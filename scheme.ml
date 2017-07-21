open Core

let rec lookup_symbol env name =
  match env with
  | [] -> failwith "Unknown symbol"
  | hd :: tl -> 
    match String.Table.find hd name with
    | None -> lookup_symbol tl name
    | Some e -> e
;;

let rec assign_variable env name value =
  match env with
  | [] -> failwith "Unknown symbol"
  | frame :: tl -> 
    match String.Table.find frame name with
    | None -> lookup_symbol tl name
    | Some e -> String.Table.set frame ~key:name ~data:value
;;

let extend env names values = 
  let frame_alist = List.zip_exn names values in
  let frame_map = String.Table.of_alist_exn frame_alist in
  frame_map :: env 

let define_variable env name value = 
  (match env with
   | [] -> failwith "No environment can't define variable"
   | frame :: tl -> String.Table.set frame ~key:name ~data:value)

let rec eval env (expr : Types.expr) =
  match expr with
  | Nill -> expr
  | Atom a -> expr
  | Symbol s -> lookup_symbol env s
  | Func f -> expr
(*   | UserFunc (args, body, env) -> env, expr *)
  | UserFunc _ -> expr
  | If (pred, consq, alt) ->
     (match eval env pred with
    | Atom (Bool false) -> eval env alt
(*       if b then eval env consq else eval env alt *)
    | _ -> eval env consq) (* only #f is false in scheme *)
(*   | Lambda (args, body) -> env, UserFunc (args, body, env) *)
  | Lambda (arg_names, body) -> UserFunc {arg_names; body; environment=env}
  | Define VarDef (sym, expr) -> 
    let value = eval env expr in
    define_variable env sym value;
    Types.Symbol sym
(*
  | Define (FuncDef (name, args), body) -> 
    eval env (Define (VarDef name, Lambda (args, body)))
*)
  | Define FuncDef (name, arg_names, (body : Types.expr List.t)) ->
    let user_func_struct = ({arg_names; body; environment=env} : Types.user_func) in 
    let user_func = Types.UserFunc user_func_struct in
    eval env (Define (VarDef (name, user_func))) 
  | Pair (e1, e2) ->
    let val1 = eval env e1 in 
    Pair (val1, eval env e2) 
  | List exprs -> 
    let (vals : Types.expr List.t) = 
      List.fold exprs ~init:[]
      ~f:(fun vals expr ->
          let value = eval env expr in
          value :: vals)
    in 
    apply (List.rev vals)

and apply (exprs : Types.expr List.t) =  
  match exprs with
  | (Func f) :: arg_vals -> f arg_vals
  | (UserFunc {arg_names; body; environment}) :: arg_vals ->
     let func_env = extend environment arg_names arg_vals in
     eval_sequence func_env body 
  | _ :: arg_vals -> failwith "First elt of list passed to apply is not a function"
  | [] -> failwith "Empty list passed to apply"

and eval_sequence env (exprs : Types.expr List.t) = 
  match exprs with
  | [] -> failwith "Empty expr sequence passed to eval_sequence"
  | last_expr :: [] -> eval env last_expr
  | some_expr :: other_exprs ->
    let _ = eval env some_expr in 
    eval_sequence env other_exprs
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
