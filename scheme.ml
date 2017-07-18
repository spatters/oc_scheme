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


let extend env names values = 
  let frame_alist = List.zip_exn names values in
  let frame_map = String.Map.of_alist_exn frame_alist in
  frame_map :: env 

let rec eval env (expr : Types.expr) =
  match expr with
  | Atom a -> env, expr
  | Symbol s -> env, lookup_symbol env s
  | Func f -> env, expr
(*   | UserFunc (args, body, env) -> env, expr *)
  | UserFunc _ -> env, expr
  | If (pred, consq, alt) ->
    (let env, pred_val = eval env pred in
     match pred_val with
    | Atom (Bool false) -> eval env alt
(*       if b then eval env consq else eval env alt *)
    | _ -> eval env consq) (* only #f is false in scheme *)
(*   | Lambda (args, body) -> env, UserFunc (args, body, env) *)
  | Lambda (arg_names, body) -> env, UserFunc {arg_names; body; environment=env}
  | Define VarDef (sym, expr) -> 
    let env, value = eval env expr in
    (match env with
    | [] -> failwith "No environment"
    | hd :: tl -> 
    let frame = String.Map.add hd ~key:sym ~data:value in
    (frame :: tl), Types.Symbol sym)
(*
  | Define (FuncDef (name, args), body) -> 
    eval env (Define (VarDef name, Lambda (args, body)))
*)
  | Define FuncDef (name, arg_names, (body : Types.expr List.t)) ->
    let user_func_struct = ({arg_names; body; environment=env} : Types.user_func) in 
    let user_func = Types.UserFunc user_func_struct in
    let new_env, _ = eval env (Define (VarDef (name, user_func))) in
    user_func_struct.environment <- new_env;
    new_env, Types.Symbol name
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
  | (Func f) :: arg_vals -> f arg_vals
  | (UserFunc {arg_names; body; environment}) :: arg_vals ->
     let func_env = extend environment arg_names arg_vals in
     let unused_env, return_val = eval_sequence func_env body in
     return_val
  | _ :: arg_vals -> failwith "First elt of list passed to apply is not a function"
  | [] -> failwith "Empty list passed to apply"

and eval_sequence env (exprs : Types.expr List.t) = 
  match exprs with
  | [] -> failwith "Empty expr sequence passed to eval_sequence"
  | last_expr :: [] -> eval env last_expr
  | some_expr :: other_exprs ->
    let new_env, _ = eval env some_expr in
    eval_sequence new_env other_exprs

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
