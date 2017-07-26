open Core

let rec eval (env : Types.expr Environment.t) (expr : Types.expr) =
  match expr with
  | Nill -> expr
  | Atom a -> expr
  | Quote quote -> quote
  | Symbol s -> Environment.lookup_variable env s
  | Func (_,_) as func -> func
  | UserFunc (_,_,_,_) as user_func -> user_func
  | If (pred, consq, alt) ->
     (match eval env pred with
    | Atom (Bool false) -> eval env alt
    | _ -> eval env consq) (* only #f is false in scheme *)
  | Lambda (arg_names, body) -> UserFunc ("lambda", arg_names, body, env)
  | Define VarDef (sym, expr) -> 
    let value = eval env expr in
    Environment.define_variable env sym value;
    Types.Symbol sym
  | Define FuncDef (name, arg_names, (body : Types.expr List.t)) ->
    let user_func = Types.UserFunc (name, arg_names, body, env) in 
    eval env (Define (VarDef (name, user_func))) 
  | Assign VarDef (sym, expr) ->
    let value = eval env expr in
    Environment.assign_variable env sym value;
    Types.Symbol sym
  | Assign FuncDef _ -> failwith "Can't assign a FuncDef."
  | Pair (func_expr, arg_exprs) ->
    let func_val = eval env func_expr in 
    let arg_vals = list_of_values env arg_exprs in 
    match (func_val, arg_vals) with
    | (Func (name, f), arg_vals) -> f arg_vals
    | (UserFunc (name, arg_names, body, env) as user_func, arg_vals) -> apply user_func arg_vals
    | _ -> failwith "First element of combination is not applicable."

and apply (user_func : Types.expr) (arg_vals : Types.expr List.t) = 
  match user_func with
  | UserFunc (name, arg_names, body, env) ->
     let func_env = Environment.extend env arg_names arg_vals in
     eval_sequence func_env body 
  | _ -> failwith "First elt of list passed to apply is not a user defined function"

and eval_sequence env (exprs : Types.expr List.t) = 
  match exprs with
  | [] -> failwith "Empty expr sequence passed to eval_sequence"
  | last_expr :: [] -> eval env last_expr
  | some_expr :: other_exprs ->
    let _ = eval env some_expr in 
    eval_sequence env other_exprs

and list_of_values env (pair : Types.expr) : Types.expr List.t =
  match pair with 
  | Types.Nill -> []
  | Types.Pair (hd, tl) -> (eval env hd) :: (list_of_values env tl)
  | _ -> failwith "Argument list passed to list_of_values is not a proper list"
