open Core


let plus nums = 
  List.fold nums ~init:0  ~f:(fun accum expr -> 
      accum + (Types.expr_to_int expr))
;;

let minus nums = match nums with
  | [] -> 0
  | hd :: tl -> Types.expr_to_int hd - plus tl
;;

let rec equals (exprs : Types.expr List.t) : bool = 
  match exprs with
  | e1 :: e2 :: [] ->
    (match e1, e2 with
    | Atom (Int i1), Atom (Int i2) -> i1 = i2
    | Atom (Bool b1), Atom (Bool b2) -> b1 = b2
(*     | Symbol s1, Symbol s1 -> s1 = s2 *)
    | List l1, List l2 ->
      (match l1, l2 with
       | [], [] -> true
       | _ :: _, [] | [], _ :: _ -> false
       | hd1 :: tl1, hd2 :: tl2 -> 
         (equals [hd1; hd2]) && (equals [Types.List tl1; Types.List tl2]))
    | _, _ -> false)
  | _ -> failwith "equals called with wrong number of arguments"
;;

(* let int_to_expr f ints = Types.Func (Types.Atom (Types.Int (f ints))) *)

let int_to_expr f = Types.Func (fun ints -> Types.Atom (Types.Int (f ints)))

let new_env () =
  let built_in_funcs =
    String.Map.empty
    |> String.Map.add ~key:"+" ~data:(int_to_expr plus)
    |> String.Map.add ~key:"-" ~data:(int_to_expr minus)
  in
  [ built_in_funcs ]
;;
