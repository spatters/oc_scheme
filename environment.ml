open Core

(* module Environment : ENVIRONMENT = struct *)
type 'a t = ('a String.Table.t) List.t

let empty  = []

let of_alist_exn = fun alist -> [String.Table.of_alist_exn alist]

let rec lookup_variable env name =
  (match env with
   | [] -> failwith "Unknown symbol"
   | hd :: tl -> 
     (match String.Table.find hd name with
      | None -> lookup_variable tl name
      | Some e -> e))

let rec assign_variable env name value =
  match env with
  | [] -> failwith "Unknown symbol"
  | frame :: tl -> 
    match String.Table.find frame name with
    | None -> assign_variable tl name value
    | Some e -> String.Table.set frame ~key:name ~data:value

let define_variable env name value = 
  (match env with
   | [] -> failwith "No environment can't define variable"
   | frame :: tl -> String.Table.set frame ~key:name ~data:value)

let extend env names values = 
  let frame_alist = List.zip_exn names values in
  let frame_map = String.Table.of_alist_exn frame_alist in
  frame_map :: env 
(* end *)
