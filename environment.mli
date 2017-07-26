open Core

type 'a  t
val empty : 'a t
val of_alist_exn : (string * 'a) List.t -> 'a t
val lookup_variable : 'a t -> string -> 'a
val define_variable : 'a t -> string -> 'a -> unit
val assign_variable : 'a t -> string -> 'a -> unit
val extend : 'a t -> string List.t -> 'a List.t -> 'a t
