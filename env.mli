type 'a t
type 'a frame
type pos = int * int

exception Name_not_found

val empty : 'a t
val first_frame : 'a t  -> 'a frame
val enclosing_environment : 'a t -> 'a t
val extend : 'a t -> 'a list -> 'a t
val lookup : pos -> 'a t -> 'a
val update_name : pos -> 'a -> 'a t -> unit
val define_name : pos -> 'a -> 'a t -> 'a t
