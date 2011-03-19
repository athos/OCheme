type ('a, 'b) t
type ('a, 'b) frame

val empty : ('a, 'b) t
val first_frame : ('a, 'b) t  -> ('a, 'b) frame
val enclosing_environment : ('a, 'b) t -> ('a, 'b) t
val extend : ('a, 'b) t -> 'a list -> 'b list -> ('a, 'b) t
val lookup : 'a -> ('a, 'b) t -> 'b
val update_name : 'a -> 'b -> ('a, 'b) t -> unit
