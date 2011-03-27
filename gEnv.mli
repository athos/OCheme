type ('a, 'b) t = ('a, 'b) Hashtbl.t

exception Name_not_found

val create : ?size:int -> unit -> ('a, 'b) Hashtbl.t
val get : ('a, 'b) Hashtbl.t -> 'a -> 'b
val put : ('a, 'b) Hashtbl.t -> 'a -> 'b -> unit
