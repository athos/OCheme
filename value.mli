type t = Vm.value

val is_null : t -> bool
val is_bool : t -> bool
val is_number : t -> bool
val is_symbol : t -> bool
val is_pair : t -> bool
val is_list : t -> bool
val as_bool : t -> bool
val nil : t
val cons : t -> t -> t
val car : t -> t
val cdr : t -> t
val caar : t -> t
val cadr : t -> t
val cdar : t -> t
val cddr : t -> t
val from_bool : bool -> t
val from_int : int -> t
val from_list : t list -> t
val to_bool : t -> bool
val to_int : t -> int
val to_list : t -> t list
val intern : string -> t
val symbol_name : t -> string
val show : t -> string
