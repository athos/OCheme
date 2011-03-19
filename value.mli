type t =
    SNil
  | SBool of bool
  | SInt of int
  | SSymbol of string
  | SPair of (t * t)
  | SClosure
  | SCont

exception Invalid_Operation of string

val is_null : t -> bool
val as_bool : t -> bool
val cons : t -> t -> t
val car : t -> t
val cdr : t -> t
val from_list : t list -> t
val symbol_table : (string, t) Hashtbl.t
val intern : string -> t
val show : t -> string
