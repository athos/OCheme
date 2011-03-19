open Util

type t =
  | SNil
  | SBool of bool
  | SInt of int
  | SSymbol of string
  | SPair of (t * t)
  | SClosure
  | SCont

exception Invalid_Operation of string

let is_null = function
  | SNil -> true
  | _ -> false

let as_bool = function
  | SBool false -> false
  | _ -> true

let cons car cdr = SPair (car, cdr)
let car = function
  | SPair (a, _) -> a
  | _ -> raise @@ Invalid_Operation "can't take car of non-pair object"
let cdr = function
  | SPair (_, d) -> d
  | _ -> raise @@ Invalid_Operation "can't take cdr of non-pair object"

let from_list xs = List.fold_right cons xs SNil

let symbol_table : (string, t) Hashtbl.t = Hashtbl.create 256

let intern name =
  if Hashtbl.mem symbol_table name then
    Hashtbl.find symbol_table name
  else
    let sym = SSymbol name in
      Hashtbl.add symbol_table name sym;
      sym

let rec show = function
  | SNil -> "()"
  | SBool true -> "#t"
  | SBool false -> "#f"
  | SInt i -> string_of_int i
  | SSymbol s -> s
  | SPair p -> "(" ^ show_pair p ^ ")"
  | SClosure -> "#<closusure>"
  | SCont -> "#<cont>"
and show_pair (x, xs) =
  let s = match xs with
    | SNil -> ""
    | SPair p -> " " ^ show_pair p
    | _ -> " . " ^ show xs
  in show x ^ s
