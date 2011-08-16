open Util
open Vm

type t = value

let is_null = Vm.is_null

let is_bool = function
  | Bool _ -> true
  | _ -> false

let is_number = function
  | Int _ -> true
  | _ -> false

let is_symbol = function
  | Symbol _ -> true
  | _ -> false

let is_pair = function
  | Pair _ -> true
  | _ -> false

let rec is_list = function
  | Nil -> true
  | Pair (car,cdr)
    -> is_list cdr
  | _ -> false

let as_bool x = Vm.as_bool x

let nil = Nil

let cons car cdr = Pair (car, cdr)

let car = Vm.car
let cdr = Vm.cdr

let caar x = (car $ car) x
let cadr x = (car $ cdr) x
let cdar x = (cdr $ car) x
let cddr x = (cdr $ cdr) x

let from_bool x = Bool x
let from_int x = Int x
let from_list xs = List.fold_right cons xs Nil

let to_bool = function
  | Bool x -> x
  | x -> raise @@ Runtime_error (show x ^ " is not boolean")

let to_int = function
  | Int x -> x
  | x -> raise @@ Runtime_error (show x ^ " is not int")

let to_list = Vm.to_list

let symbol_table : (string, t) Hashtbl.t = Hashtbl.create 256

let intern name =
  if Hashtbl.mem symbol_table name then
    Hashtbl.find symbol_table name
  else
    let sym = Symbol name in
    Hashtbl.add symbol_table name sym;
    sym
      
let symbol_name = function
  | Symbol name -> name
  | x -> raise @@ Runtime_error (show x ^ " is not a symbol")

let show x = Vm.show x
