open Util
open Vm

type t = value

let is_null = function
    SNil -> true
  | _ -> false

let is_bool = function
    SBool _ -> true
  | _ -> false

let is_number = function
    SInt _ -> true
  | _ -> false

let is_symbol = function
    SSymbol _ -> true
  | _ -> false

let is_pair = function
    SPair _ -> true
  | _ -> false

let rec is_list = function
    SNil -> true
  | SPair (car,cdr)
      -> is_list cdr
  | _ -> false

let as_bool x = Vm.as_bool x

let nil = SNil

let cons car cdr = SPair (car, cdr)
let car = function
  | SPair (a, _) -> a
  | _ -> raise @@ Invalid_Operation "can't take car of non-pair object"
let cdr = function
  | SPair (_, d) -> d
  | _ -> raise @@ Invalid_Operation "can't take cdr of non-pair object"
let caar x = (car $ car) x
let cadr x = (car $ cdr) x
let cdar x = (cdr $ car) x
let cddr x = (cdr $ cdr) x

let from_bool x = SBool x
let from_int x = SInt x
let from_list xs = List.fold_right cons xs SNil

let rec to_list x =
  if is_null x then
    []
  else
    let a = car x in
    let d = cdr x in
      a::to_list d

let symbol_table : (string, t) Hashtbl.t = Hashtbl.create 256

let intern name =
  if Hashtbl.mem symbol_table name then
    Hashtbl.find symbol_table name
  else
    let sym = SSymbol name in
      Hashtbl.add symbol_table name sym;
      sym

let symbol_name = function
    SSymbol name -> name
  | x -> raise @@ Invalid_Operation (show x ^ " is not a symbol")

let show x = Vm.show x
