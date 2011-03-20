open Util
open Vm

type t = value

let is_null = function
  | SNil -> true
  | _ -> false

let as_bool x = Vm.as_bool x

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

let show x = Vm.show x
