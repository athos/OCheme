open Util

module V = Value

let assert_pred pred =
  if pred () then
    ()
  else
    raise Vm.Runtime_error

let arithmetic op e xs =
  assert_pred (fun () -> List.for_all V.is_number xs);
  V.from_int @@ List.fold_left (fun m n -> op m @@ V.to_int n) e xs

let add = arithmetic (+) 0
let mul = arithmetic ( * ) 1
let sub = function
    [] -> raise Vm.Runtime_error
  | [x] -> V.from_int (- (V.to_int x))
  | x::xs ->
      arithmetic (-) (V.to_int x) xs
let div = function
    [] -> raise Vm.Runtime_error
  | [x] -> V.from_int 0
  | x::xs ->
      arithmetic (/) (V.to_int x) xs

let equal xs =
  let rec equal2 x y =
    match x, y with
	Vm.Nil, Vm.Nil -> true
      | (Vm.Bool x),       (Vm.Bool y)       -> x = y
      | (Vm.Int x),        (Vm.Int y)        -> x = y
      | (Vm.Symbol x),     (Vm.Symbol y)     -> x = y
      | (Vm.Pair (x1,x2)), (Vm.Pair (y1,y2)) -> x = y && equal2 x2 y2
      | _,                    _              -> false
  in match xs with
      [x; y] -> V.from_bool @@ equal2 x y
    | _ -> raise Vm.Runtime_error

let cons = function
    [x; y] -> V.cons x y
  | _ -> raise Vm.Runtime_error

let car = function
    [x] -> V.car x
  | _ -> raise Vm.Runtime_error

let cdr = function
    [x] -> V.cdr x
  | _ -> raise Vm.Runtime_error

let display = function
    [x] -> print_string @@ V.show x; V.nil
  | _ -> raise Vm.Runtime_error

let newline = function
    [] -> print_newline (); V.nil
  | _ -> raise Vm.Runtime_error

let read = function
    [] ->
      let s = Stream.of_channel stdin in
        Reader.read s
  | _ -> raise Vm.Runtime_error

let primitives =
  [("+", add); ("-", sub); ("*", mul); ("/", div); ("equal?", equal);
   ("cons", cons); ("car", car); ("cdr", cdr); ("display", display);
   ("newline", newline); ("read", read)]

let load_primitives genv =
  List.iter
    (fun (name, proc) ->
       Vm.define_variable genv (Vm.as_variable name) (Vm.Primitive proc))
    primitives
