open Util
open Value

let assert_pred pred =
  if pred () then
    ()
  else
    raise Vm.Runtime_error

let arithmetic op e xs =
  assert_pred (fun () -> List.for_all is_number xs);
  from_int @@ List.fold_left (fun m n -> op m @@ to_int n) e xs

let add = arithmetic (+) 0
let mul = arithmetic ( * ) 1
let sub = function
    [] -> raise Vm.Runtime_error
  | [x] -> from_int (- (to_int x))
  | x::xs ->
      arithmetic (-) (to_int x) xs
let div = function
    [] -> raise Vm.Runtime_error
  | [x] -> from_int 0
  | x::xs ->
      arithmetic (/) (to_int x) xs

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
      [x; y] -> from_bool @@ equal2 x y
    | _ -> raise Vm.Runtime_error

let primitives =
  [("+", add); ("-", sub); ("*", mul); ("/", div); ("equal?", equal)]

let load_primitives env =
  List.fold_left
    (fun env (name, proc) ->
       Vm.define_variable (Vm.as_variable name) (Vm.Primitive proc) env)
    env
    primitives
