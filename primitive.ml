open Util

module V = Value

let error msg =
  raise @@ Vm.Runtime_error msg

let assert_pred pred =
  if pred () then
    ()
  else
    error ""

let arithmetic op e xs =
  assert_pred (fun () -> List.for_all V.is_number xs);
  V.from_int @@ List.fold_left (fun m n -> op m @@ V.to_int n) e xs

let add = arithmetic (+) 0
let mul = arithmetic ( * ) 1
let sub = function
  | [] -> error "procedure requires at least one argument: (-)"
  | [x] -> V.from_int (- (V.to_int x))
  | x::xs ->
    arithmetic (-) (V.to_int x) xs
let div = function
  | [] -> error "procedure requires at least one argument: (/)"
  | [x] -> V.from_int 0
  | x::xs ->
    arithmetic (/) (V.to_int x) xs

let equal xs =
  let rec equal2 x y =
    match x, y with
    | Vm.Nil, Vm.Nil -> true
    | (Vm.Bool x),       (Vm.Bool y)       -> x = y
    | (Vm.Int x),        (Vm.Int y)        -> x = y
    | (Vm.Symbol x),     (Vm.Symbol y)     -> x = y
    | (Vm.Pair (x1,x2)), (Vm.Pair (y1,y2)) -> x = y && equal2 x2 y2
    | _,                    _              -> false
  in
  match xs with
  | [x; y] -> V.from_bool @@ equal2 x y
  | _ -> error "wrong number of arguments for equal?"

let is_null = function
  | [x] -> Vm.Bool (V.is_null x)
  | _ -> error "wrong number of arguments for null?"

let cons = function
  | [x; y] -> V.cons x y
  | _ -> error "wrong number of arguments for cons"

let car = function
  | [x] -> V.car x
  | _ -> error "wrong number of arguments for car"

let cdr = function
  | [x] -> V.cdr x
  | _ -> error "wrong number of arguments for cdr"

let display = function
  | [x] -> print_string @@ V.show x; V.nil
  | _ -> error "wrong number of arguments for display"

let newline = function
  | [] -> print_newline (); V.nil
  | _ -> error "wrong number of arguments for newline"

let read = function
  | [] ->
    let s = Stream.of_channel stdin in
    Reader.read s
  | _ -> error "wrong number of arguments for read"

let eval exp env =
  let c = Syntax.from_value exp in
  let rec f = function
    | Syntax.SDefinition (var, exp') ->
      let v = f exp' in
      Vm.define_variable env var v;
      Vm.Bool false
    | c ->
      let b = Compiler.compile c Compiler.empty_cenv Vm.Halt in
      Vm.run @@ Vm.initial_state b env
  in f c

let eval' = function
  | [x; e] -> eval x @@ Vm.env_from_value e
  | _ -> error "wrong number of arguments for eval"

let empty_env = function
  | [] -> Vm.GEnv (Vm.empty_genv ())
  | _ -> error "wrong number of arguments for empty-environment"

let rec primitives =
  [("+", add); ("-", sub); ("*", mul); ("/", div); ("equal?", equal);
   ("cons", cons); ("car", car); ("cdr", cdr); ("display", display);
   ("newline", newline); ("read", read); ("eval", eval'); ("null?", is_null);
   ("empty-environment", empty_env); ("standard-environment", standard_env')]

and standard_env () =
  let env = Vm.empty_genv () in
  List.iter
    (fun (name, proc) ->
      Vm.define_variable env (Vm.as_variable name) (Vm.Primitive proc))
    primitives;
  env

and standard_env' = function
  | [] -> Vm.GEnv (standard_env ())
  | _ -> error "wrong number of arguments for standard-environment"
