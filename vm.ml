open Util

type variable = string
type env = (variable, value) Env.t

and value =
    Nil
  | Bool of bool
  | Int of int
  | Symbol of string
  | Pair of (value * value)
  | Closure of variable list * insn * env
  | Cont of stack
  | Primitive of (value list -> value)

and insn =
    Halt
  | Refer of variable * insn
  | Constant of value * insn
  | Close of variable list * insn * insn
  | Test of insn * insn
  | Assign of variable * insn
  | Conti of insn
  | Nuate of stack * variable
  | Frame of insn * insn
  | Argument of insn
  | Apply
  | PApply of insn
  | Return

and frame = {
  return : insn;
  cenv : env;
  crib : value list;
}

and stack = frame list

type state = {
  acc : value;
  next : insn;
  env : env;
  rib : value list;
  stack : stack
}

exception Runtime_error
exception Invalid_operation of string

let as_bool = function
    Bool false -> false
  | _ -> true

let as_variable s = s

let rec show = function
    Nil -> "()"
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Int i -> string_of_int i
  | Symbol s -> s
  | Pair p -> "(" ^ show_pair p ^ ")"
  | Closure (_,_,_) -> "#<closusure>"
  | Cont _ -> "#<cont>"
  | Primitive _ -> "#<primitive>"
and show_pair (x, xs) =
  let s = match xs with
      Nil -> ""
    | Pair p -> " " ^ show_pair p
    | _ -> " . " ^ show xs
  in show x ^ s

let initial_state next env =
  {acc = Nil; next; env; rib = []; stack = []}

let empty_env = Env.empty

let define_variable variable value env =
  Env.define_name variable value env

let rec run s =
  match s.next with
      Halt -> s.acc
    | Refer (v, next) ->
	let value =
	  try Env.lookup v s.env
	  with e ->
	    raise Runtime_error
	in run {s with acc = value; next}
    | Constant (value, next) ->
	run {s with acc = value; next}
    | Close (vars, body, next) ->
	run {s with acc = Closure (vars, body, s.env); next}
    | Test (t, e) ->
	let next = if as_bool s.acc then t else e
	in run {s with next}
    | Assign (v, next) ->
	Env.update_name v s.acc s.env;
	run {s with next}
    | Conti next ->
	run {s with acc = Cont s.stack; next}
    | Nuate (stack, v) ->
	run s			(* FIXME *)
    | Frame (return, next) ->
	let frame = {return; cenv = s.env; crib = s.rib}
	in run {s with rib = []; stack = frame::s.stack; next}
    | Argument next ->
	run {s with rib = (s.acc::s.rib); next}
    | Apply ->
	begin
	  match s.acc with
	      Closure (vars, body, env) ->
		run {s with next = body; env = (Env.extend env vars s.rib)}
	    | _ ->
		raise @@ Invalid_operation (show s.acc ^ " can't be applied")
	end
    | PApply next ->
	begin
	  match s.acc with
	      Primitive proc ->
		run {s with acc = proc s.rib; next}
	    | _ -> raise @@ Invalid_operation (show s.acc ^ " can't be applied")
	end
    | Return ->
	begin
	  match s.stack with
	      [] -> raise Runtime_error
	    | {return = next; cenv = env; crib = rib}::stack ->
		run {s with next; env; rib; stack}
	end
