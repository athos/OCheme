open Util

type variable = string
type env = (variable, value) Env.t

and value =
    SNil
  | SBool of bool
  | SInt of int
  | SSymbol of string
  | SPair of (value * value)
  | SClosure of variable list * insn * env
  | SCont of stack
  | SPrimitive of (value list -> value)

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
    SBool false -> false
  | _ -> true

let as_variable s = s

let rec show = function
    SNil -> "()"
  | SBool true -> "#t"
  | SBool false -> "#f"
  | SInt i -> string_of_int i
  | SSymbol s -> s
  | SPair p -> "(" ^ show_pair p ^ ")"
  | SClosure (_,_,_) -> "#<closusure>"
  | SCont _ -> "#<cont>"
  | SPrimitive _ -> "#<primitive>"
and show_pair (x, xs) =
  let s = match xs with
      SNil -> ""
    | SPair p -> " " ^ show_pair p
    | _ -> " . " ^ show xs
  in show x ^ s

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
	run {s with acc = SClosure (vars, body, s.env); next}
    | Test (t, e) ->
	let next = if as_bool s.acc then t else e
	in run {s with next}
    | Assign (v, next) ->
	Env.update_name v s.acc s.env;
	run {s with next}
    | Conti next ->
	run {s with acc = SCont s.stack}
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
	      SClosure (vars, body, env) ->
		run {s with next = body; env = (Env.extend env vars s.rib)}
	    | _ ->
		raise @@ Invalid_operation (show s.acc ^ " can't be applied")
	end
    | Return ->
	begin
	  match s.stack with
	      [] -> raise Runtime_error
	    | {return = next; cenv = env; crib = rib}::stack ->
		run {s with next; env; rib; stack}
	end
