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

and stack = {
  return : insn;
  cenv : env;
  crib : value list;
  cstack : stack;
}

type state = {
  acc : value;
  next : insn;
  env : env;
  rib : value list;
  stack : stack
}

exception Error
exception Invalid_Operation of string

let as_bool = function
  | SBool false -> false
  | _ -> true

let rec show = function
  | SNil -> "()"
  | SBool true -> "#t"
  | SBool false -> "#f"
  | SInt i -> string_of_int i
  | SSymbol s -> s
  | SPair p -> "(" ^ show_pair p ^ ")"
  | SClosure (_,_,_) -> "#<closusure>"
  | SCont _ -> "#<cont>"
and show_pair (x, xs) =
  let s = match xs with
    | SNil -> ""
    | SPair p -> " " ^ show_pair p
    | _ -> " . " ^ show xs
  in show x ^ s

let rec run s =
  match s.next with
    | Halt -> s.acc
    | Refer (v, next) ->
	let value =
	  try Env.lookup v s.env
	  with e ->
	    raise Error
	in run {s with acc = value; next}
    | Constant (value, next) ->
	run {s with acc = value; next}
    | Close (vs, body, next) ->
	run s
    | Test (t, e) ->
	let next = match s.acc with
	  | SBool false -> t
	  | _ -> e
	in run {s with next}
    | Assign (v, next) ->
	Env.update_name v s.acc s.env;
	run {s with next}
    | Conti next ->
	run s			(* FIXME *)
    | Nuate (stack, v) ->
	run s			(* FIXME *)
    | Frame (return, next) ->
	let stack = {return; cenv = s.env; crib = s.rib; cstack = s.stack}
	in run {s with rib = []; stack; next}
    | Argument next ->
	run {s with rib = (s.acc::s.rib); next}
    | Apply ->
	begin
	  match s.acc with
	    | SClosure (_,_,_) ->		(* FIXME *)
		run s
	    | _ ->
		raise @@ Invalid_Operation (show s.acc ^ " can't be applied")
	end
    | Return ->
	let {return = next; cenv = env; crib = rib; cstack = stack} = s.stack
	in run {s with next; env; rib; stack}
