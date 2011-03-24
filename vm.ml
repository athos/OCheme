open Util

type pos = Env.pos
type env = value Env.t

and value =
    Nil
  | Bool of bool
  | Int of int
  | Symbol of string
  | Pair of (value * value)
  | Closure of insn * env
  | Cont of stack
  | Primitive of (value list -> value)

and insn =
    Halt
  | Refer of pos * insn
  | Constant of value * insn
  | Close of insn * insn
  | Test of insn * insn
  | Assign of pos * insn
  | Conti of insn
  | Nuate of stack * pos
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

let rec show = function
    Nil -> "()"
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Int i -> string_of_int i
  | Symbol s -> s
  | Pair p -> "(" ^ show_pair p ^ ")"
  | Closure (_,_) -> "#<closusure>"
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

let return_state s = function
    [] -> raise Runtime_error
  | {return = next; cenv = env; crib = rib}::stack ->
      {s with acc = List.hd s.rib; next; env; rib; stack}

let rec run s =
  match s.next with
      Halt -> s.acc
    | Refer (pos, next) ->
	let value =
	  try Env.lookup pos s.env
	  with e ->
	    raise Runtime_error
	in run {s with acc = value; next}
    | Constant (value, next) ->
	run {s with acc = value; next}
    | Close (body, next) ->
	run {s with acc = Closure (body, s.env); next}
    | Test (t, e) ->
	let next = if as_bool s.acc then t else e
	in run {s with next}
    | Assign (pos, next) ->
	Env.update_name pos s.acc s.env;
	run {s with next}
    | Conti next ->
	run {s with acc = Cont s.stack; next}
    | Nuate (stack, pos) ->
	run s			(* FIXME *)
    | Frame (return, next) ->
	let frame = {return; cenv = s.env; crib = s.rib}
	in run {s with rib = []; stack = frame::s.stack; next}
    | Argument next ->
	run {s with rib = (s.acc::s.rib); next}
    | Apply ->
	begin
	  match s.acc with
	      Closure (body, env) ->
		run {s with next = body; env = (Env.extend env s.rib)}
	    | Cont stack -> run @@ return_state s stack
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
    | Return -> run @@ return_state s s.stack
