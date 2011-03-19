open Util

type variable = string
type value = Value.t
type env = (variable, value) Env.t

type insn =
  | Halt
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

let state ?acc ?next ?env ?rib ?stack s =
  let update p = function
    | Some v -> v
    | None -> p in
  let acc' = update s.acc acc in
  let next' = update s.next next in
  let env' = update s.env env in
  let rib' = update s.rib rib in
  let stack' = update s.stack stack in
    {acc = acc'; next = next'; env = env'; rib = rib'; stack = stack'}

let rec run s =
  match s.next with
    | Halt -> s.acc
    | Refer (v, next) ->
	let value =
	  try Env.lookup v s.env
	  with e ->
	    raise Error
	in run @@ state s ~acc:value ~next
    | Constant (value, next) ->
	run @@ state s ~acc:value ~next
    | Close (vs, body, next) ->
	run @@ state s
    | Test (t, e) ->
	let next = if Value.as_bool s.acc then t else e
	in run @@ state s ~next
    | Assign (v, next) ->
	Env.update_name v s.acc s.env;
	run @@ state s ~next
    | Conti next ->
	run @@ state s			(* FIXME *)
    | Nuate (stack, v) ->
	run @@ state s			(* FIXME *)
    | Frame (return, next) ->
	let stack = {return; cenv = s.env; crib = s.rib; cstack = s.stack}
	in run @@ state s ~rib:[] ~stack ~next
    | Argument next ->
	run @@ state s ~rib:(s.acc::s.rib) ~next
    | Apply ->
	(match s.acc with
	   | Value.SClosure ->		(* FIXME *)
	       run @@ state s
	   | _ ->
	       raise @@ Value.Invalid_Operation
		 (Value.show s.acc ^ " can't be applied"))
    | Return ->
	let {return = next; cenv = env; crib = rib; cstack = stack} = s.stack
	in run @@ state s ~next ~env ~rib ~stack
