open Util

type variable = string
type pos = Env.pos
type env = value Env.t
and genv = (variable, value) GEnv.t

and value =
  | Nil
  | Bool of bool
  | Int of int
  | Symbol of string
  | Pair of (value * value)
  | Closure of insn * env
  | Cont of stack
  | Primitive of (value list -> value)
  | GEnv of genv

and insn =
  | Halt
  | LRef of pos * insn
  | GRef of variable * insn
  | Constant of value * insn
  | Close of insn * insn
  | Test of insn * insn
  | LSet of pos * insn
  | GSet of variable * insn
  | Conti of insn
  | Nuate of stack * pos
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
  stack : stack;
  genv : genv
}

exception Runtime_error of string

let as_variable x = x

let as_bool = function
  | Bool false -> false
  | _ -> true

let rec show = function
  | Nil -> "()"
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Int i -> string_of_int i
  | Symbol s -> s
  | Pair p -> "(" ^ show_pair p ^ ")"
  | Closure (_,_) -> "#<closusure>"
  | Cont _ -> "#<cont>"
  | Primitive _ -> "#<primitive>"
  | GEnv _ -> "#<environment>"
and show_pair (x, xs) =
  let s = match xs with
    | Nil -> ""
    | Pair p -> " " ^ show_pair p
    | _ -> " . " ^ show xs
  in show x ^ s

let initial_state next genv =
  {acc = Nil; next; env = Env.empty; rib = []; stack = []; genv = genv}

let empty_genv () = GEnv.create ()

let define_variable genv variable value =
  GEnv.put genv variable value

let env_from_value = function
  | GEnv genv -> genv
  | _ -> raise @@ Runtime_error ""               (* FIXME: error message *)

let return_state s acc = function
  | [] -> raise @@ Runtime_error "can't return here"
  | {return = next; cenv = env; crib = rib}::stack ->
    {s with acc; next; env; rib; stack}

let rec run s =
  match s.next with
  | Halt -> s.acc
  | LRef (pos, next) ->
    let value =
      try Env.lookup pos s.env
      with _ ->
        assert false                    (* NOTREACHED *)
    in run {s with acc = value; next}
  | GRef (var, next) ->
    let value =
      try GEnv.get s.genv var
      with e ->
        raise @@ Runtime_error (Printf.sprintf "no such variable: %s" var)
    in run {s with acc = value; next}
  | Constant (value, next) ->
    run {s with acc = value; next}
  | Close (body, next) ->
    run {s with acc = Closure (body, s.env); next}
  | Test (t, e) ->
    let next = if as_bool s.acc then t else e
    in run {s with next}
  | LSet (pos, next) ->
    Env.update_name pos s.acc s.env;
    run {s with next}
  | GSet (var, next) ->
    GEnv.put s.genv var s.acc;
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
      | Closure (body, env) ->
	run {s with next = body; env = (Env.extend env s.rib); rib = []}
      | Primitive proc ->
        let v = proc s.rib in
        run {s with acc = v; next = Return; rib = []}
      | Cont stack -> run @@ return_state s (List.hd s.rib) stack
      | _ ->
	raise @@ Runtime_error (show s.acc ^ " can't be applied")
    end
  | Return -> run @@ return_state s s.acc s.stack
