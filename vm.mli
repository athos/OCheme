type env
type stack
type pos = Env.pos

type value =
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

type state

exception Runtime_error
exception Invalid_operation of string

val as_bool : value -> bool
val show : value -> string

val initial_state : insn -> env -> state
val empty_env : env
val define_variable : pos -> value -> env -> env

val run : state -> value
