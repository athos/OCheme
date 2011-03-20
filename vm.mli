type variable
type env
type stack

type value =
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
  | Return

type state

exception Runtime_error
exception Invalid_operation of string

val as_bool : value -> bool
val as_variable : string -> variable
val show : value -> string

val initial_state : insn -> env -> state
val empty_env : env

val run : state -> value
