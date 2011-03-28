type variable
type env
type genv
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

type state

exception Runtime_error of string

val as_variable : string -> variable
val as_bool : value -> bool
val show : value -> string

val initial_state : insn -> genv -> state
val empty_genv : unit -> genv
val define_variable : genv -> variable -> value -> unit

val run : state -> value
