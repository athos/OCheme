type variable
type env
type stack

type value =
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

type state

exception Error
exception Invalid_Operation of string

val as_bool : value -> bool
val show : value -> string

val run : state -> value
