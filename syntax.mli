type variable = string

type t =
    SConst of Value.t
  | SQuote of Value.t
  | SVar of variable
  | SLambda of variable list * t
  | SIf of t * t * t
  | SBegin of t list
  | SSet of variable * t
  | SCallCC of t
  | SApply of t * t list
  | SDefinition of variable * t

exception Syntax_error of string

val from_value : Value.t -> t
