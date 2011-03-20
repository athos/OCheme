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
  | SApply of t * t

val as_variable : string -> variable
