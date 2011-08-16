open Util

type variable = Vm.variable

module V = Value

exception Syntax_error of string

type t =
  | SConst of Value.t
  | SQuote of Value.t
  | SVar of variable
  | SLambda of variable list * t
  | SIf of t * t * t
  | SBegin of t list
  | SSet of variable * t
  | SApply of t * t list
  | SDefinition of variable * t
  | SLet of (variable * t) list * t list

let as_variable x =
  Vm.as_variable @@ V.symbol_name x

let assert_pred pred =
  if pred () then
    ()
  else
    raise @@ Syntax_error ""		(* FIXME *)

let assert_nargs n args =
  let rec iter i args =
    if V.is_null args then
      if i = 0 then
	()
      else
	raise @@ Syntax_error (string_of_int n ^ " arguments required")
    else
      iter (i - 1) (V.cdr args)
  in iter n args

let rec from_value v =
  if not @@ V.is_list v then
    if V.is_bool v || V.is_number v then
      SConst v
    else if V.is_symbol v then
      SVar (as_variable v)
    else
      raise @@ Syntax_error ""		(* FIXME *)
  else
    let op = V.car v in
    let args = V.cdr v in
    if V.is_symbol op then
      match V.symbol_name op with
      | "quote" ->
	assert_nargs 1 args;
	SConst (V.car args)
      | "lambda" ->
	assert_pred (fun () -> List.length @@ V.to_list args >=2);
	assert_pred (fun () -> V.is_list args);
	SLambda ((List.map as_variable @@ (V.to_list @@ V.car args)),
                 SBegin (List.map from_value @@ V.to_list @@ V.cdr args))
      | "if" ->
	assert_nargs 3 args;
	SIf ((from_value @@ V.car args),
	     (from_value @@ V.cadr args),
	     (from_value @@ V.car @@ V.cddr args))
      | "begin" ->
	assert_pred (fun () -> not @@ V.is_null args);
	SBegin (List.map from_value @@ V.to_list args)
      | "set!" ->
	assert_nargs 2 args;
	assert_pred (fun () -> V.is_symbol @@ V.car args);
	SSet ((as_variable @@ V.car args),
	      (from_value @@ V.cadr args))
      | "define" ->
	assert_nargs 2 args;
	assert_pred (fun () -> V.is_symbol @@ V.car args);
	SDefinition ((as_variable @@ V.car args),
		     (from_value @@ V.cadr args))
      | "let" ->
        let bindings = V.car args in
        let body = V.cdr args in
        let rec iter bindings ret =
          if V.is_null bindings then
            ret
          else
            let binding = V.car bindings in
            let var = V.car binding in
            let exp = V.cadr binding in
            iter (V.cdr bindings)
            @@ (Vm.as_variable @@ V.symbol_name var, from_value exp)::ret
        in SLet (iter bindings [], List.map from_value @@ V.to_list body)
      | _ ->
	SApply ((from_value op),
		(List.map from_value @@ V.to_list args))
    else
      SApply ((from_value op),
	      (List.map from_value @@ V.to_list args))
