open Util

module S = Syntax
module V = Value

let rec compile code next =
  match code with
      S.SConst c ->
	Vm.Constant (c, next)
    | S.SQuote c ->
	Vm.Constant (c, next)
    | S.SVar v ->
	Vm.Refer (v, next)
    | S.SLambda (vars, body) ->
	Vm.Close (vars, compile body Vm.Return, next)
    | S.SIf (test, t, e) ->
	let thenc = compile t next in
	let elsec = compile e next in
	  compile test @@ Vm.Test (thenc, elsec)
    | S.SBegin xs ->
	let rec iter xxs c =
	  match xxs with
	      [] ->
		c			(* FIXME *)
	    | x::xs ->
		iter xs @@ compile x c
	in iter xs next
    | S.SSet (v, x) ->
	compile x @@ Vm.Assign (v, next)
    | S.SCallCC x ->
	let c = Vm.Conti (Vm.Argument (compile x Vm.Apply)) in
	  if false then c else Vm.Frame (next, c)
    | S.SApply (proc, args) ->
	let rec iter args c =
	  match args with
	      [] ->
		if false then c	else Vm.Frame (next, c)
	    | arg::args ->
		iter args @@ compile arg @@ Vm.Argument c
	in iter args @@ compile proc @@ Vm.Apply
    | S.SDefinition (v, x) ->
	Obj.magic false
