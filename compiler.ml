open Util

module S = Syntax
module V = Value

exception Compile_error

type name = S.variable
type cenv = name list list

let empty_cenv : cenv = []

let extend cenv vars =
  vars :: cenv

let compile_lookup name cenv =
  let rec scan_cenv m = function
      [] -> `Global
    | frame::cenv ->
        let rec scan_frame n = function
            [] -> scan_cenv (m + 1) cenv
          | v::frame ->
              if v = name then
                `Local (m, n)
              else
                scan_frame (n + 1) frame
        in scan_frame 0 frame
  in scan_cenv 0 cenv

let is_tail = function
    Vm.Return -> true
  | _ -> false

let rec compile code cenv next =
  match code with
      S.SConst c ->
	Vm.Constant (c, next)
    | S.SQuote c ->
	Vm.Constant (c, next)
    | S.SVar v ->
        begin
          match compile_lookup v cenv with
              `Local (m, n) ->
                Vm.LRef ((m, n), next)
            | `Global ->
                Vm.GRef (v, next)
        end
    | S.SLambda (vars, body) ->
	let cenv' = extend cenv vars in
	  Vm.Close (compile body cenv' Vm.Return, next)
    | S.SIf (test, t, e) ->
	let thenc = compile t cenv next in
	let elsec = compile e cenv next in
	  compile test cenv @@ Vm.Test (thenc, elsec)
    | S.SBegin xs ->
	let rec iter = function
	    [] -> next
	  | x::xs ->
              compile x cenv @@ iter xs
	in iter xs
    | S.SSet (v, x) ->
        let c = match compile_lookup v cenv with
            `Local (m, n) ->
              Vm.LSet ((m, n), next)
          | `Global ->
              Vm.GSet (v, next)
        in compile x cenv c
    | S.SCallCC x ->
	let c = Vm.Conti (Vm.Argument (compile x cenv Vm.Apply)) in
	  if is_tail next then c else Vm.Frame (next, c)
    | S.SApply (proc, args) ->
	let rec iter args c =
	  match args with
	      [] ->
		if is_tail next then c else Vm.Frame (next, c)
	    | arg::args ->
		iter args @@ compile arg cenv @@ Vm.Argument c
	in iter args @@ compile proc cenv @@ Vm.Apply
    | S.SDefinition (v, x) ->
	Obj.magic false
