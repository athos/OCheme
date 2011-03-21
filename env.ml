open Util

type ('a, 'b) entry = {name : 'a; mutable value : 'b}
type ('a, 'b) frame = ('a, 'b) entry list
type ('a, 'b) t = ('a, 'b) frame list    

exception Name_Not_Found
    
let empty : ('a, 'b) t = []

let first_frame = List.hd
let enclosing_environment = List.tl

let extend env vars vals =
  List.map2 (fun name value -> {name; value}) vars vals :: env

let lookup_and_do v e sk fk =
  let rec scan_env = function
    | [] -> raise Name_Not_Found
    | frame::env ->
	let rec scan_frame = function
	  | [] -> fk (fun () -> scan_env env)
	  | ({name = name; value = value} as nv)::_ when v == name ->
	      sk @@ nv
	  | _::frame' -> scan_frame frame'
	in scan_frame frame
  in scan_env e

let lookup v e =
  lookup_and_do v e
    (fun {name = _; value = value} -> value)
    (fun f -> f ())

let update_name v x e =
  lookup_and_do v e
    (fun nv -> nv.value <- x)
    (fun f -> f ())

let define_name v x e =
  lookup_and_do v e
    (fun nv -> nv.value <- x; e)
    (fun f ->
       let entry = {name = v; value = x} in
	 match e with
	     [] -> [[entry]]
	   | frame::env ->
	       (entry::frame)::env)
