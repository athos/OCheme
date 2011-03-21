open Util

type 'a frame = 'a ref list
type 'a t = 'a frame list
type pos = int * int

exception Name_Not_Found

let empty : 'a t = []

let first_frame = List.hd
let enclosing_environment = List.tl

let extend env vals =
  List.map ref vals :: env

let lookup_and_do (m, n) e sk fk =
  let rec scan_env m = function
      [] -> raise Name_Not_Found
    | frame::env ->
	if m = 0 then
	  let rec scan_frame n = function
	      [] -> fk ()
	    | entry::frame ->
		if n = 0 then
		  sk entry
		else
		  scan_frame (n - 1) frame
	  in scan_frame n frame
	else
	  scan_env (m - 1) env
  in scan_env m e

let lookup pos e =
  lookup_and_do pos e
    (fun ent -> ent.contents)
    (fun () -> raise Name_Not_Found)

let update_name pos x e =
  lookup_and_do pos e
    (fun ent -> ent := x)
    (fun () -> raise Name_Not_Found)

let define_name pos x e =
  lookup_and_do pos e
    (fun ent -> ent := x; e)
    (fun () ->
       let entry = ref x in
	 match e with
	     [] -> [[entry]]
	   | frame::env ->
	       (entry::frame)::env)
