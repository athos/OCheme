open Util

module V = Value
  
exception Error

let is_whitespace c =
  match c with
      ' ' | '\t' | '\n' -> true
    | _ -> false

let is_identifier_first_letter c =
  match c with
      '!' | '$' | '%' | '&' | '*' | '+' | '-' | '/'
    | ':' | '<' | '=' | '>' | '?' | '@'
    | 'A'..'Z' | '^' | '_' | 'a'..'z' | '~' 
	  -> true
    | _ ->  false

let is_digit c =
  match c with
      '0'..'9' -> true
    | _ -> false

let digit_to_int d =
  Char.code d - Char.code '0'
	
let is_identifier_letter c =
  is_identifier_first_letter c || is_digit c

let peek s =
  match Stream.peek s with
      Some c -> c
    | None -> raise Error

let junk s = Stream.junk s

let peek_nonspace s =
  let rec iter () =
    let c = peek s in
      if is_whitespace c then
	begin
	  junk s;
	  iter ()
	end
      else c
  in iter ()

let read_number s =
  let rec iter n =
    let c = peek s in
      if is_digit c then
	begin
	  junk s;
	  iter @@ 10 * n + digit_to_int c
	end
      else if is_identifier_letter c then
	raise Error
      else V.from_int n
  in iter 0

let read_symbol s =
  let buf = Buffer.create 64 in
  let rec iter () =
    let c = peek s in
      if is_identifier_letter c then
	begin
	  Buffer.add_char buf c;
	  junk s;
	  iter ()
	end
      else
	V.intern @@ Buffer.contents buf
  in iter ()

let read_bool s =
  junk s;
  match peek s with
      't' ->
	junk s;
	V.from_bool true
    | 'f' ->
	junk s;
	V.from_bool false
    | _ ->
	raise Error

let rec read s =
  let c = peek_nonspace s in
    if is_digit c then
      read_number s
    else if is_identifier_first_letter c then
      read_symbol s
    else match c with
	'(' -> read_list s
      | '#' -> read_bool s
      | '\'' ->
	  V.from_list [V.intern "quote"; read s]
      | '`' ->
	  V.from_list [V.intern "quasiquote"; read s]
      | ',' ->
	  junk s;
	  let c = peek s in
	    if c = '@' then
	      begin
		junk s;
		V.from_list [V.intern "unquote-splicing"; read s]
	      end
	    else
	      V.from_list [V.intern "unquote"; read s]
      | _ -> raise Error

and read_list s =
  junk s;
  let vs = read_list_elements s in
    junk s;
    vs

and read_list_elements s =
  let c = peek s in
    match c with
	')' -> V.nil
      | '.' -> raise Error
      | _ ->
	  let v = read s in
	    (* improper list *)
	    if peek_nonspace s = '.' then
	      begin
		junk s;
		(* no objects occur after '.' *)
		if peek_nonspace s == ')' then
		  raise Error
		else
		  let v' = read s in
		    (* more than one objects occur after '.' *)
		    if peek_nonspace s <> ')' then
		      raise Error
		    else V.cons v v'
	      end
	    (* proper list *)
	    else V.cons v @@ read_list_elements s
