type ('a, 'b) t = ('a, 'b) Hashtbl.t

exception Name_not_found

let create ?(size=256) () =
  Hashtbl.create size

let get genv x =
  try
    Hashtbl.find genv x
  with Not_found ->
    raise Name_not_found

let put genv x v =
  Hashtbl.add genv x v
