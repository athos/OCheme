open Util

let _ =
  let s = Stream.of_channel stdin in
  Primitive.eval (Reader.read s) (Primitive.standard_env ())
