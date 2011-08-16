open Util

let _ =
  let s = Stream.of_channel stdin in
  let c = Syntax.from_value @@ Reader.read s in
  let b = Compiler.compile c Compiler.empty_cenv Vm.Halt in
  Vm.run @@ Vm.initial_state b @@ Primitive.load_primitives ()
