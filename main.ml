open Util

let _ =
  let s = Stream.of_channel stdin in
  let c = Syntax.from_value @@ Reader.read s in
  let b = Compiler.compile c Vm.Halt in
    Vm.run @@ Vm.initial_state b Vm.empty_env
