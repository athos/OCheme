exception Compile_error

type cenv

val empty_cenv : cenv
val compile : Syntax.t -> cenv -> Vm.insn -> Vm.insn
