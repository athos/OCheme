exception Compile_error

type cenv
type name = Syntax.variable

val empty_cenv : cenv
val compile : Syntax.t -> cenv -> Vm.insn -> Vm.insn
