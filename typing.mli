(* Compiler Construction - Minimal Lambda Language *)

exception Error of Ast.loc * string

val check : Ast.program -> Typed_ast.program

