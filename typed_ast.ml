(* Compiler Construction - Minimal Lambda Language
 *
 * This is a version of the AST produced by the type checker.
 * It does not actually include type information presently
 * since it is not needed by other passes. Lambda captures
 * are the targets of named references are made explicit here.
 *)

type loc = Lexing.position

type id = int

type expr
  = FuncExpr of loc * id
  | EnvExpr of loc * id
  | BoundExpr of loc * id
  | ArgExpr of loc * id
  | IntExpr of loc * int
  | AddExpr of loc * expr * expr
  | LambdaExpr of loc * int * expr array * expr
  | CallExpr of loc * expr * expr array

type statement
  = ReturnStmt of loc * expr
  | ExprStmt of loc * expr
  | BindStmt of loc * id * expr

type func =
  { id: id
  ; name: string
  ; num_params: int
  ; num_locals: int
  ; body: (statement list) option
  ; loc: loc
  }

type program = func array array

