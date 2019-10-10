(* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the intermediate representation obtained
 * from the typed AST. It targets a stack machine, with instructions
 * documented individually below. At the highest level, the program
 * consists of a number of closures: toplevel functions themselves
 * are lowered to closures which do not capture anything.
 *
 * Some helper methods to dump the IR are also included.
 *)

type inst
  (* Pushes the address of a closure onto the stack. *)
  (* This is not a function - it is a block of memory whose first field is *)
  (* a pointer to the actual function. This is due to the lack of distinction *)
  (* between functions and closures in the runtime. *)
  = GetClosure of int
  (* Pushes a closure for a builtin, provided by the standard library. *)
  | GetBuiltin of string
  (* Pushes an environment variable, relative to the environment pointer. *)
  | GetEnv of int
  (* Pushes an argument - peeks up the stack. *)
  | GetArg of int
  (* Pushes a local onto the stack. *)
  | GetLocal of int
  (* Pops a value and sets a local. *)
  | SetLocal of int
  (* Pops a constant onto the stack. *)
  | ConstInt of int
  (* Pops a number of values and pushes a closure capturing them. *)
  | Closure of int * int
  (* Pops two values and pushes their sum. *)
  | Add
  (* Pops a closure and invokes it. *)
  | Call
  (* Pops a return value and returns. *)
  | Return
  (* Discards the value from the top of the stack. *)
  | Pop

type closure =
  { id: int
  ; name: string option
  ; num_params: int
  ; num_captures: int
  ; num_locals: int
  ; insts: inst array
  }

type program = closure array

let print_inst out inst =
  match inst with
  | GetClosure i  -> Printf.fprintf out "\tGetClosure(%d)\n" i
  | GetBuiltin n  -> Printf.fprintf out "\tGetBuiltin(%s)\n" n
  | GetEnv i      -> Printf.fprintf out "\tGetEnv(%d)\n" i
  | GetArg i      -> Printf.fprintf out "\tGetArg(%d)\n" i
  | GetLocal i    -> Printf.fprintf out "\tGetLocal(%d)\n" i
  | SetLocal i    -> Printf.fprintf out "\tSetLocal(%d)\n" i
  | ConstInt i    -> Printf.fprintf out "\tConstInt(%d)\n" i
  | Closure(i, n) -> Printf.fprintf out "\tClosure(%d, %d)\n" i n
  | Add           -> Printf.fprintf out "\tAdd\n"
  | Call          -> Printf.fprintf out "\tInvoke\n"
  | Return        -> Printf.fprintf out "\tReturn\n"
  | Pop           -> Printf.fprintf out "\tPop\n"

let print_closure out {id; name; num_params; num_captures; num_locals; insts} =
  Printf.fprintf out "%s#%d(%d, %d, %d):\n"
    (match name with None -> "" | Some n -> n)
    id
    num_params
    num_captures
    num_locals;
  Array.iter (print_inst out) insts

let print_program out prog =
  Array.iter (print_closure out) prog

