(* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the code generator for x86.
 * The IR is traversed and x86 instructions emulating the
 * stack machine's operation are emitted.
 *)

open Ir

let compile_closure out { id; num_params; num_locals; name; insts; _ } =
  Printf.fprintf out "\t.text\n";
  Printf.fprintf out "_lambda_%d:\n" id;

  (match name with
  | None -> ()
  | Some n ->
    Printf.fprintf out ".global _lambda_%s\n" n;
    Printf.fprintf out "_lambda_%s:\n" n
  );

  Printf.fprintf out "\tpushq %%rbp\n";
  Printf.fprintf out "\tmovq %%rsp, %%rbp\n";
  if num_locals > 0 then
    Printf.fprintf out "\tsubq $%d, %%rsp\n" (8 * num_locals);
  Printf.fprintf out "\tpushq %%rax\n";

  Array.iter
    (fun inst -> match inst with
    | GetClosure c ->
      Printf.fprintf out "\tleaq _lambda_%d_closure(%%rip), %%rcx\n" c;
      Printf.fprintf out "\tpushq %%rcx\n"
    | GetBuiltin name ->
      Printf.fprintf out "\tleaq __builtin_%s_closure(%%rip), %%rcx\n" name;
      Printf.fprintf out "\tpushq %%rcx\n"
    | GetEnv i ->
      let offset = 8 + i * 8 in
      Printf.fprintf out "\tpushq %d(%%rax)\n" offset
    | GetArg i ->
      let offset = 16 + (num_params - i - 1) * 8 in
      Printf.fprintf out "\tpushq %d(%%rbp)\n" offset
    | GetLocal i ->
      Printf.fprintf out "\tpushq %d(%%rbp)\n" (-(i + 1) * 8);
    | SetLocal i ->
      Printf.fprintf out "\tpopq %%rcx\n";
      Printf.fprintf out "\tmovq %%rcx, %d(%%rbp)\n" (-(i + 1) * 8)
    | ConstInt i ->
      Printf.fprintf out "\tmovq $%d, %%rcx\n" i;
      Printf.fprintf out "\tpushq %%rcx\n"
    | Closure(i, num_capture) ->
      let size = num_capture * 8 + 8 in
      Printf.fprintf out "\tmovq $%d, %%rcx\n" num_capture;
      Printf.fprintf out "\tcallq __builtin_allocate\n";
      Printf.fprintf out "\tleaq _lambda_%d(%%rip), %%rdx\n" i;
      Printf.fprintf out "\tmovq %%rdx, (%%rcx)\n";
      for i = 0 to num_capture - 1 do
        Printf.fprintf out "\tpopq %%rdx\n";
        Printf.fprintf out "\tmovq %%rdx, %d(%%rcx)\n" (size - (i + 1) * 8);
      done;
      Printf.fprintf out "\tpushq %%rcx\n"
    | Add ->
      Printf.fprintf out "\tpopq %%rcx\n";
      Printf.fprintf out "\taddq %%rcx, (%%rsp)\n"
    | Call ->
      Printf.fprintf out "\tpopq %%rax\n";
      Printf.fprintf out "\tcallq *(%%rax)\n";
      Printf.fprintf out "\tpushq %%rcx\n"
    | Return ->
      Printf.fprintf out "\tpopq %%rcx\n";
      Printf.fprintf out "\tpopq %%rax\n";
      if num_locals > 0 then
        Printf.fprintf out "\taddq $%d, %%rsp\n" (8 * num_locals);
      Printf.fprintf out "\tpopq %%rbp\n";
      Printf.fprintf out "\tpopq %%rdx\n";
      if num_params > 0 then
        Printf.fprintf out "\taddq $%d, %%rsp\n" (8 * num_params);
      Printf.fprintf out "\tpushq %%rdx\n";
      Printf.fprintf out "\tretq\n";
    | Pop ->
      Printf.fprintf out "\tpopq %%rcx\n";
    ) insts;

  Printf.fprintf out "\t.data\n";
  Printf.fprintf out "\t.quad 0\n";
  Printf.fprintf out "_lambda_%d_closure:\n" id;
  Printf.fprintf out "\t.quad _lambda_%d\n" id

let compile prog out =
  Array.iter (compile_closure out) prog

