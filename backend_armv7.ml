(* Compiler Construction - Minimal Lambda Language
 *
 * This file defined the code generator for ARMv7.
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

  Printf.fprintf out "\tstmfd sp!, {r0, fp, lr}\n";
  Printf.fprintf out "\tmov fp, sp\n";
  if num_locals > 0 then
    Printf.fprintf out "\tsub sp, sp, #%d\n" (num_locals * 4);

  Array.iter
    (fun inst -> match inst with
    | GetClosure c ->
      Printf.fprintf out "\tldr r1, =_lambda_%d_closure\n" c;
      Printf.fprintf out "\tpush {r1}\n"
    | GetBuiltin name ->
      Printf.fprintf out "\tldr r1, =__builtin_%s_closure\n" name;
      Printf.fprintf out "\tpush {r1}\n"
    | GetEnv i ->
      Printf.fprintf out "\tldr r1, [r0, #%d]\n" (4 + i * 4);
      Printf.fprintf out "\tpush {r1}\n"
    | GetArg i ->
      let offset = 3 * 4 + (num_params - i - 1) * 4 in
      Printf.fprintf out "\tldr r1, [fp, #%d]\n" offset;
      Printf.fprintf out "\tpush {r1}\n"
    | GetLocal i ->
      Printf.fprintf out "\tldr r1, [fp, #%d]\n" (-(i + 1) * 4);
      Printf.fprintf out "\tpush {r1}\n"
    | SetLocal i ->
      Printf.fprintf out "\tpop {r1}\n";
      Printf.fprintf out "\tstr r1, [fp, #%d]\n" (-(i + 1) * 4)
    | ConstInt i ->
      Printf.fprintf out "\tldr r1, =%d\n" i;
      Printf.fprintf out "\tpush {r1}\n"
    | Closure(i, num_capture) ->
      let size = num_capture * 4 + 4 in
      Printf.fprintf out "\tmov r1, #%d\n" num_capture;
      Printf.fprintf out "\tbl __builtin_allocate\n";
      Printf.fprintf out "\tldr r2, =_lambda_%d\n" i;
      Printf.fprintf out "\tstr r2, [r1]\n";
      for i = 0 to num_capture - 1 do
        Printf.fprintf out "\tpop {r2}\n";
        Printf.fprintf out "\tstr r2, [r1, #%d]\n" (size - (i + 1) * 4);
      done;
      Printf.fprintf out "\tpush {r1}\n"
    | Add ->
      Printf.fprintf out "\tpop {r1}\n";
      Printf.fprintf out "\tpop {r2}\n";
      Printf.fprintf out "\tadd r1, r1, r2\n";
      Printf.fprintf out "\tpush {r1}\n";
    | Call ->
      Printf.fprintf out "\tpop {r0}\n";
      Printf.fprintf out "\tldr r1, [r0]\n";
      Printf.fprintf out "\tblx r1\n";
      Printf.fprintf out "\tpush {r1}\n";
    | Return ->
      Printf.fprintf out "\tpop {r1}\n";
      if num_locals > 0 then
        Printf.fprintf out "\tadd sp, sp, #%d\n" (num_locals * 4);
      Printf.fprintf out "\tldmfd sp!, {r0, fp, lr}\n";
      if num_params > 0 then
        Printf.fprintf out "\tadd sp, sp, #%d\n" (num_params * 4);
      Printf.fprintf out "\tmov pc, lr\n"
    | Pop ->
      Printf.fprintf out "\tpop {r1}\n"
    ) insts;

  Printf.fprintf out "\t.data\n";
  Printf.fprintf out "\t.word 0\n";
  Printf.fprintf out "_lambda_%d_closure:\n" id;
  Printf.fprintf out "\t.word _lambda_%d\n" id

let compile prog out =
  Array.iter (compile_closure out) prog

