################################################################################
# Compiler Construction - Minimal Lambda Language
################################################################################


################################################################################
# main
################################################################################
  .text
  .global main
main:
  stmfd sp!, {lr}
  .global _lambda_main
  bl _lambda_main
  ldmfd sp!, {pc}


################################################################################
# __builtin_input_int
################################################################################
  .text
  .global __builtin_input_int
__builtin_input_int:
  stmfd sp!, {r0, fp, lr}

  # Allocate a word to store the result of scanf.
  sub   sp, sp, #4
  # Invoke scanf.
  ldr   r0, =scanf_format
  mov   r1, sp
  bl    scanf
  # Load the result.
  ldr   r1, [sp]
  # Deallocate the word.
  add sp, sp, #4

  ldmfd sp!, {r0, fp, pc}

  .data
scanf_format:
  .string "%d"

  .data
  .p2align 2
  .global __builtin_input_int_closure
  .word 0
__builtin_input_int_closure:
  .word __builtin_input_int



################################################################################
# __builtin_print_int
################################################################################
  .text
  .global __builtin_print_int
__builtin_print_int:
  stmfd sp!, {r0, fp, lr}

  # Invoke printf.
  ldr  r0, =printf_format
  ldr  r1, [sp, #12]
  bl   printf

  eor  r1, r1, r1

  ldmfd sp!, {r0, fp, lr}
  add sp, sp, #4
  mov pc, lr


  .data
printf_format:
  .string "%d\n"

  .data
  .global __builtin_print_int_closure
  .word 0
__builtin_print_int_closure:
  .word __builtin_print_int



################################################################################
# __builtin_print_int
################################################################################
  .text
  .global __builtin_allocate
__builtin_allocate:
  stmfd sp!, {r0, fp, lr}

  add r0, r1, #2
  lsl r0, r0, #2
  push {r1}
  bl malloc
  pop {r1}
  add r1, r1, #1
  str r1, [r0]
  add r1, r0, #4

  ldmfd sp!, {r0, fp, pc}


