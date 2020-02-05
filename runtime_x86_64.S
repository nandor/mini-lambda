################################################################################
# Compiler Construction - Minimal Lambda Language
################################################################################

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
  #define G(x) x
#elif __APPLE__
  #define G(x) _##x
#elif __linux__
  #define G(x) x
#else
#   error "Unknown compiler"
#endif

################################################################################
# main
################################################################################
  .text
  .global G(main)
G(main:)
  .global _lambda_main
  jmp _lambda_main


################################################################################
# __builtin_input_int
################################################################################
  .text
  .global __builtin_input_int
__builtin_input_int:
  pushq %rbp
  movq %rsp, %rbp
  pushq %rax

  # Align the stack to a 16-byte boundary and allocate some space.
  movq %rsp, %rbx
  subq $32, %rsp
  andq $-16, %rsp

  # Invoke scanf.
  leaq  scanf_format(%rip), %rdi
  movq  %rsp, %rsi
  callq G(scanf)

  # Load the result.
  movq (%rsp), %rcx

  # Restore the stack.
  movq %rbx, %rsp

  popq %rax
  popq %rbp
  retq

  .data
scanf_format:
  .string "%d"

  .data
  .p2align 2
  .global __builtin_input_int_closure
  .quad 0
__builtin_input_int_closure:
  .quad __builtin_input_int



################################################################################
# __builtin_print_int
################################################################################
  .text
  .global __builtin_print_int
__builtin_print_int:
  # Save previous frame.
  pushq %rbp
  movq %rsp, %rbp
  pushq %rax

  # Align the stack to a 16-byte boundary.
  movq %rsp, %rbx
  subq $16, %rsp
  andq $-16, %rsp

  # Invoke printf.
  leaq  printf_format(%rip), %rdi
  movq  16(%rbp), %rsi
  callq G(printf)
  xorq  %rcx, %rcx

  # Restore previous frame.
  movq %rbx, %rsp
  popq %rax
  popq %rbp

  # Pop the argument to the function.
  popq %rdx
  addq $8, %rsp
  pushq %rdx
  retq

  .data
printf_format:
  .string "%d\n"

  .data
  .global __builtin_print_int_closure
  .quad 0
__builtin_print_int_closure:
  .quad __builtin_print_int



################################################################################
# __builtin_print_int
################################################################################
  .text
  .global __builtin_allocate
__builtin_allocate:
  pushq %rbp
  movq %rsp, %rbp
  pushq %rax

  # Align the stack to a 16-byte boundary.
  movq %rsp, %rbx
  subq $16, %rsp
  andq $-16, %rsp

  # Compute the size of the block: 8 * fields + header + code pointer
  leaq 1(%rcx), %r12
  leaq 16(,%rcx, 8), %rdi
  # Invoke malloc.
  callq G(malloc)
  # Store header.
  movq %r12, (%rax)
  # Return pointer in %rcx.
  leaq 8(%rax), %rcx

  # Restore stack.
  movq %rbx, %rsp

  popq %rax
  popq %rbp
  retq


