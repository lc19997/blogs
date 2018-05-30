#include "textflag.h"

DATA op_labels<>+0(SB)/8, $op_exit(SB)
DATA op_labels<>+8(SB)/8, $op_add1(SB)
DATA op_labels<>+16(SB)/8, $op_sub1(SB)
GLOBL op_labels<>(SB), (RODATA|NOPTR), $24

#define next_op \
  MOVBQZX (CX), DX \
  ADDQ $1, CX \
  MOVQ $op_labels<>(SB), DI \
  JMP (DI)(DX*8)

TEXT ·eval(SB), NOSPLIT, $0-16
  MOVQ opbytes+0(FP), CX //; Set up program counter (PC)
  XORQ AX, AX            //; Accumulator always starts with 0
  next_op                //; Start the evaluation

TEXT op_exit(SB), NOSPLIT, $0-0
  MOVQ AX, ret+8(FP)
  RET

TEXT op_add1(SB), NOSPLIT, $0-0
  ADDQ $1, AX
  next_op

TEXT op_sub1(SB), NOSPLIT, $0-0
  SUBQ $1, AX
  next_op

