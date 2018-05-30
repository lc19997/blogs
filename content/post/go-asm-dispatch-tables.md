+++
date = "Thu May 31 01:39:59 MSK 2018"
title = "Dispatch tables in Go asm"
tags = [
    "[go]",
    "[asm]",
]
description = "Bytecode interpreter in Go asm using direct threading for dispatching."
draft = false
+++

## Dispatch tables

When you want to execute particular code path depending on some kind
of tag/opcode or other integer value that can be easily mapped into index,
dispatch tables can speed things up compared to the sequence of
comparisons and conditional jumps.

In interpreters, this technique is often used as an alternative to switch-based dispatch.  
It's called direct threading in that domain. Each opcode corresponds to table index that contains machine
code address that can execute operation specified by the opcode.

## Threaded code in Intel syntax

Suppose we're implementing some virtual machine for a toy programming language.

All our VM can do is to add and substract from `AX` (`rax`) register.  
Suppose `CX` (`rcx`) always points to opcode stream, it's our program counter.

With [nasm](https://www.nasm.us/) and Intel syntax, our code could look like this:

```x86asm
$op_labels:
  dq op_exit ;; Stop the evaluation
  dq op_add1 ;; Add 1 to RAX
  dq op_sub1 ;; Sub 1 from RAX

%macro next_op 0
  movzx rdx, byte [rcx]        ;; Fetch opcode
  add rcx, 1                   ;; Advance PC (inc instruction is OK here too)
  jmp [$op_labels + (rdx * 8)] ;; Execute the operation
%endmacro

eval:
  next_op

op_exit:
  ret

op_add1:
  add rax, 1
  next_op

op_sub1:
  sub rax, 1
  next_op
```

Now, the question is: how to do exactly the same thing in Go assembly?

## Go implementation

In Go assembly, it's not possible to have global labels.
It's also not possible to store label address into anything.
`TEXT` blocks are our replacements here.

```x86asm
//; The $sym syntax is required to get symbol address, "literal value".
//; op_exit, op_add1 and op_sub1 are declared as TEXT blocks, like normal functions.
DATA op_labels<>+0(SB)/8, $op_exit(SB)
DATA op_labels<>+8(SB)/8, $op_add1(SB)
DATA op_labels<>+16(SB)/8, $op_sub1(SB)
GLOBL op_labels<>(SB), RODATA|NOPTR, $24
```

Macros are akin to C.  
Multiline macros require newline escapes.

```x86asm
#define next_op \
  MOVBQZX (CX), DX \
  ADDQ $1, CX \
  MOVQ $op_labels<>(SB), DI \
  JMP (DI)(DX*8)
```

You may notice that there is one excessive `MOVQ` there.  
There is an explanation [in the end of the article](#why-additional-movq-in-next-op).

```x86asm
//; We are going to go one step further and return AX value to the caller.
TEXT op_exit(SB), NOSPLIT, $0-0
  MOVQ AX, ret+8(FP)
  RET

TEXT op_add1(SB), NOSPLIT, $0-0
  ADDQ $1, AX
  next_op

TEXT op_sub1(SB), NOSPLIT, $0-0
  SUBQ $1, AX
  next_op
```

Note that all routines defined above have zero size frame and parameters space.
This is to emphasise that those functions are not `CALL`'ed but rather `JMP`'ed into.

The last thing is entry point, `eval` function.
It's signature in Go would look like this:

```go
func eval(opbytes *byte) int64
```

For asm, it's important to consider stack frame size and parameters width.
These are shared among all opcode executing routines.
We don't need stack frame, 16 bytes for input pointer and output int64.
(Our code is for 64-bit platform only, but you can make it more portable.)

```x86asm
TEXT ·eval(SB), NOSPLIT, $0-16
  MOVQ opbytes+0(FP), CX //; Set up program counter (PC)
  XORQ AX, AX            //; Accumulator always starts with 0
  next_op                //; Start the evaluation
```

See [eval_amd64.s](/blog/code/eval_amd64.s) for complete asm code.

## Calling eval from Go

Main function can look like this:

```go
func main() {
	const (
		opExit = iota
		opAdd1
		opSub1
	)
	prog := []byte{
		opAdd1,
		opAdd1,
		opSub1,
		opAdd1,
		opExit,
	}
	fmt.Println(eval(&prog[0]))
}
```

Constants defined purely for convenience reasons.
It is important to keep definitions in sync with asm implementation.
Code generation can help here.

See [eval.go](/blog/code/eval.go) for complete Go code.

If you put `eval.go` and `eval_amd64.s` in one directory, running  
`$ go build -o eval.exe . && ./eval.exe` should be enough to execute the program.

## Why additional MOVQ in next_op?

Direct translation of `next_op` would be:

```x86asm
#define next_op \
  MOVBQZX (CX), DX \
  ADDQ $1, CX \
  JMP $op_labels<>(SB)(DX*8)
```

This way, it would fully match nasm implementation.

But unfortunately, this is not a valid Go asm syntax.

You can't use index expressions while using pseudo register.  
And you can't access global data without `SB` pseudo register.

This could be fixed in future, although the probability is pretty low.  
Weird syntax is derived from plan9 asm and is shared among multiple architectures.
