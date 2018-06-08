+++
date = "Fri Jun  8 13:29:13 MSK 2018"
title = "Disassembling Go AVX-512"
tags = [
    "[go]",
    "[asm]",
    "[avx-512]",
    "[Intel XED]",
]
description = "objdump that is distributed with Go can't handle AVX-512 yet. This article describes workarounds."
draft = false
+++

## The problem

Go 1.11 got updated assembler that supports AVX-512, but disassembler is left unchanged.  
In other words, `go tool asm` speaks AVX-512, `go tool objdump` does not.

Suppose we have this `avx.s` file:

```x86asm
TEXT ·avxCheck(SB), 0, $0
        VPOR X0, X1, X2             //; AVX1
        VPOR Y0, Y1, Y2             //; AVX2
        VPORD.BCST (DX), Z1, K2, Z2 //; AVX-512
        RET
```

You will be surprised after assemble+disassemble attempt:

```bash
$ go tool asm avx.s
$ go tool objdump avx.o

TEXT ·avxCheck(SB) gofile..$GOROOT/avx.s
  avx.s:2       0xb7            c5f1ebd0        JMP 0x8b
  avx.s:3       0xbb            c5f5ebd0        JMP 0x8f
  avx.s:4       0xbf            62              ?
  avx.s:4       0xc0            f1              ICEBP
  avx.s:4       0xc1            755a            JNE 0x11d
  avx.s:4       0xc3            eb12            JMP 0xd7
  avx.s:5       0xc5            c3              RET
```

Rest of this article described how to overcome this situation.

## 1. System objdump over binary

System [objdump](https://linux.die.net/man/1/objdump) can't handle Go object
files as they are not ELF library files (`e_type=1`) but rather internal to Go wire format.

```bash
$ objdump -D avx.o

objdump: avx.o: File format not recognized
```

We can make it work though.  
To do so, we need to build executable that `objdump` can understand.

First off, we add `main.go`:

```go
package main

func avxCheck()

func main() {
    avxCheck()
}
```

It's now possible to build `avxCheck` along with main package.

```bash
go build -o avxcheck .

$ objdump -D avxcheck | sed '/<main.avxCheck>:/,/^$/!d'

000000000044e580 <main.avxCheck>:
  44e580:   c5 f1 eb d0             vpor   %xmm0,%xmm1,%xmm2
  44e584:   c5 f5 eb d0             vpor   %ymm0,%ymm1,%ymm2
  44e588:   62 f1 75 5a eb 12       vpord  (%rdx){1to16},%zmm1,%zmm2{%k2}
  44e58e:   c3                      retq
  44e58f:   cc                      int3
```

## 2. System objdump over shellcode

Assembling `avx.s` with `-S` flag almost yields wanted results:

```bash
$ go tool asm -S avx.s

avxCheck STEXT nosplit size=15 args=0xffffffff80000000 locals=0x0
    0x0000 00000 (avx.s:1)  TEXT    avxCheck(SB), NOSPLIT, $0
    0x0000 00000 (avx.s:2)  VPOR    X0, X1, X2
    0x0004 00004 (avx.s:3)  VPOR    Y0, Y1, Y2
    0x0008 00008 (avx.s:4)  VPORD.BCST  (DX), Z1, K2, Z2
    0x000e 00014 (avx.s:5)  RET
    0x0000 c5 f1 eb d0 c5 f5 eb d0 62 f1 75 5a eb 12 c3     ........b.uZ...
go.info.avxCheck SDWARFINFO size=34
    0x0000 02 61 76 78 43 68 65 63 6b 00 00 00 00 00 00 00  .avxCheck.......
    0x0010 00 00 00 00 00 00 00 00 00 00 01 9c 00 00 00 00  ................
    0x0020 01 00
```

Function body is `c5 f1 eb d0 c5 f5 eb d0 62 f1 75 5a eb 12 c3`.

These bytes definitely include 4 instructions from `avxCheck` function,
but it's hard to associate octets with instructions they encode.
The're all intermixed.

`objdump` does support raw shellcode input format.  
All we need to do is to turn hex octets into that.

```bash
$ echo 'c5 f1 eb d0 c5 f5 eb d0 62 f1 75 5a eb 12 c3' |
    xxd -r -p > code.bin
$ objdump -b binary -m i386 -D code.bin

Disassembly of section .data:

00000000 <.data>:
   0:   c5 f1 eb d0             vpor   %xmm0,%xmm1,%xmm2
   4:   c5 f5 eb d0             vpor   %ymm0,%ymm1,%ymm2
   8:   62 f1 75 5a eb 12       vpord  (%edx){1to16},%zmm1,%zmm2{%k2}
   e:   c3                      ret
```

## 3. Intel XED CLI

[Intel XED](https://github.com/intelxed/xed) includes several useful [command-line tools](https://intelxed.github.io/ref-manual/group__EXAMPLES.html).

One of them is called `xed`. It's capable of encoding and decoding x86 instructions.

```bash
$ echo 'c5 f1 eb d0 c5 f5 eb d0 62 f1 75 5a eb 12 c3' > code.txt
$ xed -64 -A -ih code.txt

00: LOGICAL  AVX         C5F1EBD0      vpor %xmm0, %xmm1, %xmm2
04: LOGICAL  AVX2        C5F5EBD0      vpor %ymm0, %ymm1, %ymm2
08: LOGICAL  AVX512EVEX  62F1755AEB12  vpordl  (%rdx){1to16}, %zmm1, %zmm2{%k2}
0e: RET      BASE        C3            retq
```

Decoding single instruction is even simpler:

```bash
$ xed -64 -A -d '62 f1 75 5a eb 12'

62F1755AEB12
ICLASS: VPORD   CATEGORY: LOGICAL   EXTENSION: AVX512EVEX  IFORM: VPORD_ZMMu32_MASKmskw_ZMMu32_MEMu32_AVX512   ISA_SET: AVX512F_512
SHORT: vpordl  (%rdx){1to16}, %zmm1, %zmm2{%k2}
```

Without `-A` flag, it will print instructions in Intel syntax:

```bash
$ xed -64 -d '62 f1 75 5a eb 12'

62F1755AEB12
ICLASS: VPORD   CATEGORY: LOGICAL   EXTENSION: AVX512EVEX  IFORM: VPORD_ZMMu32_MASKmskw_ZMMu32_MEMu32_AVX512   ISA_SET: AVX512F_512
SHORT: vpord zmm2{k2}, zmm1, dword ptr [rdx]{1to16}
```

In addition, there is also `xed-ex4`, which prints many interesting details about instruction being decoded:

```bash
$ xed-ex4 -64 C5 F1 EB D0

PARSING BYTES: c5 f1 eb d0
  VPOR VPOR_XMMdq_XMMdq_XMMdq
  EASZ:3,
  EOSZ:2,
  HAS_MODRM:1,
  LZCNT,
  MAP:1,
  MAX_BYTES:4,
  MOD:3,
  MODE:2,
  MODRM_BYTE:208,
  NOMINAL_OPCODE:235,
  OUTREG:XMM0,
  P4,
  POS_MODRM:3,
  POS_NOMINAL_OPCODE:2,
  REG:2,
  REG0:XMM2,
  REG1:XMM1,
  REG2:XMM0,
  SMODE:2,
  TZCNT,
  VEXDEST210:6,
  VEXDEST3,
  VEXVALID:1,
  VEX_PREFIX:1
0       REG0/W/DQ/EXPLICIT/NT_LOOKUP_FN/XMM_R
1       REG1/R/DQ/EXPLICIT/NT_LOOKUP_FN/XMM_N
2       REG2/R/DQ/EXPLICIT/NT_LOOKUP_FN/XMM_B
YDIS: vpor xmm2, xmm1, xmm0
ATT syntax: vpor %xmm0, %xmm1, %xmm2
INTEL syntax: vpor xmm2, xmm1, xmm0
```

Its output requires XED knowledge in order to be fully understood, but if you're
excited, I'm suggesting you to read the documentation and/or sources and achieve enlightenment.

## Prefix-only disassembling

If, for whatever reason, you only want to inspect prefix details, there is [vexdump](https://github.com/Quasilyte/tools/tree/master/src/vexdump)
utility which can be used to do just that.

Dump single instruction prefix info:

```bash
$ vexdump 6272fd098ae8

EVEX rxbR00mm Wvvvv1pp zLlbVaaa opcode modrm    fields
62   01110010 11111101 00001001 8A     11101000 EVEX.128.66.0F38.W1
```

Dump multiple instructions (most probably for comparison):

```bash
$ vexdump 6272FD098AE8 '62 72 fd 09 8a c5'

EVEX rxbR00mm Wvvvv1pp zLlbVaaa opcode modrm    fields
62   01110010 11111101 00001001 8A     11101000 EVEX.128.66.0F38.W1
62   01110010 11111101 00001001 8A     11000101 EVEX.128.66.0F38.W1
```

It can also dump mixed prefixes:

```bash
$ vexdump 6272fd098ae8 6272fd098ac5 c4e1315813 c5b15813 c5f877

VEX2 rvvvvlpp opcode modrm    fields
C5   10110001 58     00010011 VEX.128.66.0F.W0
C5   11111000 77     00000000 VEX.128.0F.W0
VEX3 rxbmmmmm Wvvvvlpp opcode modrm    fields
C4   11100001 00110001 58     00010011 VEX.128.66.0F.W0
EVEX rxbR00mm Wvvvv1pp zLlbVaaa opcode modrm    fields
62   01110010 11111101 00001001 8A     11101000 EVEX.128.66.0F38.W1
62   01110010 11111101 00001001 8A     11000101 EVEX.128.66.0F38.W1
```

That tool can be especially helpful for encoder validation.
