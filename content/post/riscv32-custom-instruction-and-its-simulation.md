+++
date = "2017-06-21"
title = "RISC-V: custom instruction and its simulation"
tags = [
    "[gcc]",
    "[gcc plugin]",
    "[risc-v]",
    "[tutorial]",
    "[hardcore]",
    "[compilers]",
]
description = "Trying to enumerate what C defaults are wrong."
draft = false
+++

## Agenda

This article shows how to add a new instruction to RISC-V and simulate it.

These topics are covered along the way:

- Whole GNU `riscv` toolchain installation;
- Implementation of a new instruction for `spike` RISC-V ISA simulator;
- Manual instruction encoding in C/C++;
- Custom instruction simulation (with visible output);
- [riscv32-]GCC plugin development;
 
You may find [associated repository](https://github.com/Quasilyte/gnu-riscv32_ext) useful.

Many things can go wrong. 
Be prepared to fix upcoming issues by yourself.  
The final result is very rewarding, I promise.

## Toolchain installation

Choose installation directory. Call it `RISCV`.

Add these lines to your `~/.bashrc`:

```bash
    # Directory which will contain everything we need.
export RISCV_HOME=~/riscv-home
    # $RISCV will point to toolchain install location.
export RISCV="${RISCV_HOME}/riscv"
export PATH="${PATH}:${RISCV}/bin"
```

Run `mkdir -p "${RISCV_HOME}" "${RISCV}"`.

Use [1_install/2_download-repos](https://github.com/Quasilyte/gnu-riscv32_ext/blob/master/1_install/2_download-repos) script to clone all required repositories.

If you wish to save some time and traffic, avoid recursive clone of
toolchain repository. Instead, clone sub-modules by hand.
You may exclude "riscv-glibc".

> Be warned: I have not tested partial toolchain build, caveat emptor

Satisfy [GNU toolchain](https://github.com/riscv/riscv-gnu-toolchain) 
prerequisites by installing all required packages.
In addition, spike requires `device-tree-compiler` package.

We choose:

- RISCV32 over RISCV64
- newlib over glibc

Repositories must be built in this order:

1. riscv-gnu-toolchain
2. riscv-fesvr, riscv-pk
3. riscv-isa-sim

You can use [1_install/3_build-repos](https://github.com/Quasilyte/gnu-riscv32_ext/blob/master/1_install/build-repos)
script as a guideline.

To check installation, use [1_install/4_check-install](https://github.com/Quasilyte/gnu-riscv32_ext/blob/master/1_install/check-install).

## Custom instruction description

Within the framework of this article, we will implement [mac](https://en.wikipedia.org/wiki/Multiply%E2%80%93accumulate_operation) instruction.

`rv32im` has `mul` and `add` instructions, `mac` combines them.  
It defined as `a0 := a0 + a1 * a2` (ordinary 3-address instruction).

```ruby
# Without mac (preserve registers):
mv t0, a0      # addi r0, a0, 0	
mul a1, a2, a3
add a1, a1, t0
# With mac:
mac a1, a2, a3
```

## Adding "mac" instruction to the rv32im

To add an instruction to the simulator:
1. Describe the instruction's functional behavior;
2. Add the opcode and opcode mask to "riscv/opcodes.h";

First step is accomplished by adding a `riscv/insns/mac.h` file:

```c++
/* file "$RISCV_HOME/riscv-isa-sim/riscv/insns/mac.h" */
// 'M' extension means we require integer mul/div standard extension.
require_extension('M');
// RD = RD + RS1 * RS2
reg_t tmp = sext_xlen(RS1 * RS2);
WRITE_RD(sext_xlen(READ_REG(insn.rd()) + tmp));
```

For the second step, we use [riscv-opcodes](https://github.com/riscv/riscv-opcodes).

```bash
cd "${RISCV_HOME}/riscv-opcodes"
echo -e "mac rd rs1 rs2 31..25=1 14..12=0 6..2=0x1A 1..0=3\n" >> opcodes
make install
```

It turns out there is a third step which is not documented.
New entry must be added to the `riscv_insn_list`.

```bash
sed -i 's/riscv_insn_list = \\/riscv_insn_list = mac\\/g' \
    "${RISCV_HOME}/riscv-isa-sim/riscv/riscv.mk.in"
```

Rebuild the simulator.

```bash
cd "${RISCV}/riscv-isa-sim/build"
sudo make install
```

## Testing rv32im brand new instruction

At this stage:

- Compiler knows nothing about `mac`. It can not emit that instruction;
- Assembler knows nothing about `mac`. We can not use `mac` in inline assembly;

Our last resort is manual encoding.

```c
#include <stdio.h>
// Needed to verify results.
int mac_c(int a, int b, int c) {
    a += b * c; // Semantically, it is "mac"
    return a;
}
// Should not be inlined, because we expect arguments
// in particular registers.
__attribute__((noinline))
int mac_asm(int a, int b, int c) {
    asm __volatile__ (".word 0x02C5856B\n");
    return a;
}
int main(int argc, char** argv) {
    int a = 2, b = 3, c = 4;
    printf("%d =?= %d\n", mac_c(a, b, c), mac_asm(a, b, c));
}
```

Save test program as `test_mac.c`.

```bash
riscv32-unknown-elf-gcc test_mac.c -O1 -march=rv32im -o test_mac
spike --isa=RV32IM "${RISCV_PK}" test_mac
```

You should see `14 =?= 14` printed to stdout.  
If result differs, `riscv32-unknown-elf-gdb` can help you in troubleshooting.

## Mac encoding explained

Be sure to look at [official specifications](https://riscv.org/specifications/) if
you aim for precise descriptions.

`mac` will mimic `mul` encoding, but use different opcode.

```ruby
# file "riscv-opcodes/opcodes"
#                                differs
#                                |
#                                v
mac rd rs1 rs2 31..25=1 14..12=0 6..2=0x1A 1..0=3
mul rd rs1 rs2 31..25=1 14..12=0 6..2=0x0C 1..0=3
#   ^  ^   ^   ^        ^        ^         ^
#   |  |   |   |        |        |         |
#   |  |   |   |        |        |         |
#   |  |   |   |        |        |         also opcode 3 bits
#   |  |   |   |        |        opcode 5 bits
#   |  |   |   |        funct3 3 bits
#   |  |   |   funct7 7 bits
#   |  |   rs2 (src2) 5 bits
#   |  rs1 (src1) 5 bits
#   dest 5 bits
```

Actual encoding has different order of components and opcode is
really single 7 bit segment. 

> 5 bits per register operand means that we have 32 addressable registers.

```ruby
# Encoding used for "mac a0, a1, a2"
0x02C5856B [base 16]
==
10110001011000010101101011 [base 2]
== 
00000010110001011000010101101011 [base 2]
# Group by related bit chunks:
0000001 01100 01011 000 01010 1101011
^       ^     ^     ^   ^     ^
|       |     |     |   |     |
|       |     |     |   |     opcode (6..2=0x0C 1..0=3)
|       |     |     |   dest (10 : a0)
|       |     |     funct3 (14..12=0)
|       |     src1 (11 : a1)
|       src2 (12 : a2)
funct7 (31..25=1)
```

<img src="/blog/img/reg_table.png">

## Plugin vs patch

There are two ways to extend GCC:

1. Patch GCC itself
2. Write loadable plugin for GCC

Prefer plugins to GCC patches whenever possible.  
GCC wiki ["plugins"](https://gcc.gnu.org/wiki/plugins) page described
advantages in the "Background" section.

In this guide, both methods will be covered.

Useful links:

- [Simple GCC plugin](http://thinkingeek.com/2015/08/16/a-simple-plugin-for-gcc-part-1/) series of posts
- [GCC plugins manual](https://gcc.gnu.org/onlinedocs/gccint/Plugins.html#Plugins)

## GCC "rv32imMac" plugin

**TODO**

## GIMPLE "gmac" statement

**TODO**

## The pleasure of intrinsics

**TODO**

## Compiling "mac" without intrinsic

**TODO**
