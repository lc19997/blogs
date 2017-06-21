+++
date = "2017-06-21"
title = "RISC-V: custom instruction and its simulation"
tags = [
    "[gcc]",
    "[gcc plugin]",
    "[risc-v]",
    "[tutorial]",
    "[hardcore]",
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

```bash
    # ~/.bashrc
    # Directory which will contain everything we need.
export RISCV_HOME=~/riscv-home
    # $RISCV will point to toolchain install location.
export RISCV="${RISCV_HOME}/riscv"
export PATH="${PATH}:${RISCV}/bin"
```

Go to that directory and clone all required repositories.
The process may take more than a hour; you may want to download
them in parallel.

```bash
mkdir -p "${RISCV_HOME}" "${RISCV}"
cd $RISCV
    # RISC-V frontend server. Needed for spike simulator.
git clone https://github.com/riscv/riscv-fesvr
    # Proxy kernel. Needed for spike simulator.
git clone https://github.com/riscv/riscv-pk
    # The spike itself.
git clone https://github.com/riscv/riscv-isa-sim
    # Useful helper package. Makes it easier to add a new opcode.
git clone https://github.com/riscv/riscv-opcodes
    # "riscv-opcodes" expects "riscv-tests" to be present in $RISCV_HOME.
git clone https://github.com/riscv/riscv-tests.git
    # This will take awhile... The longest step.
    # Toolchain contains riscv ports of binutils, GCC and more.
git clone git clone --recursive https://github.com/riscv/riscv-gnu-toolchain
```

If you wish to save some time and traffic, avoid recursive clone of
toolchain repository. Instead, clone sub-modules by hand.
You may exclude "riscv-glibc". Not sure about "riscv-dejagnu" though.  

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

**TODO**

## Adding "mac" intrinsic to the GCC

**TODO**

## Mac GIMPLE statement

**TODO**

## The pleasure of intrinsics

**TODO**

## Compiling "mac" without intrinsic

**TODO**
