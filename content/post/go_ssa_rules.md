+++
date = "Sat Jun 30 18:53:25 MSK 2018"
title = "Go compiler: SSA optimization rules description language"
tags = [
    "[go]",
    "[asm]",
    "[compilers]",
    "[ssa]",
    "[habr-translation]",
]
description = "Learn how to write new SSA optimization rules for Go compiler."
draft = false
+++

![](https://habrastorage.org/webt/0b/vq/ua/0bvquazri632jnibzwkywt7hfim.png)

> Original (ru): https://habr.com/post/415771/.

Go compiler uses its own Lisp-like domain-specific language ([DSL](https://en.wikipedia.org/wiki/Domain-specific_language)) for [Static Single Assignment](https://en.wikipedia.org/wiki/Static_single_assignment_form) (SSA) optimization rules description.

Lets dig into that language, its peculiarities and limitations. As an excercise, we'll add a new optimization rule into Go compiler that would optimize expressions like `a*b+c` using new operations we're going to implement along the way.

This is the first article in the series about [Go compiler SSA backend](https://github.com/golang/go/tree/master/src/cmd/compile/internal/ssa), this is why I've included some fundamental and architectural info besides DSL overview.

## Introduction

Go compiler frontend boundary ends when SSA form is generated. Functions that perform that translation can be found at [cmd/compile/internal/gc/ssa.go](https://github.com/golang/go/blob/master/src/cmd/compile/internal/gc/ssa.go). `ssa.Compile` function is an entry point for compiler SSA backend, it's defined in [cmd/compile/internal/ssa/compile.go](https://github.com/golang/go/blob/master/src/cmd/compile/internal/ssa/compile.go).

<details>
<summary>Terminology</summary>

| Term | Description |
|------|-------------|
| Compiler frontend | Lexing, parsing, typechecking. Intermediate representation is close to the source code structure. [wiki](https://en.wikipedia.org/wiki/Compiler#Front_end) |
| Compiler backend | Lower level optimizations and intermediate representations, code generation. [wiki](https://en.wikipedia.org/wiki/Compiler#Back_end) |
| Form | Used here as "expression" synonym. Originates from Lisps, where `form` is quite usual term when referring to atom/list. |
| Optimization pass | Execution of particular algorithm over the SSA program. Optimization usually consists of several such passes. |

</details>

SSA optimizer consists of several optimization passes. Every such pass traverses a body of a function that is being compiled, doing actions like removing dead nodes (values) and replacements of one forms in favor of others that are potentially more efficient. Some of these passes use "rewrite rules" that perform such SSA updates.

Rewrite rules are described using [S-expressions](https://en.wikipedia.org/wiki/S-expression). These `S-expressions` encode [ssa.Value](https://github.com/golang/go/blob/master/src/cmd/compile/internal/ssa/value.go#L19) nodes and form something like a [CFG](https://en.wikipedia.org/wiki/Control_flow_graph) of a program. In the simplest case, rewrite rule replaces one `ssa.Value` with another.

For example, this rule folds 8-bit constants multiplication:
```lisp
(Mul8 (Const8 [c]) (Const8 [d])) -> (Const8 [int64(int8(c*d))])
```

There are two main categories for SSA values: high-level, almost completely machine-independent and ones that are architecture-dependent (they usually map to native instructions in `1-to-1` fashion).

Optimizations are described in terms of these two categories. High-level optimizations that are shared among all targets go first, then target-specific rules are applied.

All rules-specific code is located at [cmd/compile/internal/ssa/gen](https://github.com/golang/go/tree/master/src/cmd/compile/internal/ssa/gen).<br>
We'll be touching only these two sets:

1. [genericOps.go](https://github.com/golang/go/blob/master/src/cmd/compile/internal/ssa/gen/genericOps.go) - machine-independent operations.
2. [AMD64Ops.go](https://github.com/golang/go/blob/master/src/cmd/compile/internal/ssa/gen/AMD64Ops.go) - `GOARCH=AMD64`-specific operations.

After next few passes that operate on the abstract machine model, so-called "lowering" is applied, which performs a transition from `genericOps` into arch-specific set. For our case, this operation set is `AMD64Ops`. All `lower` following passes operate on the operations from the second category.

After all optimizations are done, code generator plays its role. AMD64 code generation implementation can be found inside [cmd/compile/internal/amd64](https://github.com/golang/go/tree/master/src/cmd/compile/internal/amd64) package. Code generator purpose is to replace `ssa.Block` and `ssa.Value` objects with a sequence of corresponding [obj.Prog](https://github.com/golang/go/blob/0dc814cd7f6a5c01213169be17e823b69e949ada/src/cmd/internal/obj/link.go#L271)s that are passed to the [x86 assembler](https://github.com/golang/go/tree/master/src/cmd/internal/obj/x86). Assembler emits machine code which will become executable after [linking](https://github.com/golang/go/tree/master/src/cmd/link) is done.

# Optimization rules

Files that define operations have "`${ARCH}Ops.go`" name pattern.<br>
Optimization rules have "`${ARCH}.Rules`" filename pattern.

High-level (generic) rules perform some simple rewrites, most of the [constant folding](https://en.wikipedia.org/wiki/Constant_folding) and some other transformations that make further processing easier.

Every arch-specific `Rules` file consist of two parts:

1. Lowering that replace abstract operations to a more concrete machine-specific equivalents.
2. Optimizations themselves.

Operation lowering example:
```lisp
(Const32 [val]) -> (MOVLconst [val]) //; L - long, 32-bit
(Const64 [val]) -> (MOVQconst [val]) //; Q - quad, 64-bit 
 |                  |
 generic op         |
                   AMD64 op
```

Most important optimizations are performed on lowered SSA values:

* [Operations strength reduction](https://en.wikipedia.org/wiki/Strength_reduction)
* [Addressing modes utilization](https://en.wikipedia.org/wiki/Addressing_mode)
* Runtime function calls specialization (like in [CL121697](https://go-review.googlesource.com/c/go/+/121697))
* And many more

All operations have mnemonical name, which we call "opcode". Opcodes of target-dependent operations usually reflect native intructions mnemonics.

## Rules language syntax

Grammar is described in [rulegen.go](https://github.com/golang/go/blob/master/src/cmd/compile/internal/ssa/gen/rulegen.go):
```go
// rule syntax:
//    sexpr [&& extra conditions] -> [@block] sexpr
//
// sexpr are s-expressions (lisp-like parenthesized groupings)
// sexpr ::= [variable:](opcode sexpr*)
//         | variable
//         | <type>
//         | [auxint]
//         | {aux}
//
// aux      ::= variable | {code}
// type     ::= variable | {code}
// variable ::= some token
// opcode   ::= one of the opcodes from the *Ops.go files
```

Worth mentioning that you can use `//`-style comments inside `.Rules` files as well.

Lets examine simple example which contains all these elements:
```lisp
   Opcode=ADDLconst - evaluates sum of an argument with a 32-bit constant
     :    AuxInt=c - constant that is being added to the `x`
     :      :
(ADDLconst [c] x) && int32(c)==0 -> x
|              /  |           /     |
|             /   |          /      |
|            /    |         /       Replacement form
|           /     Replacement condition (more conditions can be chained with `&&`)
Form that we are matching (and want to replace)
```

Rule defined above turns `x+0` into just `x`. Everything inside conditions is an ordinary Go code with obvious restriction of boolean-only expressions. You may call predicates declared in [rewrite.go](https://github.com/golang/go/blob/master/src/cmd/compile/internal/ssa/rewrite.go) and add your own.

You can also use `|` alternation-like syntax to generate several forms from one pattern:
```lisp
(ADD(Q|L)const [off] x:(SP)) -> (LEA(Q|L) [off] x)
//; Removing Q|L alternation:
(ADDQconst [off] x:(SP)) -> (LEAQ [off] x)
(ADDLconst [off] x:(SP)) -> (LEAL [off] x)
//; Removing `x` binding:
(ADDQconst [off] (SP)) -> (LEAQ [off] (SP))
(ADDLconst [off] (SP)) -> (LEAL [off] (SP))
```

> `(SP)` is generic operation that expresses stack pointer load. For architectures that do not have hardware stack support and/or `SP` register, it must be emulated.

Notable properties of pattern variables:

* Variables like `x` that do not have `:`-binding captures anything
* Like normal variables, `_` captures anything, but the result can be ignored

```lisp
//; Both rules do exactly the same thing: they implement ADDQconst identity function.
//; In other words, they returns their matched form unchanged.
(ADDQconst _) -> v
(ADDQconst x) -> (ADDQconst x)
```

If `AuxInt` is not specified explicitly (expression inside square brackets), then pattern will match any `AuxInt` value. Same things apply to the `{}`-parameters (more on that below).

`v` variable is automatically bound to the outmost pattern match.<br>
For example, `(ADDQconst (SUBQconst x))` has `ADDQconst` bound to `v`.

If same variable is used more than once inside pattern, it will require matching of a multiple S-expression parts between them:
```lisp
(ADDQconst [v] (ADDQconst [v] x))
//; Will match "x+2+2" (x+v+v).
```

## Types inside rules

Sometimes it is required to specify form type explicitly. Type is specified inside angle brackets `<T>`, like template parameters in C++:

```lisp
//; typ.UInt32 - BTSLconst operation type.
//; BSFL has fixed type of `typ.UInt32`, so it doesn't
//; need explicit type specification.
(Ctz16 x) -> (BSFL (BTSLconst <typ.UInt32> [16] x))
```

In addition to types, there are also "symbols" (or, more generally, `Aux` properties).
```lisp
(StaticCall [argsWidth] {target} mem) -> (CALLstatic [argsWidth] {target} mem)
```

* `[argsWidth]` - `Value.AuxInt`. For `StaticCall` - total arguments size
* `{target}` - `Value.Aux`. For `StaticCall` - function that is being called
* `<typ.UInt32>` - `Value.Type`. Result value type

`Aux` and `AuxInt` fields semantics vary from one opcode to another. It's better to consult associated `*Ops.go` files to see how these fields should be interpreted. Every `opData` that holds `Aux` and/or `AuxInt` sets appropriate `opData.aux` field that describes auxilary value purpose.

All types are coming from [cmd/compile/internal/types](https://github.com/golang/go/tree/master/src/cmd/compile/internal/types) package. Some types are SSA-specific, like `types.TypeFlags` while the others are shared between `cmd/compile/internal/gc` и `cmd/compile/internal/ssa`.

## Special SSA types

`types.TypeMem` is a special kind of value type which serves multiple purposes:

1. It makes it possible to order and group `ssa.Value` objects by their memory access patterns. In particular, this gives us a way to enforce proper order of evaluation inside basic block (more on that later).
2. It defines a memory flow inside SSA program. If instruction modifies memory, new SSA value of type `types.TypeMem` is created and returned as a result of such operation.

Just like `OpPhi` is a very special opcode that is treated differently in different passes, `types.TypeMem` is treated with additional care in many passes.

<details>
<summary>More on Phi</summary>

`Phi` has different roles that vary from pass to pass.

In the earlier phases of SSA backend, it serves classical role and the program itself is valid SSA. It expresses value selection depending on the execution path that reached it.

For example, if there are two ways to enter a block, and both of them modify memory, then destination block with get memory of `(Phi mem1 mem2)`. Loops also result in a `Phi` operation.
</details>

Another special type is `types.TypeFlags`. It describes [CPU flags](https://en.wikipedia.org/wiki/FLAGS_register) generation.

Instructions like `ADDQ` do not have `types.TypeFlags` type even though they do produce flags. They're only marked with `clobberFlags` attribute.

`types.Flags` is used for instructions that do not write result to any of its explicit arguments, like `CMPQ`, that reads both operands and updates CPU state accordingly by setting appropriate flags that can be used by instruction that follows it.

Instructions like `SETL` are used to "read" flags and return them as `ssa.Value` that can be assigned to a register.

```lisp
 L-less than               G-greater than
 |                         |
(SETL (InvertFlags x)) -> (SETG x)
                   |
                   Form that produces flags
```

## SSA program inspection

Given this Go program (`example.go`):

```go
package example

func fusedMulAdd(a, b, c float64) float64 {
	return a*c + b
}
```

We can inspect SSA generated for `fusedMulAdd`:
```bash
$ GOSSAFUNC=fusedMulAdd go tool compile example.go > ssa.txt
```

Checkout you working (current) directory:

* `ssa.txt` contains textual SSA dump.
* `ssa.html` is generated automatically and contains same information as `ssa.txt`, but in more human-readable and interactive format. Try opening it in your browser.

<details>
<summary>fusedMulAdd machine code</summary>

`~r3` renamed to `ret` for clarity.

```asm
v7  (4) MOVSD a(SP), X0
v11 (4) MOVSD c+16(SP), X1
v12 (4) MULSD X1, X0
v6  (4) MOVSD b+8(SP), X1
v13 (4) ADDSD X1, X0
v15 (4) MOVSD X0, ret+24(SP)
b1  (4) RET
```

</details>

This is how SSA for the `fusedMulAdd` looks after the `lower` pass (from ssa.html):

![](https://habrastorage.org/webt/zs/yb/ax/zsybaxjcr1s0_u1csmhgkt3d1za.png)

<details>
<summary>Textual SSA format</summary>

If you want to copy that for whatever reason:

```
lower [77667 ns]
b1:
    v1 (?) = InitMem <mem>
    v2 (?) = SP <uintptr>
    v7 (?) = LEAQ <*float64> {~r3} v2
    v8 (3) = Arg <float64> {a}
    v9 (3) = Arg <float64> {b}
    v10 (3) = Arg <float64> {c}
    v12 (+4) = MULSD <float64> v8 v10
    v13 (4) = ADDSD <float64> v12 v9
    v14 (4) = VarDef <mem> {~r3} v1
    v15 (4) = MOVSDstore <mem> {~r3} v2 v13 v14
Ret v15 (line +4)
```

</details>

We can translate that to S-expressions:

```lisp
(MOVQstore {~r3} 
           (SP)
           (ADDSD (MULSD (Arg {a})
                         (Arg {c}))
                  (Arg {b})))
```

<details>
<summary>SSA after regalloc pass</summary>

![](https://habrastorage.org/webt/ef/kx/u5/efkxu5pdwqvs14c9xwblajxoywo.png)

```
regalloc [87237 ns]
b1:
    v1 (?) = InitMem <mem>
    v14 (4) = VarDef <mem> {~r3} v1
    v2 (?) = SP <uintptr> : SP
    v8 (3) = Arg <float64> {a} : a[float64]
    v9 (3) = Arg <float64> {b} : b[float64]
    v10 (3) = Arg <float64> {c} : c[float64]
    v7 (4) = LoadReg <float64> v8 : X0
    v11 (4) = LoadReg <float64> v10 : X1
    v12 (+4) = MULSD <float64> v7 v11 : X0
    v6 (4) = LoadReg <float64> v9 : X1
    v13 (4) = ADDSD <float64> v12 v6 : X0
    v15 (4) = MOVSDstore <mem> {~r3} v2 v13 v14
Ret v15 (line +4)
```

</details>

## Defining new optimization rules

Processors that have [FMA](https://en.wikipedia.org/wiki/FMA_instruction_set) can evaluate `a*c + b` in a single instruction as opposed to 2 `MULSD`+`ADDSD`.

We'll take [Ilya Tocar](https://github.com/TocarIP) [CL117295](https://go-review.googlesource.com/c/go/+/117295) as a foundation for our experiment.

For your convenience, I've prepared minimal `diff` patch:<br>
https://gist.github.com/Quasilyte/0d4dbb0f8311f38d00a7b2d25dcec704.

**1. Adding new opcode - FMASD**

Find `AMD64ops` slice variable inside `compile/internal/ssa/gen/AMD64Ops.go` and add new element to it (position does not matter):


```go
{ // fp64 fma
  name: "FMASD",      // SSA opcode
  argLength: 3,
  reg: fp31,          // Info required for regalloc, regs inputs/outputs mask
  resultInArg0: true, // Annotate first argument as both source and destination
  asm: "VFMADD231SD", // x86 asm opcode
},
```

There was no `(fp, fp, fp -> fp)` operations before, so we need to add new registers specifier in the same file:

```diff
  fp01     = regInfo{inputs: nil, outputs: fponly}
  fp21     = regInfo{inputs: []regMask{fp, fp}, outputs: fponly}
+ fp31     = regInfo{inputs: []regMask{fp, fp, fp}, outputs: fponly}
```

**2. Adding rewrite rule**

```lisp
(ADDSD (MULSD x y) z) -> (FMASD z x y)
```

Better implementation would not be unconditional. It would check for FMA availability before applying the rule. We'll be treating every target AMD64 as FMA-enabled for now.

Compiler check can be implemented by using `config` like this:
```lisp
//; If config.useFMA is false, rule rewrite won't happen.
(ADDSD (MULSD x y) z) && config.useFMA-> (FMASD z x y)
```

<details>
<summary>How to check FMA availability on the machine?</summary>

If `lscpu` available, then this should suffice:
```bash
$ lscpu | grep fma
```

</details>

**3. Codegen implementation**

Now we need to add `FMASD` code generation into `ssaGenValue` function defined in `compile/internal/amd64/ssa.go`:

```go
func ssaGenValue(s *gc.SSAGenState, v *ssa.Value) {
  switch v.Op {
  case ssa.OpAMD64FMASD:
    p := s.Prog(v.Op.Asm()) // Creating new obj.Prog inside current block
    // From: first source operand.
    p.From = obj.Addr{Type: obj.TYPE_REG, Reg: v.Args[2].Reg()}
    // To: destination operand.
    // v.Reg() returns register that is allocated for FMASD result.
    p.To = obj.Addr{Type: obj.TYPE_REG, Reg: v.Reg()}
    // From3: second source operand.
    // From3 name is historical. In fact, SetFrom3 call sets
    // RestArgs field that can contain a slice of all but first src operands.
    p.SetFrom3(obj.Addr{
      Type: obj.TYPE_REG,
      Reg: v.Args[1].Reg(),
    })
    if v.Reg() != v.Args[0].Reg() { // Validate resultInArg0 invariant
      s := v.LongString()
      v.Fatalf("input[0] and output not in same register %s", s)
    }

  // Rest of the code remains unchanged. We're only adding 1 new case clause.
  }
}
```

Now everything is set and we can try out our new optimization. It's very rare occasion when you add new opcodes to the SSA backends. Most of the time new optimizations use already existing operations. We introduced new opcode for educational reasons.

## Checking out the results

First step is to re-generate rules-related Go code from `gen/AMD64Ops.go` и `gen/AMD64.Rules`.

```bash
# If GOROOT is unset, cd to the directory that is printed by `go env GOROOT`.
cd $GOROOT/src/cmd/compile/internal/ssa/gen && go run *.go
```

Now we need to build our new compiler:
```bash
go install cmd/compile
```

After `example.go` compiletion, we get different machine code output:

```diff
- v7  (4) MOVSD a(SP), X0
- v11 (4) MOVSD c+16(SP), X1
- v12 (4) MULSD X1, X0
- v6  (4) MOVSD b+8(SP), X1
- v13 (4) ADDSD X1, X0
- v15 (4) MOVSD X0, ret+24(SP)
- b1  (4) RET
+ v12 (4) MOVSD b+8(SP), X0
+ v7  (4) MOVSD a(SP), X1
+ v11 (4) MOVSD c+16(SP), X2
+ v13 (4) VFMADD231SD X2, X1, X0
+ v15 (4) MOVSD  X0, ret+24(SP)
+ b1  (4) RET
```

## Basic blocks

It's now time to discuss [basic blocks](https://en.wikipedia.org/wiki/Basic_block) of Go SSA.

`ssa.Values` that we were optimizing above are contained inside blocks (`ssa.Block`), blocks themselves are contained inside function.

Like SSA values, there are two kinds of blocks: abstract and arch-dependent. All blocks have exactly one entry point and 0-2 destination blocks (depends on the block kind).

`If`, `Exit` and `Plain` are the simplest blocks out there:

* `Exit` block has 0 destinations. It describes leaf blocks that perform non-local jumps (usually via `panic`)
* `Plain` block has 1 destination. Can be viewed as unconditional jump that is performed after all block values evaluation
* `If` block has 2 destinations. `Block.Control` boolean expression controls which path is chosen

Here are simple examples of blocks lowering for `AMD64`:
```lisp
                "then" body (block itself)
                |   "else" body (block itself)
                |   |
(If (SETL  cmp) yes no) -> (LT cmp yes no)
(If (SETLE cmp) yes no) -> (LE cmp yes no)
```

We'll cover blocks in a more details in a context of other SSA optimization passes.

## Optimization rules limitations

SSA backend has its advantages. Some optimizations can be performed in `O(1)` time thanks to it. But there are also some drawbacks that mostly come from the way Go compiler implements SSA and how initial SSA form is generated.

Lets imagine that you want to [combine append calls](https://go-critic.github.io/overview#appendCombine-ref):

```go
xs = append(xs, 'a')
xs = append(xs, 'b')
// =>
xs = append(xs, 'a', 'b')
```

When SSA is generated, high-level code structure is lost and `append` will appear as a set of values injected into the code instead of actuall `append` call (this is because append is a special builtin function and is always inlined). You would need to write a huge form that matches all these values produced by `gc` compiler.

Speaking of `.Rules`, there are some inconveniences when working with blocks. Any non-trivial optimization that needs blocks manipulation can't be expressed by using Lisp-like DSL. Partial block update is impossible, removing blocks is also impossible (but there is hack like `First` block that is used for dead code removal).

> Even if these shortcomings are ever addressed, it may not be a very good idea to migrate on SSA IR completely, rendering more high-level representation away. Most production-grade compilers have more than one IR. There is no "one best IR" as far as I know.

## Go that goes faster

If you have some cool optimization idea, try it out and send it to the [go-review.googlesource.com](https://go-review.googlesource.com). I'll gladly review such a patch (use `iskander.sharipov@intel.com` uid when doing CC).

Happy compiler hacking!

![](https://habrastorage.org/webt/op/wr/50/opwr50a-n6imbex_ykcxpia5_ny.gif)

**Bonus materials**

Some good examples of Go patches that added or changed SSA rules:

* [CL99656: cmd/compile/internal/ssa: emit IMUL3{L/Q} for MUL{L/Q}const on x86](https://go-review.googlesource.com/c/go/+/99656)
* [CL102277: cmd/compile/internal/ssa: optimize away double NEG on amd64](https://go-review.googlesource.com/c/go/+/102277)
* [CL54410: cmd/compile/internal/ssa: use sse to zero on amd64](https://go-review.googlesource.com/c/go/+/54410)
* [CL58090: cmd/compile/internal/ssa: remove redundant zeroextensions on amd64](https://go-review.googlesource.com/c/go/+/58090)
* [CL95475: cmd/compile/internal/ssa: combine byte stores on amd64](https://go-review.googlesource.com/c/go/+/95475)
* [CL97175: cmd/compile/internal/ssa: combine consecutive LE stores on arm64](https://go-review.googlesource.com/c/go/+/97175)
* [CL115617: cmd/compile/internal/ssa: remove useless zero extension](https://go-review.googlesource.com/c/go/+/115617)
* [CL101275: cmd/compile: add amd64 LEAL{1,2,4,8} ops](https://go-review.googlesource.com/c/go/+/101275)

Not so long ago, [README](https://github.com/golang/go/blob/master/src/cmd/compile/internal/ssa/README.md) document was added to the ssa package. Recommending to read it (as it will eventually only get better).
