+++
date = "2018-01-14"
title = "Goism objects layout model"
tags = [
    "[go]",
    "[emacs lisp]",
    "[goism project]",
]
description = "How Go objects represented inside Emacs."
draft = false
+++

## Introduction

[Goism](https://github.com/Quasilyte/goism) project requires Go pointers
emulation inside Emacs Lisp code.

This document describes how to achive (almost) full functionality with
potential to have optimizations that eliminate some of the
emulation-related overhead.

The actual implementation can diverge.
Only initial design is outlined.

## Struct representation

Go structures represented by **lists**.  
Empty struct run-time value is unspecified, but it satisfies Go spec requirements.

```go
type a1 struct { f1 int }
// a1{1}
// (list 1)

type a2 struct { f1, f2, f3, f4 int }
// a2{1, 2, 3, 4}
// (list 1 2 3 4)
```

For the very small (1-3 fields) objects lists are a better choice than vectors,
but generally, vectors are more memory-efficient and provide faster random access.

[Pointers](#pointers) section explains why lists were selected over the vectors
as default representation.

For unexported struct types, optimizer is permitted to use "best fit" data type
for run-time values.
This is the reason why it is important to forbid usage of unexported types inside
Emacs Lisp domain.

## Arrays, strings and slices

Arrays represented by Emacs Lisp **vectors**.

Strings are Emacs Lisp **unibyte strings**. Literals are UTF-8 encoded.  
Strings created by Go code considered immutable.  
Immutability is not enforced during the execution.

Slices are implemented by a struct of `length`, `capacity`, `offset` and `data` fields.
The purpose of first two fields is self-explanatory.
The `offset` is used during index calculations; needed for re-sliced slices.
`data` is the underlying vector. 
Field order described here is not mandatory.

```go
[3]int{1, 2, 3}       // [1 2 3]
[]int{1, 2, 3}        // (3 3 0 [1 2 3])
([3]int{1, 2, 3})[1:] // (2 2 1 [1 2 3])
"abc"                 // "abc"
"π"                   // "\317\200"
```

Arrays, strings and slices are reference types in Emacs Lisp.

## General boxing

**Box** term is used when referring to thin wrapper that exist to enable 
pointer-like semantics.

The boxed value required to support `car` and `setcar` operations.
`car` is for dereference, `setcar` is for writes/updates.

The main purpose of boxing is to implement arbitrary pointer indirection.  
We never care about `cdr` part; it's value is undefined on purpose.

```lisp
  *T (cons T.value ?)
 **T (cons (cons T.value ?) ?)
***T (cons (cons (cons T.value ?) ?) ?)
;; ... and so on
```

```go
x := new(int) // x = (cons 0 ?)
*x            // (car x)
*x = 10       // (setcar x 10)
```

## Pointers: reference types

For reference types, the single level of indirection is the object itself.  
This means that `*T` has the same run-time representation as `T`.

When object has non-pointer struct type, all assignments use `copy-sequence`.
Arrays are assigned via copying, too.

Higher order indirection uses [general boxing](#general-boxing).

```go
x1 := Point{x: 1, y: 2}  // x1 = (list 1 2)
x2 := x1                 // x2 = (copy-sequence x1)

y1 := &Point{x: 1, y: 2} // y1 = (list 1 2)
y2 := y1                 // y2 = y1
y3 := *y2                // y3 = (copy-sequence y2)

z := new(*Point)         // z = (cons (list 0 0) ?)
*z = y2                  // (setcar z y2)
```

Pointer to n-th struct member is it's `nthcdr`.  
This is why lists are default struct representation - it makes
member address operation possible (and allocation-free).

```go
pt := Point{1, 2} // pt = (list 1 2)
x := &pt.x        // x = (nthcdr 0 pt) = pt
y := &pt.y        // y = (nthcdr 1 pt) = (cdr pt)
*x = 10           // (setcar x 10)
*y = 20           // (setcar y 20)
// pt fields are updated as expected.
```

Because `cdr` pointer part is always ignored, `(x y z)` is a valid pointer for `x`.

## Pointers: non-reference types

In Emacs Lisp, there are non-reference types; they are not mutable.  
The solution to this is [inferior mutability](https://ricardomartins.cc/2016/06/08/interior-mutability).

We apply boxing for all such values when they are not part of the struct
or other container.
Temporary values that are used for stores are not boxed, too.

```go
x := 10   // Boxed as (list 10)
y := 10.5 // Boxed as (list 10.5)

pt.x = 777 // 777 is not boxed here
pt.y = y   // Unboxing is required: (car y)

passVal(x)  // Possibilities: (copy-sequence x), (car x) or just x
passPtr(&x) // Always boxed x; makes x optimizations impossible
```

The negative impact on performance is addressed by [escape analysis](#escape-analysis).

## Pointers: array/slice element address

Situation with arrays and slices is more complicated.

* Arrays and slices can be very large, which makes lists impractical;
* Speculative layout optimization which is used for structs is not applicable (see below);

Even if it is possible to determine that particular array never gives element address away,
turning it into "real vector" will not work as it becomes incompatible with unoptimized arrays
of the same static type.

Proposed solution:

* It is easy to return a pointer to reference type value. Permit this operation;
* Forbid taking element address of non-reference types;

This is a trade-off between performance and Go spec compliance.  
Enabling this feature without constraints will make arrays (and slices)
very inefficient.

```go
xs := [2]Point{}
x := &xs[1] // Valid.

ys := [2]int{}
y := &ys[1] // Invalid. Compile-time error.
```

## Escape analysis

If pointers never existed in Go, we could avoid many complications described above.

Escape analysis is performed as the last part of optimizations.
It's aim is to find data that is never used in a way that forces us to
generate less efficient code.

For example, if address operator is never applied to local non-reference type
variable, there is no need to box it.

Go structures that have particular layout can be optimized if
member field address never taken from any of it's instances.
This analysis can be sound for unexported types.

If needed, special annotation can select particular struct run-time
representation.  
Compiler will reject code that uses such types in
non-compatible ways.
This feature should only be used when particular layout is very important.

```go
//goism:repr=vector
type Foo struct {
    A, B int
    C Bar
}

// Instances of Foo are represented as Emacs Lisp vectors.
// It is compile-time error to take address of A and B fields.
// It is OK to take C field address.
```

Possible representations:

* `list` - nil-terminated cons pairs. Can take address of any field.
* `list*` - improper list. Like lists, but last element address only works for ref types.
* `vector` - same as for arrays. Can take address of any ref type field.
* `string` - all fields must fit into 16bit ints. Can't take field address.
* `bool-vector` - all fields must be booleans. Can't take field address.
* `atom` - unboxed value. Valid only for unit (single field) structs. Can't take address.

Some representations not only have restricted field address operation, but also
member types/count constraints. 

The upside is the benefits of particular data type.  
For example, strings are much cheaper to allocate, but a little slower at
random access, than vectors.  
Improper lists are only a slight improvement over proper lists, but add nearly
no additional restrictions. They also work like a charm for 2 field objects.
