+++
date = "2017-05-19"
title = "Emacs Lisp multi return values"
tags = [
    "[emacs lisp]",
    "[Go.el project]",
    "[performance]",
]
description = "Efficient Go style multi return values in Emacs Lisp."
draft = false
+++

## The missing feature

Did you ever wrote a function in **Emacs Lisp** which should return
more than one result? 

Emacs Lisp has no native support for multiple return values,
but provides `cl-lib` that emulates it in a **Common Lisp** style.

In this article I will show that `cl-values` is suboptimal and
can be replaced without any sacrifices to the convenience.

## Naive solution

`cl-lib` implements `cl-values` in terms of `list`.
This approach is naive because each time you return with that, 
an allocation is involved. GC will trigger more frequently
and perfomance will degrade.

```lisp
(let ((lexical-binding t))
  (benchmark-run-compiled 1000000
    (cl-multiple-value-bind (a b c) (cl-values 1 2 3)
    (ignore a b c)))) ;; => (0.8493319750000001 59 0.7827748330000008)
;; ...more than 50 garbage collections,
```

We see the bottleneck now: proposed solutions should
avoid memory allocations.  
In other words: no `list`, `cons`, `vector`, ...

## No allocations with preallocations

We can still use lists and vectors if preallocation is done.
Multiple return values are mostly consist of 2-4 elements =>
the set of required containers is fixed and known beforehand.

```lisp
(defvar mv--2 (make-vector 2 nil))
(defvar mv--3 (make-vector 3 nil))
;; ... as many as we need.
```

When 2 value tuple must be returned, `mv--2` vector 
is populated with corresponding values.
For 3 value tuple, `mv--3` is used. 
Filled vector is returned to the caller.
Special macro can be used to extract vector elements 
into specified bindings.

This brings us close to the `cl-lib`, but without allocations.

Emacs Lisp has no real multithreading, so it is safe to
store results inside private global variable. 

## List vs vector

The choice between `vector` and `list` is not easy, 
especially if you know Emacs bytecode.

First operation we care about is **return efficiency**.
To make multi value return, preallocated list/vector 
must be filled with data.

```text
                          f(x, y) = x, y+1

(defvar mv--2 '(nil . nil))           (defvar mv--2 [nil nil])
constants=[mv--2] maxStack=5          constants=[mv--2 0 1] maxStack=6
                                    |
add1        <x y+1>                 | add1        <x y+1>
varref 0    <x y+1 mv--2>           | varref 0    <x y+1 mv--2>
dup         <x y+1 mv--2 mv--2>     | dup         <x y+1 mv--2 mv--2>
stack-ref 3 <x y+1 mv--2 mv--2 x>   + constant 1  <x y+1 mv--2 mv--2 0>
setcar      <x y+1 mv--2>           + stack-ref 4 <x y+1 mv--2 mv--2 0 x>
dup         <x y+1 mv--2 mv--2>     + aset        <x y+1 mv--2>
stack-ref 2 <x y+1 mv--2 mv--2 y+1> + dup         <x y+1 mv--2 mv--2>
setcdr      <x y+1 mv--2>           + constant 2  <x y+1 mv--2 mv--2 1>
ret                                 + stack-ref 3 <x y+1 mv--2 mv--2 1 y+1>
                                    + aset        <x y+1 mv--2>
                                    | ret
```

Left block shows list implementation. Right block is for vector.

As you may see, for `N=2` case cons cell is better than vector in many ways:

* Bytecode is shorter
* Less stack space is used
* Smaller constant vector (no need for indexes)

Second operation is **return value receive**.

```text
                          let x, y = f(...)

call ...    <mv--ret2>            | call ...    <mv--ret2>
dup         <mv--ret2 mv--ret2>   | dup         <mv--ret2 mv--ret2>
car         <mv--ret2 x>          + constant X  <mv--ret2 mv--ret2 0>
stack-ref 1 <mv--ret2 x mv--ret2> + aget        <mv--ret2 x>
cdr         <mv--ret2 x y>        + stack-ref 1 <mv--ret2 x mv--ret2>
                                  + constant Y  <mv--ret2 x mv--ret2 1>
                                  + aget        <mv--ret2 x y>
```

What about 3 or more return values? 
General algorithm for lists is:

1. For `N` return values use dedicated preallocated list
2. First value bound with `setcar`
3. Last value bound with `setcdr` 
4. Values in between set with `setcar` AND perform `cdr`

Note that used list is not **proper list**. The last `cdr` is not `nil`.

At `N=3` vector and list are nealy equal in efficiency, `N=4` favors vectors.
List becomes less and less efficient as the `N` grows.

In my experience 2-value returns cover 90% of cases.
This means that list is a winner here.

## Not list, nor vector?

It is possible to avoid lists and vectors completely.

For each **additional** return value it is possible to use
single global variable. 

First value is returned as usual, while others 
use `varset` (setq) to bind additional data.
On the caller side, function result is bound to
the first variable; other variables read from
corresponding global variables.

```lisp
;; Return "a", "b", "c":
(progn 
  (setq mv--3 "c")
  (setq mv--2 "b")
  "a")
;; Bind results:
(let ((x1 (f ...)
      (x2 mv--2)
      (x3 mv--3)))
  ...)
```

This gives us very compact bytecode. Perfomance
depends on many factors, but it can
match implementation based on preallocated lists.

Let's use this idea to create `mv-lib`.

## mv-lib

The minimal `mv-lib` should consist of at least two macros:

1. `mv-ret` - yield a multi value 
2. `mv-let` - bind multi value to local variables

Like with other solutions, predefined globals are required.
For simplicity, they have 0-based suffixes.
That is, second return value is stored inside `mv--0` (not in `mv--2`).

```lisp
(defconst mv--max-count 10) ;; Arbitrary limit

(defun mv--var (index)
  "Get return value variable symbol by INDEX"
  (when (>= index mv--max-count)
    (error "Index %d is too high (%d is max)" index (1- mv--max-count)))
  (intern (format "mv--%d" index)))

(dotimes (i mv--max-count)
  (eval `(defvar ,(mv--var i) nil)))
```

`mv-ret` and `mv-let` are convenience wrappers for code that is
presented in previous section.

[Full mv-lib implementation](/blog/code/mv-lib.el). 

```lisp
(let ((lexical-binding t))
  (benchmark-run-compiled 1000000
    (mv-let (a b c) (mv-ret 1 2 3)
      (ignore a b c)))) ;; => (0.174552687 0 0.0)
;; 0 GC runs!
```

Multitple values return with zero allocations achieved.

## Why I prefer Go.el

Macro can help a lot with many features, but what about 
packages or namespaces? 

It is tedious and ugly to use prefixed identifiers for **everything**.
Even **C** has better modularity and encapsulation with 
internal linkage and opaque pointers.

Everyone understand complications that arise with modules 
for Emacs. Luckily, there is another way.
Some languages already have modules.
With [Go.el](https://github.com/Quasilyte/Go.el) it is possible
to write **Go** code that is translated into **Emacs Lisp**.

As a bonus, when **Go.el** will be complete, we could
use **Go** libraries inside Emacs. 
