+++
date = "2018-02-23"
title = "Goism compilation modes"
tags = [
    "[go]",
    "[emacs lisp]",
    "[goism project]",
]
description = "Attempt to optimize compiler for the most desired use patterns."
draft = true
+++

## Fast & slow builds

This whole article can be reduced to the simple statement: sometimes compilation
speed does not matter, but when it does, you can't make it "too fast".

The analysis of exact use cases may reveal opportunities that may cut
compile time significantly. This also helps to avoid bad decisions that hurt
user experience.

## User-oriented compilation

Optimizations only make sense when priorities are well-defined: you can't get
everything; there is something you give away in order to get properties that
are more valuable in your situation. 

The famous [speed-time tradeoff](https://en.wikipedia.org/wiki/Space%E2%80%93time_tradeoff) is a nice example, but the optimization term is also applicable to user experience improvements. This article is focused on this aspect of optimizations: designing an instrument that respects user workflows and it's dynamic nature.

Tuning the language toolchain towards usefulness requires specification of
the most common and desirable use patterns.

In the context of interactive development inside [GNU Emacs](https://www.gnu.org/software/emacs/), I can distinguish
three main use cases that require different trade-offs in compilation design:

1. [Single expression evaluation](#single-expression-evaluation) (like in interactive [REPL](https://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop))
2. [Script-like main package execution](#scripts)
3. [Library compilation](#libraries)
4. [Go "testing" mode](#go-testing-mode)

There could be more noble usage schemes, but they are escaping my mind at this moment.

## Single expression evaluation

From time to time we want to run specific expression and get evaluation results to gain confidence.

Suppose you don't remember exact `strings.Split` behavior for empty string input arguments.
Will `len(strings.Split("", "\n")` result in 0 or 1?

If there is no built-in support for this in your editor or IDE, 
you may find yourself visiting [Go playground](https://play.golang.org/)
(creating local `main` package is the same kind of solution).

My main complaints for this approach are:

1. Need to wrap code into `main`.
2. Need to print evaluation results.
3. Not very convenient to re-use evaluation results for further experiments.
4. Prior to [goimports](https://godoc.org/golang.org/x/tools/cmd/goimports) integration into playground, you also was responsible to include required imports (not the case anymore).

Here are main design considerations for solving this inside [goism](https://github.com/Quasilyte/goism):

* `goism-eval-string` function accepts Go code string to be executed and returns
  Emacs Lisp object that represents evaluation result of arbitrary type.
* The result is pushed into N-slot stack. Stack top is always last evaluation result value.
  User may bind those values to Emacs Lisp variables or refer to them via stack reference.
* Required imports are automatically resolved. User can specify additional lookup patterns
  to enable non-standard packages.
* The compilation should be blazingly fast. So fast that the user feels like it's instant.
  This may even justify [daemon](https://en.wikipedia.org/wiki/Daemon_(computing))+[IPC](https://en.wikipedia.org/wiki/Inter-process_communication) design to avoid extra overhead
  of process spawning per each request.

The `goism-eval-string` should be enough to implement `goism-eval-last-expr` and
`goism-eval-print-last-expr` interactive commands.

There is so much more to this.  
We may want to select a region, do `M-x eval-region` and replace some variables with
constant arguments, so the selection of `strings.Split(x, "\n")` can be executed
with `x` bound to `""`. This does not require significant foundation changes: proposed model 
can enable creation of such great features without any troubles.

## Scripts

Much of the script-like code is written in Emacs Lisp to do code generation
or source code transformations.

When goism is capable enough to load `go/*` packages that make it quite easy
to manipulate Go sources, it would be possible to write one-off, syntax-aware scripts
as a part of Go development workflow. The "compiled" nature of Go will feel
less restrictive inside Emacs.

Not to forget that executed code can affect Emacs global state.
This makes Go scripts suitable for data load tasks that may be hard using
only Emacs Lisp due to the lack of high-quality libraries.

TODO: think about scripts more deeply.

## Libraries

TODO: library development VS library deployment

## Go "testing" mode

TODO: does testing framework require specific features from goism?

TODO: should I describe compiled object format here? Or mention them at all?