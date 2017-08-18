+++
date = "2017-08-18"
title = "Path to convenient C FFI in Go"
tags = [
    "[go]",
    "[cgo]",
    "[ffi]",
    "[reflection]",
]
description = "Almost useful and flexible C FFI for Go."
draft = false
+++

## DWIM-style FFI

[CGo](https://golang.org/cmd/cgo/) is a widely adopted way of calling C functions from Go:
low level [C FFI](https://en.wikipedia.org/wiki/Foreign_function_interface) which
does many things behind the scene, but exposes only minimal functionality
that is required to interact with C.

CGo does no implicit conversions or data copying during C functions call,
even `int` and `C.int` are not compatible.
Code that uses this mechanism without wrappers ([bindings](https://en.wikipedia.org/wiki/Language_binding)) will be polluted with
explicit slice/array copies and type conversions.

That is perfectly fine for default behavior and FFI foundation,
but sometimes we do not require this amount of control.
In this cases, we want the programming environment to [do what we mean](https://en.wikipedia.org/wiki/DWIM).

Imagine we have this C code:

```c
// Let's assume this function is very important.
int sum(int *xs, int xs_len) {
    int ret = 0;
    for (int i = 0; i < xs_len; ++i) {
        ret += xs[i];
    }
    return ret;
}
```

And you wish to call it from Go.
There is `[]int` of valuable payload which must be
aggregated with `sum` function.

```go
xs := []int{1, 2, 3} // Payload
// a. This is how you may want to call that function:
sum := cffi.Func(C.sum)
sum(xs)
// b. This is how you actually can call that function:
ys := append([]int{}, xs...) // Make a copy (for safety)
C.sum(unsafe.Pointer(&ys[0]), C.int(len(ys)))
```

This article aims tries to reach **a**-like API,
as close as possible. 

> Note that it is not always a desired behavior to
> pass slice as 2 separate {data, len} arguments,
> but I have selected this strategy to show something
> worthwhile in the final section of this post.

## Universal {Go}->{C} value mapping

If type mapping is the most boilerplate-full part, let's
write a simple library that does it for us.

Almost all primitive types have obvious C counterparts.
For other types we can define conversion rules and apply
them consistently. 

For starters, `Go2C` function should handle 1 type: integers.
It takes arbitrary Go type as `interface{}` and
returns inferred C type boxed into `interface{}`.

```go
package cffi
import "C"
func Go2C(x interface{}) interface{} {
    switch x := x.(type) {
    case int:
        return C.int(x)
    default:
        panic("todo: implement more types")
    }
}
```

So long it looks fine.
Try to use it via client package and you may be surprised.

```go
package main
import "C"
import "cffi"
func main() {
    x := int(10)      // Clearly, an int
    y := cffi.Go2C(x) // Dynamic type=C.int
    z := y.(C.int)    // Panics!
    println(z)
}
```

Exact error message may vary, but it reads like: "panic: interface 
conversion: interface {} is cffi._Ctype_int, not main._Ctype_int".

This is a [known issue](https://github.com/golang/go/issues/13467).
Practically speaking, we can not implement `Go2C` this way properly.

This also makes it impossible to match result types and
do reversal, `C2Go` mapping.

> The only way to define a converter function is to
> delegate that task to the client code.
> Package that imports "C" does implement the conversion rules.

## CGo function metadata

To define function like `func Call(fn <?>, args ...<?>) <?>` we need
to have signature info of `fn` argument.

How much information is provided by CGo?
What kind of value `C.<X>` yields, given that `<X>` is a function?

```go
package main
// int foo(void) { return 0; }
import "C"
import "fmt"
func main() { fmt.Printf("%T\n", C.foo) }
```

The answer is `unsafe.Pointer`.
Well, this is bad for two reasons:

1. We can not wrap it into `reflect.Value`;
2. `unsafe.Pointer` gives zero type information;

More experiments will reveal a cheesy way to get what we need. 

**Step1**: discover CGo name mangling scheme.

```go
package main
// void foo(void) {}
import "C"
func main() { C.foo(1, 2) }
```

Go will kindly reply that you called function with wrong number
of arguments: 

```text
main.go:6: too many arguments in call to _Cfunc_foo
	have (number, number)
	want ()
```

See that `_Cfunc_foo`? I think you get the pattern.

**Step2**: examine mangled symbol directly.

```go
package main
// void foo(void) {}
import "C"
import "fmt"
func main() { fmt.Printf("%#v\n", _Cfunc_foo) }
```

Go rejects your code: `main.go:5: undefined: _Cfunc_foo`.
This is easy to fix.

**Step3**: figure out a fix to error above.

```go
package main
// void foo(void) {}
import "C"
import "fmt"
func main() { 
    _ = C.foo() // Use "foo"
    fmt.Printf("%#v\n", _Cfunc_foo) 
}
```

This snippet actually gets us closer to the solution.
Expression `_Cfunc_foo` is not equivalent to `C.foo` as it
gives us `func() main._Ctype_void` type.

## Implementation overview

Implementation requirements:

1. Function is callable via its symbol, like `f()`, where `f` is a symbol;
2. Ingoing and outgoing arguments are automatically converted;

First requirement can be fulfilled only by global (possibly dot-imported) function
or closure variable. 
Second requirement, due to restrictions outlined above, is possible
with a help of external state. This state can be global or captured (with closures).

```go
package main
// int add1(int x) { return x + 1; }
import "C"
import "cffi"
var add1 cffi.Func
func init() {
    _ = C.add1(0) // [I]

    invoker := cffi.NewInvoker( // [II]
        // Go -> C
        func(x interface{}) interface{} {
            return C.int(x.(int))
        },
        // C -> Go
        func (x interface{}) interface{} {
            return int(x.(C.int))
        }
    )

    add1 = cffi.Wrap(invoker, _Cfunc_add1) // [III]
}

func main() {
    println(add1(50)) // [IV]
}
```

**(I)** is needed if `add1` is never called via `C.add1` symbol.
We will not get `_Cfunc_add1` without it.

**(II)** invoker instance should take care of values conversions
and universal call evaluation. 

**(III)** actual function pointer is wrapped into a closure
that holds invoker.

**(IV)** prepared closure can be used in a desired way.

Invoker can handle 1->N value mapping.
For example, it can be legal to return `[]interface{}` for
Go values that should be unwrapped into 2 C function arguments.
Slices are such example (we ignore `cap` on purpose).

```go
func (x interface{}) interface{} {
    switch x := x.(type) {
    case []int:
        y := make([]C.int, len(x))
        for i := range x {
            y[i] = C.int(x[i])
        }
        ptr := (*C.int)(unsafe.Pointer(&y[0]))
        return []interface{}{ptr, C.int(len(x))}

    // ... handle other types
    }
}
```

If you want to see implementation sources, inspect [cffi](https://github.com/Quasilyte/cffi) library.

## Warnings and closing notes 

**Performance**.

`cffi` library solution involves many extra overhead compared to
simple CGo call, which itself is far more expensive than normal Go
function call.

**Portability**.

Using CGo alone can hurt application portability.
Granted, using CGo oddities like name mangled function
objects slaughters program portability completely.
It may break with newer Go1 releases.