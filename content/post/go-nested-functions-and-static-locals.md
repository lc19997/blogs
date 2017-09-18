+++
date = "2017-09-18"
title = "Go nested functions and static locals"
tags = [
    "[go]",
    "[closure]",
]
description = "Avoid global lookup tables. Making things as local (private) as possible."
draft = false
+++

## Symbol visibility

Default symbol visibility should be as narrow as possible.
This means that you use globals with internal linkage instead of external,
local variables rather than globals, hide class-related constants inside
it's scope, and so on. 

If function is only called inside particular function,
it should become a [nested function](https://en.wikipedia.org/wiki/Nested_function).

Most of these rely on the language support.

Go has quite simple model of scopes and symbol visibility.
User-defined identifier can be local or package-local (global).
Package level identifiers can be exported or unexported.

- No `static` storage class for local variables.
- Function declarations can only appear at top level. No nested functions.

As a consequence, you end up using globals for lookup tables,
compiled regular expressions and other objects that should
be initialized once, and then used during every function call.

Why such encapsulation matters is not a topic of this post.
Instead, this article is focused on the working technique overview.
Benchmarks and quirks list are included.

## Closures and immediately-invoked function expressions

Go permits top level dynamic initialization.
We are interested in [IIFE](https://en.wikipedia.org/wiki/Immediately-invoked_function_expression)
in combination with closures.

Suppose someone developed `describeString` function listed below.
It is not overly complex, but in order to define it,
programmer also introduced `hasVowel` helper function,
which requires `vowels` global variable.
`describeString` has to check a string against
regular expression, so it was assigned to `rxGolang`,
this removes a need to compile regexp during each function call.

```go
var vowels = map[rune]bool{
    'a': true, 'e': true, 'i': true,
    'o': true, 'u': true, 'y': true,
}
var rxGolang = regexp.MustCompile(`[Gg]o|[Gg]golang`)
func hasVowel(s string) bool {
    for _, c := range s {
        if vowels[c] {
            return true
        }
    }
    return false
}
func describeString(s string) string {
    var attrs []string
    if hasVowel(s) {
        attrs = append(attrs, "has vowel letter")
    }
    if rxGolang.MatchString(s) {
        attrs = append(attrs, "may be about Go language")
    }
    attrs = append(attrs, fmt.Sprintf("has length of %d", len(s)))
    return strings.Join(attrs, "; ")
}
```

So far, **4** global symbols for single function.
With closures and IIFE we can reduce this number to **1**.

```go
var describeString = func() func(string) string {
    vowels := map[rune]bool{
        'a': true, 'e': true, 'i': true,
        'o': true, 'u': true, 'y': true,
    }
    rxGolang := regexp.MustCompile(`[Gg]o|[Gg]golang`)
    hasVowel := func(s string) bool {
        for _, c := range s {
            if vowels[c] {
                return true
            }
        }
        return false
    }

    return func(s string) string {
        var attrs []string
        if hasVowel(s) {
            attrs = append(attrs, "has vowel letter")
        }
        if rxGolang.MatchString(s) {
            attrs = append(attrs, "may be about Go language")
        }
        attrs = append(attrs, fmt.Sprintf("has length of %d", len(s)))
        return strings.Join(attrs, "; ")
    }
}() // <- Note this.
```

Note that inner closure body is identical to initial `describeString` implementation.
The rest of this post describes provided solution characteristics.

## Performance

As you may guess there are some performance penalties.

Two main differences between normal function and closure-based approaches:

1. Initialization time. IIFE will be evaluated during package initialization, at run-time.
2. Function call overhead. IIFE closure calls are never inlined.

The exact numbers are hard to predict, but you may expect about **1-5%** slowdown.
This may be important if your application is very performance-critical *and* that
function is called inside a tight loop.

You can use [linked benchmark](/blog/code/closure_test.go) to have an approximation.
Example results are provided in the next snippet.

```text
$ go test -bench=.
BenchmarkNormalFunc-4   	   20000	     90200 ns/op
BenchmarkClosure-4      	   20000	     94576 ns/op

$ benchstat func.txt closure.txt 
name          old time/op  new time/op  delta
NormalFunc-4  88.6µs ± 1%  89.3µs ± 2%  +0.85%  (p=0.015 n=10+10)
```

## Potential problems

This article would be incomplete without a list of known problems with
proposed solution.

> Problem 1 - no parameter names hint.

With normal function call hint may look like `func(s string) string`,
while our closure will get `func(string) string`. 

You can fix that with simple change.

```diff
 
-var describeString = func() func(string) string {
+var describeString = func() func(name string) string {
        vowels := map[rune]bool{
                'a': true, 'e': true, 'i': true,
                'o': true, 'u': true, 'y': true,
```

Hovewer, this will force you to break DRY principle, albeit slightly.
The main disadvantage that you have to change parameter names in two
places instead of one. 

From the other point of view it is an additional flexibility, 
because you can use longer, expressive names for the "public" parameters and
shorter identifier for the implementation itself.

> Problem 2 - function variable is mutable.

For unexported functions this is not a problem,
but if symbol is exported, users may re-assign variable for
something else. They do not have this opportunity with
functions that are defined in a normal way.

## Conclusion

Closure-based encapsulation is an old trick.
JavaScript programmers use it along with IIFE all the time.

If you have a question: "do I have to?",
the answer is "no" of course.
But when you seek for additional patterns to reduce code
complexity, this solution may prove useful.
