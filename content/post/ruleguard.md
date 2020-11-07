+++
date = "Wed Jan  8 21:54:02 MSK 2020"
title = "ruleguard: dynamic inspection rules for Go"
tags = [
    "[go]",
    "[habr-translation]",
    "[static-analysis]",
]
description = "New CodeQL-Like Analyzer for Go."
draft = false
+++

<center>If you found a typo or a misspelling, please [file an issue](https://github.com/quasilyte/blog-src/issues/new) or send a PR that fixes it.</center>

![](https://habrastorage.org/webt/b5/p-/sq/b5p-sqgr-9b1e5mimtxaftmryau.png)

> Original (ru): https://habr.com/post/481696/.

This article introduces a new static analysis library (and CLI utility) [go-ruleguard](https://github.com/quasilyte/go-ruleguard). It's like a [gogrep](https://github.com/mvdan/gogrep) that is adapted for the use inside your [CI](https://en.wikipedia.org/wiki/Continuous_integration) pipeline.

You describe static analysis rules in terms of a special Go-like DSL. During the startup, `ruleguard` turns these definitions into a set of inspections to be executed.

As a bonus, we'll also talk about [go/analysis](https://godoc.org/golang.org/x/tools/go/analysis) and it's [predecessors](https://github.com/go-lintpack/lintpack).


# Static analysis extensibility

There is [a lot](https://github.com/golangci/awesome-go-linters) of Go linters, but only a few of them can be extended. Usually, you need to write a Go code that uses a special linter API to add new inspections for it.

Two main options are [Go plugins](https://golang.org/pkg/plugin/) and monolith. The monolith implies that all inspections (including your own) are available during the compilation.

[revive](https://github.com/mgechev/revive) follows the monolith path as you're expected to include the new checks into the linter core to integrate them. [go-critic](https://github.com/go-critic/go-critic) supports plugins model that makes it possible to build analyzer extensions independently from the go-critic code. Either way, in the end, you're going to work with [go/ast](https://golang.org/pkg/go/ast/) and [go/types](https://golang.org/pkg/go/types/) plus some linter plugin API to implement the inspection itself. Even the simplest checks can require a [significant amount of boilerplate code](https://github.com/mgechev/revive/blob/master/rule/call-to-gc.go).

[go/analysis](https://godoc.org/golang.org/x/tools/go/analysis) will simplify this picture since it provides a framework that can be used by the different static analysis tools, so we get rid of the custom linter API part at least partially. However, it doesn't simplify the inspection implementation itself.

<details><summary>About `go/loader` and `go/packages`</summary><blockquote>

When you're implementing a Go static analyzer, you want to inspect an AST or SSA form of a target Go program. Before you can do that, source code needs to be properly "loaded". Simply speaking, loading procedure includes [parsing](https://golang.org/pkg/go/parser/), type checking and [dependencies importing](https://golang.org/pkg/go/importer/).

Every linter has to do all these loading steps in order to perform any meaningful work. [loader](https://godoc.org/golang.org/x/tools/go/loader) package was one of the first attempts to make this process less tiresome. With `loader` it was possible to "load" all you need with a couple of function calls. `loader` became deprecated before it could stop being experimental. [packages](https://godoc.org/golang.org/x/tools/go/packages) was a new way to do source code loading: it has an improved API and works with Go modules.

Some time ago I created [lintpack](https://github.com/go-lintpack/lintpack) - a linters framework that was used inside [go-critic](https://github.com/go-critic/go-critic). It could build a linter executable that includes different inspections providers. Now it's deprecated as well because we have [analysis](https://godoc.org/golang.org/x/tools/go/analysis) framework from the Go team.

Nowadays you're encouraged to use the above-mentioned analysis framework to create a static analysis tool without using Go parser of `go/packages` directly. The analysis package gives you a paradigm, a structure that you have to follow. You get a lot of good features and utilities in return. For instance, it greatly simplifies [analyzer testing](https://godoc.org/golang.org/x/tools/go/analysis/analysistest). If you wonder, lintpack also had [linttest](https://github.com/go-lintpack/lintpack/tree/master/linttest) package that does the same, only the magic comment syntax is different.

</blockquote></details>

# ruleguard - created to be extended

![](https://habrastorage.org/webt/zp/ym/rj/zpymrjjb8zkqa_c069ccd-yf3xg.png)

[`go-ruleguard`](https://github.com/quasilyte/go-ruleguard) is a static analysis tool that includes **zero** inspections by default.

Rule definitions are loaded during the start from a special `gorules` file which describes bad code patterns in a declarative way. For every such pattern, there is an associated message to be printed if the pattern would match. That file is an extension point and is intended to be edited by the users.

You don't need to re-compile the linter driver (main) program just to register new checks. This is why we can call these rules [dynamic](https://medium.com/@vktech/noverify-dynamic-rules-for-static-analysis-8f42859e9253).

`ruleguard` driver program looks like this:

```go
package main

import (
	"github.com/quasilyte/go-ruleguard/analyzer"
	"golang.org/x/tools/go/analysis/singlechecker"
)

func main() {
	singlechecker.Main(analyzer.Analyzer)
}
```

`analyzer` is implemented via [ruleguard](https://godoc.org/github.com/quasilyte/go-ruleguard/ruleguard) package. If you want to use `ruleguard` as a library, this is the right package to use.

# ruleguard VS revive

Let's take a simple yet real code example. Imagine that we want to bad [runtime.GC()](https://golang.org/pkg/runtime/#GC) calls in our programs. Revive has a `call-to-gc` diagnostic for that.

`call-to-gc` implementation (70 lines of code):

```go
package rule

import (
	"go/ast"

	"github.com/mgechev/revive/lint"
)

// CallToGCRule lints calls to the garbage collector.
type CallToGCRule struct{}

// Apply applies the rule to given file.
func (r *CallToGCRule) Apply(file *lint.File, _ lint.Arguments) []lint.Failure {
	var failures []lint.Failure
	onFailure := func(failure lint.Failure) {
		failures = append(failures, failure)
	}

	var gcTriggeringFunctions = map[string]map[string]bool{
		"runtime": map[string]bool{"GC": true},
	}

	w := lintCallToGC{onFailure, gcTriggeringFunctions}
	ast.Walk(w, file.AST)

	return failures
}

// Name returns the rule name.
func (r *CallToGCRule) Name() string {
	return "call-to-gc"
}

type lintCallToGC struct {
	onFailure             func(lint.Failure)
	gcTriggeringFunctions map[string]map[string]bool
}

func (w lintCallToGC) Visit(node ast.Node) ast.Visitor {
	ce, ok := node.(*ast.CallExpr)
	if !ok {
		return w // nothing to do, the node is not a call
	}

	fc, ok := ce.Fun.(*ast.SelectorExpr)
	if !ok {
		return nil // nothing to do, the call is not of the form pkg.func(...)
	}

	id, ok := fc.X.(*ast.Ident)

	if !ok {
		return nil // in case X is not an id (it should be!)
	}

	fn := fc.Sel.Name
	pkg := id.Name
	if !w.gcTriggeringFunctions[pkg][fn] {
		return nil // it isn't a call to a GC triggering function
	}

	w.onFailure(lint.Failure{
		Confidence: 1,
		Node:       node,
		Category:   "bad practice",
		Failure:    "explicit call to the garbage collector",
	})

	return w
}
```
</details>

This is how it's done in `ruleguard`:

```go
package gorules

import "github.com/quasilyte/go-ruleguard/dsl/fluent"

func callToGC(m fluent.Matcher) {
	m.Match(`runtime.GC()`).Report(`explicit call to the garbage collector`)
}
```

In my opinion, this approach is almost as terse as we can get without sacrificing the Go syntax which is useful for us to get tooling support when editing `gorules`.

We'll get to the more exciting examples now, but you should already see the difference.

# Quickstart

There is a [rangeExprCopy](https://go-critic.github.io/overview#rangeExprCopy-ref) checker in `go-critic` linter. It finds potentially unwanted array copying.

This code is iterated over a **copy** of the array:

```go
var xs [2048]byte
for _, x := range xs { // Copies 2048 bytes
	// Loop body.
}
```

Since every iteration does element copy as well, we copy 2 times more bytes than we might expect.

A fix is quite simple, it requires only 1 character addition:

```diff
  var xs [2048]byte
- for _, x := range xs {  // Copies 2048 bytes
+ for _, x := range &xs { // No copy
  	// Loop body.
  }
```

Most likely, you don't need that excessive array value copy. Fixed code performance is almost always superior, especially for a bigger array size. You can either wait until the Go compiler is better or you can find such code patterns and fix them today.

This inspection can be implemented in terms of `ruleguard` DSL:

```go
package gorules

import "github.com/quasilyte/go-ruleguard/dsl/fluent"

func _(m fluent.Matcher) {
    m.Match(`for $_, $_ := range $x { $*_ }`,
            `for $_, $_ = range $x { $*_ }`).
            Where(m["x"].Addressable && m["x"].Type.Size >= 128).
            Report(`$x copy can be avoided with &$x`).
            At(m["x"]).
            Suggest(`&$x`)
}
```

This rule finds all `for-range` loops which uses both loop variables (only this case leads to unwanted copy). [Where()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Matcher.Where) requires that iterated expression `$x` is [addressable](https://golang.org/ref/spec#Address_operators) and its size should be at least 128 bytes.

[Report()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Matcher.Report) defines an associated message that should be given to the user if the pattern is matched. [Suggest()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Matcher.Suggest) specifies a `quickfix` pattern that can be used by your editor through [gopls](https://github.com/golang/tools/tree/master/gopls) (or other Go LSP) or via command-line API if you use `-fix` parameter (we'll discuss that later in more detail). [At()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Matcher.At) binds the warning **and** `quickfix` location to the specific part of the match. We need that location specification so we rewrite `$x` to `&$x` instead of replacing the entire for loop with that.

Both `Report()` and `Suggest()` accept a template-like string that can reference pattern submatches. Predefined variable `$$` refers to the entire match, like `$0` in the regular expressions.

To try it out, let's create a `rangecopy.go` file:

```go
package example

// sizeof(builtins[...]) = 240 on x86-64
var builtins = [...]string{
	"append", "cap", "close", "complex", "copy",
	"delete", "imag", "len", "make", "new", "panic",
	"print", "println", "real", "recover",
}

func builtinID(name string) int {
	for i, s := range builtins {
		if s == name {
			return i
		}
	}
	return -1
}
```

Now we run the `ruleguard`:

```bash
$ ruleguard -rules rules.go -fix rangecopy.go
rangecopy.go:12:20: builtins copy can be avoided with &builtins
```

If we look into `rangecopy.go` again, we'll see the fixed result, because `ruleguard` was called with `-fix` argument.

By the way, simpler rules can be debugged without `gorules` file:

```
$ ruleguard -c 1 -e 'm.Match(`return -1`)' rangecopy.go
rangecopy.go:17:2: return -1
16		}
17		return -1
18	}
```

Thanks to the [singlechecker](https://godoc.org/golang.org/x/tools/go/analysis/singlechecker) package, we have `-c` option that controls how many "context lines" are printed along with the match result.

This option is a little bit weird: default value is `-c=-1` which means "no context lines" while `-c=0` gives you exactly one context line (the matched line itself).

Some more notable features of `gorules`:

* [Type templates](https://github.com/quasilyte/go-ruleguard/blob/master/docs/gorules.md#type-pattern-matching) to match expected types. For example, `map[$t]$t` describes all maps that have key type identical to the element type and `*[$len]$elem` matches all pointers to arrays.
* There can be multiple rules inside one function. Functions themselves are called [rule groups](https://github.com/quasilyte/go-ruleguard/blob/master/docs/gorules.md#rule-group-statements).
* All rules inside a group are applied one after another, in the order they are defined. The first matched rule makes all remaining rules to be skipped for the matched node. This is needed in cases where patterns are defined with priorities in mind: when you want to rewrite `$x=$x+$y` to `$x+=$y` it would be desirable to have a higher priority pattern `$x=$x+1` that is rewritten to `$x++` instead of `$x+=1`.

See more information about the DSL in [docs/gorules.md](https://github.com/quasilyte/go-ruleguard/blob/master/docs/gorules.md) file.

# Multi-rule function example

```go
package gorules

import "github.com/quasilyte/go-ruleguard/dsl/fluent"

func exampleGroup(m fluent.Matcher) {
        // Find potentially incorrect usages of json.Decoder.
        // See http://golang.org/issue/36225
        m.Match(`json.NewDecoder($_).Decode($_)`).
                Report(`this json.Decoder usage is erroneous`)

        // Smart unconvert, removes redundant conversions.
        m.Match(`time.Duration($x) * time.Second`).
                Where(m["x"].Const).
                Suggest(`$x * time.Second`)

        // Suggest to replace fmt.Sprint($x) with a call to String() method
        // if $x has such method.
        m.Match(`fmt.Sprint($x)`).
                Where(m["x"].Type.Implements(`fmt.Stringer`)).
                Suggest(`$x.String()`)

        // Simplify some boolean expressions.
        m.Match(`!($x != $y)`).Suggest(`$x == $y`)
        m.Match(`!($x == $y)`).Suggest(`$x != $y`)
}
```

If a rule has no explicit [Report()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Matcher.Report) call, [Suggest()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Matcher.Suggest) message is used instead.

Submatch filters can have various property constraints:

* [Var.Pure](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Var) requires an expression to be side-effect-free.
* [Var.Const](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Var) expects an expression to be usable inside a const context
* And more...

For `package-qualified` type names like `fmt.Stringer` you need to use [Import()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent#Matcher.Import) method. For convenience reasons, all stdlib packages are imported by default, this is why we don't need to import anything in the examples above.

#  quickfix actions

`quickfix` actions are implemented by `analysis` framework, we get them for free.

In the `analysis` model, analyzer generates [diagnostics](https://godoc.org/golang.org/x/tools/go/analysis#Diagnostic) and [facts](https://godoc.org/golang.org/x/tools/go/analysis#Fact). Diagnostics are sent to the users, facts are used by other analyzers.

A diagnostic can have a list of [suggested fixes](https://godoc.org/golang.org/x/tools/go/analysis#SuggestedFix). Every suggestion tells how to modify source code in a given location to resolve a problem found by a diagnostic.

More detailed description can be found inside [suggested_fixes.md design document](https://github.com/golang/tools/blob/master/go/analysis/doc/suggested_fixes.md).

# Using from the golangci-lint

[golangci-lint](https://github.com/golangci/golangci-lint) integrates `go-critic`, which in turn includes `ruleguard`.

If you have a new version of `golangci-lint` that includes [PR1148](https://github.com/golangci/golangci-lint/pull/1148), you can use `ruleguard` through `golangci-lint`.

To make it work, you must ensure that:

1. `gocritic` linter is enabled
2. `ruleguard` check is enabled as well
3. `rules` parameter is set

Here is a minimal example of `.golangci.yml` that satisfies these 3 conditions:

```
linters:
  enable:
    - gocritic
linters-settings:
  gocritic:
    enabled-checks:
      - ruleguard
    settings:
      ruleguard:
        rules: "rules.go"
```

When you run `golangci-lint` with this configuration file, you should see warnings that come from the rules you defined:

```bash
$ golangci-lint run example.go 
example.go:5:9: ruleguard: can rewrite as xs[0] == ys[0] (gocritic)
        return !(xs[0] != ys[0])
               ^
```

When changing the rules file, you might need to do a cache cleanup once in a while:

```bash
golangci-lint cache clean
```

In general, it's easier to debug your rules with `ruleguard` binary, but for integration purposes, `golangci-lint` is priceless.

# Closing words

![](https://habrastorage.org/webt/m3/bs/zw/m3bszwzp2nwkxrnxdnenypatyjk.png)

Try `ruleguard` on your projects.

In case you found any bug in `ruleguard` or you have a feature request, please [open an issue](https://github.com/quasilyte/go-ruleguard/issues/new) and let me know.

Here are some ideas on how you can use `ruleguard`:

* Custom inspections implementations.
* Automated code modernization or refactoring with `-fix`.
* Collect and process code statistics with the help of [-json](https://github.com/golang/tools/blob/master/go/analysis/internal/analysisflags/flags.go#L76) flag.

`ruleguard` development plans:

* Test ideas from [Applied Go code similarity analysis](https://github.com/quasilyte/talks/tree/master/2019-7-Oct-moscow) ([code normalization](https://github.com/quasilyte/astnorm))
* Add new DSL features. [sub-matches](https://github.com/quasilyte/go-ruleguard/issues/28) is one of the examples.
* Borrow and adapt ideas from [CodeQL](https://github.com/github/codeql-go) and [comby](https://comby.dev/).

# Useful links and resources

* [Ruleguard by example](https://go-ruleguard.github.io/by-example/) is an example-based tutorial
* [Ruleguard vs Semgrep vs CodeQL](https://speakerdeck.com/quasilyte/ruleguard-vs-semgrep-vs-codeql)
* Recommended rules file example: [rules.go](https://github.com/quasilyte/go-ruleguard/blob/master/rules.go)
* [dsl/fluent package docs](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl/fluent)
* [ruleguard package docs](https://godoc.org/github.com/quasilyte/go-ruleguard/ruleguard)
* AST matching engine used in ruleguard: [mvdan/gogrep](https://github.com/mvdan/gogrep)
* [Dynamic Rules for Static Analysis](https://medium.com/@vktech/noverify-dynamic-rules-for-static-analysis-8f42859e9253)
