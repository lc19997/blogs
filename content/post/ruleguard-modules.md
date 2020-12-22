+++
date = "Mon Dec 21 00:54:25 MSK 2020"
title = "ruleguard rules package management"
tags = [
    "[go]",
    "[shortread]",
    "[ruleguard]",
    "[static-analysis]",
]
description = "A quick intro into the ruleguard rules packaging."
draft = false
+++

**Bundles** is a new feature coming to the [ruleguard](github.com/quasilyte/go-ruleguard). It'll make it possible to re-use third-party rules without having to copy/paste them.

## Creating an importable bundle

A package that exports rules must define a [Bundle](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl#Bundle) object:

```go
package gorules

import "github.com/quasilyte/go-ruleguard/dsl"

// Bundle holds the rules package metadata.
//
// In order to be importable from other gorules package,
// a package must define a Bundle variable.
var Bundle = dsl.Bundle{}

func boolComparison(m dsl.Matcher) {
	m.Match(`$x == true`,
		`$x != true`,
		`$x == false`,
		`$x != false`).
		Report(`omit bool literal in expression`)
}
```

That package should be a separate [Go module](https://github.com/golang/go/wiki/Modules). A rules bundle is versioned by its Go module.

It's possible to have several ruleguard files inside one Go module. Only one file should define a Bundle object. During a bundle import, all files will be exported.

> The metadata object is called a `Bundle` to avoid confusion with Go packages and Go modules. It's useful to have a dedicated word for them.

## Importing a bundle

A package that wants to extend some rule set should import that package and then use its bundle in [ImportRules()](https://godoc.org/github.com/quasilyte/go-ruleguard/dsl#ImportRules) call:

```go
package gorules

import (
	"github.com/quasilyte/go-ruleguard/dsl"
	quasilyterules "github.com/quasilyte/ruleguard-rules-test"
)

func init() {
	// Imported rules will have a "qrules" prefix.
	dsl.ImportRules("qrules", quasilyterules.Bundle)
}

// Then you can define your own rules.

func emptyStringTest(m dsl.Matcher) {
	m.Match(`len($s) == 0`).
		Where(m["s"].Type.Is("string")).
		Report(`maybe use $s == "" instead?`)

	m.Match(`len($s) != 0`).
		Where(m["s"].Type.Is("string")).
		Report(`maybe use $s != "" instead?`)
}
```

Now all you need is to install the imported [github.com/quasilyte/ruleguard-rules-test](https://github.com/quasilyte/ruleguard-rules-test) package. Since bundles are Go modules, it's as simple as installing any other Go module:

```bash
go get -v github.com/quasilyte/ruleguard-rules-test
```

It's possible to use an empty (`""`) prefix, but you'll risk getting a name collision. If you don't define your own rules, then it's perfectly 
fine to use an empty prefix.

All ruleguard packages are named `gorules`, so you'll need to assign a local package name. In the example above, we used `quasilyterules` name.

## Running the ruleguard

If you installed the bundle, you should be able to run your main rules file normally:

```bash
$ ruleguard -rules rules.go test.go 
test.go:4:6: emptyStringTest: maybe use s == "" instead? (rules.go:13)
test.go:5:6: qrules/boolComparison: omit bool literal in expression (rules1.go:8)
```

Using ruleguard from the [go-critic](https://github.com/go-critic/go-critic) or [golangci-lint](https://github.com/golangci/golangci-lint) stays the same. As long as bundles are installed and they can be located by the `go list $package_path`, everything should work fine.

<hr>

Limitations:

* Imported packages can't import other bundle packages (could be addressed later)
* Bundles are tied to Go modules; they might now work properly without them
