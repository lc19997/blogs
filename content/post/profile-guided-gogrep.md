+++
date = "2021-12-21"
title = "Profile-guided code search"
tags = [
    "[go]",
    "[gogrep]",
    "[habr-translation]",
]
description = "Filtering gogrep results using the CPU profiles to find hot path matches."
draft = false
+++

<center>If you found a typo or a misspelling, please [file an issue](https://github.com/quasilyte/blog-src/issues/new) or send a PR that fixes it.</center>

> Original (ru): <https://habr.com/ru/post/596755/>.

If you combine a structural code search of [gogrep](https://github.com/quasilyte/gogrep) and CPU profile filtering with [perf-heatmap](https://github.com/quasilyte/perf-heatmap), you'll get a profile-guided code search.

This combo lets you find complex code patterns that occur only on the "hot" code path, so optimizing them can be worthwhile.

perf-heatmap can also be used by a text editor to display the source code lines performance heat levels as shown below.

![](https://habrastorage.org/webt/a9/xv/tb/a9xvtbb0_gzuvebzlobcw9wyjb4.jpeg)

<cut/>

## Introduction

Let's start with a question why would you ever want to do this. We have good tools to work with CPU profiles after all, `go tool pprof` has great web UI and it's available for everyone.

When using pprof, it's easy to know that `f()` function takes a lot of time. It's also possible to learn the paths that lead to that `f` function calls. It's less obvious to see what combination of arguments these calls imply though. And sometimes you want to have more semantics than just per-function aggregation.

Let me show an example. Suppose that `fmt.Errorf` happened to enter the top-20 functions that consume the CPU. gogrep+heatmap make it possible to find `fmt.Errorf` calls of one argument that happen on these hot spots which collectively make this function reach the top-20. If `fmt.Errorf` is called with only one argument, perhaps we can replace that call with something like `errors.New`.

```diff
- return fmt.Errorf("can't open database")
+ return errors.New("can't open database")
```

Or, even better, we can use a once-allocated error object, since the error message content is constant.

```go
// Global scope.
var errOpenDatabase = errors.New("can't open database")

// Somewhere inside a hot function.
return errOpenDatabase
```

Well, the gogrep pattern for this 1-arg call is really simple, `fmt.Errorf($_)`. Now we need to filter the results to be aligned with the CPU profile insights we had above.

But first things first, we need to collect a CPU profile.

## Collecting a CPU profile

There are different CPU profile collecting patterns:

1. Collecting a profile via benchmarks when running them with `-cpuprofile` argument
2. Recording a profile for a running service/daemon program for a period of time
3. Recording a profile for a CLI program from start to the end

The (1) pattern is not very reliable as the results would heavily depend on the quality of the benchmarks. The second option is good as it may include a span of the real app workload reflected in a profile. So important things could get to the top of the profile. The third option is OK as long as you're running a program on an average or most common input which is not too artificial.

For simplicity, I'll stick to the benchmarking option, so you can reproduce my results without any hassle.

```bash
# Note: a cpu.out file will be created in a current directory.
# This command can take a few minutes to complete.
$ go test -cpuprofile cpu.out -bench . -timeout 20m -count 2 bytes
```

Go exports CPU profiles in [profile.proto](https://github.com/google/pprof/blob/6f57359322fd9ce2d6dabde5b733714463416b6f/proto/profile.proto) format.

We can parse them using the [github.com/google/pprof/profile](https://pkg.go.dev/github.com/google/pprof/profile) library.

## perf-heatmap

The [perf-heatmap](https://github.com/quasilyte/perf-heatmap.git) package creates a special index from a CPU profile in `profile.proto` format.

It's bundled with a simple CLI tool `cmd/perf-heatmap` that has two main commands:

* `perf-heatmap json cpu.out` build and print the heatmap index in the JSON format
* `perf-heatmap stat cpu.out` like the json command, but dumps in debug format

If you have Go installed, this command will install that program:

```bash
$ go install github.com/quasilyte/perf-heatmap/cmd/perf-heatmap@latest
```

All samples that are contained inside the `cpu.out` are pointing to the code locations that were executed at least once. But that alone is not enough to consider these locations to be "hot".

perf-heatmap has `-threshold` option that tells it to keep only some percentage of the line-aggregated values. For example, a threshold of 0.5 keeps the top 50%. A threshold of 1.0 would include all samples and threshold of 0.1 would keep only the hottest 10%.

The lines that made it into the topN% will be divided into the 5 classes: from the least hot ones to the hottest ones.

All data points that are kept (topN%) are considered hot, but they'll have different score levels (heat levels from 1 to 5, inclusively). Every line is annotated with two levels: local (per-file) and global (whole program). Most of the time we care about the global level the most.

```bash
# Dump the heatmap index info for the CPU profile,
# filter the output to only include the buffer.go file data.
$ perf-heatmap stat -filename buffer.go cpu.out
  func bytes.(Buffer).Write ($GOROOT/src/bytes/buffer.go):
    line  168:   0.34s L=0 G=3
    line  170:   0.56s L=4 G=3
    line  172:   0.44s L=0 G=3
    line  174:   3.97s L=5 G=5
  func bytes.(Buffer).Grow ($GOROOT/src/bytes/buffer.go):
    line  161:   0.11s L=5 G=1
  func bytes.(Buffer).Read ($GOROOT/src/bytes/buffer.go):
    line  298:   0.05s L=0 G=0
    line  299:   0.25s L=4 G=2
    line  307:   3.34s L=5 G=5
    line  308:   0.06s L=0 G=1
    line  309:   0.18s L=3 G=2
    line  310:   0.14s L=0 G=2
  func bytes.(Buffer).grow ($GOROOT/src/bytes/buffer.go):
    line  117:   0.04s L=0 G=0
    line  118:   0.01s L=0 G=0
    line  120:   0.04s L=2 G=0
    line  128:   0.01s L=0 G=0
    line  132:   0.01s L=0 G=0
    line  137:   0.19s L=5 G=2
    line  142:   0.16s L=4 G=2
    line  143:   0.05s L=3 G=0
    line  148:   0.01s L=0 G=0
  func bytes.(Buffer).tryGrowByReslice ($GOROOT/src/bytes/buffer.go):
    line  107:   1.26s L=5 G=4
    line  108:   0.27s L=0 G=2
  func bytes.makeSlice ($GOROOT/src/bytes/buffer.go):
    line  229:   0.16s L=5 G=2
  func bytes.(Buffer).empty ($GOROOT/src/bytes/buffer.go):
    line   69:   0.25s L=5 G=2
  func bytes.(Buffer).WriteByte ($GOROOT/src/bytes/buffer.go):
    line  263:   0.66s L=0 G=4
    line  265:   0.88s L=4 G=4
    line  269:   1.11s L=5 G=4
    line  270:   0.86s L=0 G=4
  func bytes.(Buffer).readSlice ($GOROOT/src/bytes/buffer.go):
    line  418:   0.05s L=0 G=0
    line  419:   0.97s L=5 G=4
    line  420:   0.01s L=0 G=0
  func bytes.(Buffer).Len ($GOROOT/src/bytes/buffer.go):
    line   73:   0.01s L=5 G=0
  func bytes.(Buffer).ReadString ($GOROOT/src/bytes/buffer.go):
    line  438:   1.03s L=0 G=4
    line  439:   3.30s L=5 G=5
  func bytes.(Buffer).WriteRune ($GOROOT/src/bytes/buffer.go):
    line  277:   0.25s L=0 G=2
    line  283:   0.22s L=0 G=2
    line  284:   0.36s L=3 G=3
    line  288:   2.51s L=5 G=4
    line  289:   0.54s L=4 G=3
    line  290:   0.17s L=0 G=2
  func bytes.(Buffer).String ($GOROOT/src/bytes/buffer.go):
    line   65:   0.04s L=5 G=0
```

* `L` is a local heat level
* `G` is a global heat level

## Heat levels by example

Suppose that we have a CPU profile that has the following samples:

| file | line | value |
|---|---|---|
| `a.go` | `10` | `100` |
| `a.go` | `10` | `200` |
| `a.go` | `13` | `200` |
| `b.go` | `40` | `100` |
| `b.go` | `40` | `300` |
| `b.go` | `40` | `400` |
| `b.go` | `49` | `100` |
| `b.go` | `49` | `100` |
| `b.go` | `50` | `500` |
| `b.go` | `51` | `100` |

The first step is to calculate the sum of all samples pointing to the same line:

| file | line | total value |
|---|---|---|
| `a.go` | `10` | `300` |
| `a.go` | `13` | `200` |
| `b.go` | `40` | `800` |
| `b.go` | `49` | `200` |
| `b.go` | `50` | `500` |
| `b.go` | `51` | `100` |

To assign the global heat level, we need to sort all values in descending order and take the first N entries that go through the threshold. Let's take threshold=0.5 case for the first example. 3 entries made it into the topN%.

| file | line | total value |
|---|---|---|
| +`b.go`+ | +`40`+ | +`800`+ |
| +`b.go`+ | +`50`+ | +`500`+ |
| +`a.go`+ | +`10`+ | +`300`+ |
| `a.go` | `13` | `200` |
| `b.go` | `49` | `200` |
| `b.go` | `51` | `100` |

If we set threshold to 0.9, we'll get 5 entries: 800, 500, 300, 200, 200.

Assigning global levels among that group of 5 elements is simple:

| file | line | total value | global heat level |
|---|---|---|---|
| `b.go` | `40` | `800` | 5 |
| `b.go` | `50` | `500` | 4 |
| `a.go` | `10` | `300` | 3 |
| `a.go` | `13` | `200` | 2 |
| `b.go` | `49` | `200` | 1 |
| `b.go` | `51` | `100` | 0 |

For 10 elements we would get 2 elements per heat level. For the cases when we can't split the elements as nicely, some approximation is used: 8 elements will form `[2, 1, 2, 1, 2]` buckets.

For the local heat levels we would take a slice of the samples that belong to the file as opposed to using all samples from the profile. Every line gets annotated with both levels.

## gogrep + heatmap

Now let's install a gogrep that includes heatmap support:

```bash
$ go install github.com/quasilyte/gogrep/cmd/gogrep@latest
```

Let's run some queries over the `bytes` package:

```bash
# Enter the "bytes" package source dir.
$ cd $(go env GOROOT)/src/bytes

# Find all append calls that are located on a hot path
# with respect to the collected CPU profile.
$ gogrep -heatmap cpu.out . 'append($*_)' '$$.IsHot()'
bytes.go:487: 				spans = append(spans, span{start, i})
bytes.go:500: 		spans = append(spans, span{start, len(s)})
bytes.go:626: 			return append([]byte(""), s...)
bytes.go:656: 			return append([]byte(""), s...)
bytes.go:702: 			b = append(b, byte(c))
bytes.go:710: 				b = append(b, replacement...)
bytes.go:715: 		b = append(b, s[i:i+wid]...)
found 7 matches
```

Perhaps you're not very familiar with gogrep and [ruleguard](https://github.com/quasilyte/go-ruleguard) idioms, so let me give you a couple of hints:

* `$*_` means from 0 to N matches of arbitrary expressions (so it's "any args" above)
* `$$` is a special variable available in the filter that references the entire match, like `$0` in regexp

By default, gogrep uses a threshold value of `0.5`. It's possible to override that value using the `-heatmap-threshold` parameter.

```bash
$ gogrep -heatmap cpu.out -heatmap-threshold 0.1 . 'append($*_)' '$$.IsHot()'
bytes.go:487: 				spans = append(spans, span{start, i})
bytes.go:500: 		spans = append(spans, span{start, len(s)})
bytes.go:626: 			return append([]byte(""), s...)
bytes.go:656: 			return append([]byte(""), s...)
found 4 matches
```

And by the way, this is how you can find all 1-arg calls of `fmt.Errorf()` with heatmap filtering:

```bash
$ gogrep -heatmap cpu.out . 'fmt.Errorf($format)' '$format.IsHot()'
```

I used the named var `$format` on purpose, to demonstrate that filters can be applied to any variable, not just `$$`.

> asciinema screencast: [asciinema.org/a/j8JM8prOFscPPCXJJPXpYwjil](https://asciinema.org/a/j8JM8prOFscPPCXJJPXpYwjil)

## How heatmap is integrated

Suppose that `<var>.IsHot()` is just some `func isHot(var gogrepVar) bool` function.

`gogrepVar` captures an AST which matched a pattern. For `$$` that is an entire tree while named variables like `$x` can capture a subtree. Technically speaking, it's always some `ast.Node` object plus a "variable name".

`isHot(v)` gets a source code lines range like `[fromLine, toLine]` using the `ast.Node` position info. Then we're calling [Index.QueryLineRange(..., fromLine, toLine)](https://pkg.go.dev/github.com/quasilyte/perf-heatmap/heatmap#Index.QueryLineRange). If that range contains at least one data point that passes the specified threshold (e.g. 0.5), then `isHot(v)` would return true.

## VS Code extension

[perf-heatmap extension on marketplace.visualstudio.com](https://marketplace.visualstudio.com/items?itemName=quasilyte.perf-heatmap)

It's not necessary to install the `perf-heatmap` binary to use an extension. I have compiled the perf-heatmap package to JS using the [gopherjs](https://github.com/gopherjs/gopherjs), so the extension can parse the profile and build indexes on its own.

How to use the extension:

1. CPU profile should be loaded into the memory first. Use `perf-heatmap.loadProfile` to do that.
2. When profile is indexed, any opened Go file can be annotated. Use `perf-heatmap.annotateLocalLevels` and `perf-heatmap.annotateGlobalLevels` commands to do that.

These commands can be found using the command palette window (`ctrl+shift+p` `workbench.action.showCommands`).

![](https://habrastorage.org/webt/ad/w-/bc/adw-bc8dt_sej_rqm5hvshmzsw4.jpeg)

> I created this plugin as a proof-of-concept. It's not that useful and convenient to use right now. You may help to improve it by [contributing to it](https://github.com/quasilyte/vscode-perf-heatmap).

## Profile symbols mapping

In the most trivial situation, the CPU profile contains identical paths and there are no problems in mapping them with sources. We can use absolute paths matching in this situation.

If, however, a profiled binary was built on some build agent machine, its symbol paths can be different. For example, a filename would be `/usr/blah/build438/go/src/bytes/buffer.go` instead of `/home/quasilyte/go/src/bytes/buffer.go`.

This issue can be solved using the trim-prefix hack when we either try to infer a common prefix (unreliable) or ask the user to provide it (inconvenient, bad UX).

In order to avoid this issue completely, heatmap uses a set of keys that are enough to somewhat reliably identify the source file without resorting to file system paths.

```go
type Key struct {
	// TypeName is a receiver type name for methods.
	// For functions it should be empty.
	TypeName string

	// FuncName is a Go function name.
	// For methods, TypeName+FuncName compose a full method name.
	FuncName string

	// Filename is a base part of the full file path.
	Filename string

	// PkgName is the name of the package that defines this symbol.
	PkgName string
}
```

Suppose that our key is `Key{"Buffer", "Write", "buffer.go", "bytes"}`, then we can perform the following queries:

* `QueryLine(key, 10) => HeatLevel`
* `QueryLineRange(key, 20, 40) => []HeatLevel`

HeatLevel - is a simple pair `{LocalHeatLevel, GlobalHeatLevel}`.

For source lines that have no samples or their values are lower than a threshold, their scores will be equal to 0.

## Final thoughts

![](https://github.com/quasilyte/perf-heatmap/raw/master/_docs/logo_small.png)

gogrep+heatmap can extend your arsenal of performance investigation tools. It'll help you when other tools were not enough. Just like flamegraphs can give you extra insights in some cases, using heatmaps with gogrep can be a more productive way for some situations.

Before a goodbye, I'll share yet another example.

```bash
$ gogrep --heatmap cpu.out . \
  'var $b bytes.Buffer; $*_; return $b.$m()' \
  '$m.Text() == "String" && $m.IsHot()'
```

A query above finds localized usages of the `bytes.Buffer` that end with a call to its `String()` method. In some cases, using `strings.Builder` can be better here.

Rather than using a `$b.String()` inside a pattern, I introduced a `$m` variable to make it possible to reference it from the filters. We're binding `IsHot()` to `$m` instead of `$$` (which would be a multi-line, multi-statement match).

Using pprof in this case could show that `bytes.(*Buffer).String` is a hot function, but this information itself is not very actionable. With gogrep, we can locate a code that can be improved by a transition from `bytes.Buffer` to `strings.Builder`.

Here is another good example:

```bash
$ gogrep --heatmap cpu.out . \
  'return $*_, errors.New($x)' \
  '$$.IsHot() && $x.IsStringLit()'
```

In the big codebase, there could be hundreds, if not thousands, places where `errors.New()` is used. More often than not, you don't care about the performance of the error path. Unless that error path can be exploited by the users or your system can frequently have data inputs that result in an error. We may want to find this 0.1% of the cases that can (and they probably should) be optimized.

Note that the example above also filters out the cases with dynamic error message creation, like `errors.New("db: " + msg)`. We only find the spots that can benefit from replacing a call with a global variable that is allocated only once.

I hope that you'll have a lot of fun with this thing, just like I did.
