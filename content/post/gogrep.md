+++
date = "Sun Jun  7 14:11:49 MSK 2020"
title = "gogrep: structural search and replace"
tags = [
    "[go]",
    "[habr-translation]",
    "[refactoring]",
    "[tutorial]"
]
description = "Learn how to use gogrep from the command line and VS Code."
draft = false
+++

<center>If you found a typo or a misspelling, please [file an issue](https://github.com/quasilyte/blog-src/issues/new) or send a PR that fixes it.</center>

![](https://habrastorage.org/webt/l7/s7/gc/l7s7gccru4deae3xyln5scwfof8.jpeg)

> Original (ru): https://habr.com/post/481696/.

[gogrep](https://github.com/mvdan/gogrep) is one of my favorite tools. It gives you the ability to search Go code using syntax patterns, filter matches by expression types, and perform structural replace.

Today I'll talk about how to use `gogrep` and about [VS Code extension](https://marketplace.visualstudio.com/items?itemName=quasilyte.gogrep) that integrates `gogrep` into the editor.

# Why do you need gogrep?

In a nutshell, `gogrep` is useful for:

* Refactoring (search and replace)
* Codebase exploration
* Suspicious code detection (example: [ruleguard](https://habr.com/ru/post/481696/))

I'll show you an example that demonstrates the power and elegance of the structural search.

In the snippet below, both `a()` and `b()` functions perform the same operation:

```go
func a(xs []int) []int {
  xs = append(xs, 1)
  xs = append(xs, 2)
  return xs
}

func b(xs []int) []int {
  xs = append(xs, 1, 2)
  return xs
}
```

Suppose we want to re-write all places where `append` calls can be merged.

Let's try `gogrep`:

```bash
gogrep -w -x '$x=append($x,$a);$x=append($x,$b)' -s '$x=append($x,$a,$b)' ./...
```

* Find all replacement candidates with `-x` command
* Re-write matches with `-s` command `$x=append($x,$a,$b)` pattern
* Add a `-w` argument to write replacements to the source files instead of stdout

With [gogrep extension for VS Code](https://marketplace.visualstudio.com/items?itemName=quasilyte.gogrep) it becomes even easier.

Here is an example of `+=1` replacement with `++`:

![](https://habrastorage.org/webt/bc/pq/1x/bcpq1xmkv2fq3annn9oo0lu9t6i.gif)

Real-world example: once upon a time, I wanted to perform a global `slice[:] -> slice` replacement. I've even opened an [issue in staticcheck](https://github.com/dominikh/go-tools/issues/282). The major problem is that you can't just search for `[:]` without knowing the types. `array[:]` expression makes sense, we only want to match strings and slices.

Here is how you can perform such replacement for `[]byte` slices inside Go stdlib:

```bash
# Search only.
gogrep -x '$s[:]' -a 'type([]byte)' std

# Search+replace.
gogrep -x '$s[:]' -a 'type([]byte)' -s '$s' -w std
```

<details>
<summary>If you wonder what would that query reveal</summary>

Only the first 30 results are showed (out of 300+):

```
$GOROOT/src/archive/tar/format.go:163:59: b[:]
$GOROOT/src/archive/tar/reader.go:345:33: tr.blk[:]
$GOROOT/src/archive/tar/reader.go:348:17: tr.blk[:]
$GOROOT/src/archive/tar/reader.go:348:28: zeroBlock[:]
$GOROOT/src/archive/tar/reader.go:349:34: tr.blk[:]
$GOROOT/src/archive/tar/reader.go:352:18: tr.blk[:]
$GOROOT/src/archive/tar/reader.go:352:29: zeroBlock[:]
$GOROOT/src/archive/tar/reader.go:396:23: tr.blk[:]
$GOROOT/src/archive/tar/reader.go:497:36: blk[:]
$GOROOT/src/archive/tar/reader.go:528:33: blk[:]
$GOROOT/src/archive/tar/reader.go:531:14: blk[:]
$GOROOT/src/archive/tar/writer.go:392:26: blk[:]
$GOROOT/src/archive/tar/writer.go:477:23: zeroBlock[:]
$GOROOT/src/archive/zip/reader.go:233:29: buf[:]
$GOROOT/src/archive/zip/reader.go:236:15: buf[:]
$GOROOT/src/archive/zip/reader.go:251:30: buf[:]
$GOROOT/src/archive/zip/reader.go:254:15: buf[:]
$GOROOT/src/archive/zip/writer.go:92:17: buf[:]
$GOROOT/src/archive/zip/writer.go:110:19: buf[:]
$GOROOT/src/archive/zip/writer.go:116:30: buf[:]
$GOROOT/src/archive/zip/writer.go:132:27: buf[:]
$GOROOT/src/archive/zip/writer.go:157:17: buf[:]
$GOROOT/src/archive/zip/writer.go:177:27: buf[:]
$GOROOT/src/archive/zip/writer.go:190:16: buf[:]
$GOROOT/src/archive/zip/writer.go:198:26: buf[:]
$GOROOT/src/archive/zip/writer.go:314:18: mbuf[:]
$GOROOT/src/archive/zip/writer.go:319:31: mbuf[:]
$GOROOT/src/archive/zip/writer.go:386:16: buf[:]
$GOROOT/src/archive/zip/writer.go:398:23: buf[:]
$GOROOT/src/bytes/bytes.go:172:24: b[:]
```

</details>

# Search patterns

A **search pattern** is a small Go code fragment that can include **$-expressions** (we'll refer to them as "pattern variables"). A pattern can be an expression, a statement, or a declaration.

**Pattern variables** are Go variables with `$` prefix. Pattern variables with the same name always match identical AST nodes. The only exception is `$_` variable that can be used multiple times to express "whatever" parts of the pattern.

If `*` is placed before the pattern variable name then it will match zero or more nodes instead of exactly one.

| Search pattern | Interpretation |
|---|---|
| `$_` | Anything. |
| `$x` | Identical to the example above, "anything". |
| `$x = $x` | Self-assignment. |
| `(($_))` | Anything surrounded by two pairs of parens. |
| `if $init; $cond {$x} else {$x}`  | `if` with duplicated then/else blocks. |
| `fmt.Fprintf(os.Stdout, $*_)` |  `Fprintf` call with `os.Stdout` argument. |

As already demonstrated in the example with `append()`, the pattern can contain multiple statements. The "`$x; $y`" syntax means "find $x that is followed by $y".

`gogrep` performs a genuine backtracking for the patterns with `*`. For example, the pattern below can find all `map` literals that contain at least 1 duplicated key expression:

```
map[$_]$_{$*_, $key: $val1, $*_, $key: $val2, $*_}
```

> The Go compiler finds the duplicated map keys in literal, but only if they're constant expressions. `gogrep` pattern will find more suspicious candidates.

# Pipelines, commands, and attributes

We already used `-x` and `-s` commands before, but now it's time to describe them in detail.

`gogrep` accepts a list of **commands** that form a **pipeline**. The order of the commands matters. The full synopsis looks like this:

```
gogrep command [more commands...] [targets...]
```

A `target` can be a file, a folder, or a package (same as the `go build` targets).

My number-1 frequently used target is `./...` which performs a recursive search.

| Command | Description |
|---|---|
| `-x pattern` | Find nodes that match the `pattern`. |
| `-g pattern` | Discard matches that **do not** match the `pattern`. |
| `-v pattern` | Discard matches that **do** match the `pattern`. |
| `-a attribute` | Discard matches that **do not** have the `attribute`. |
| `-s pattern` | Re-write the match using the `pattern. |
| `-p n` | For every match, navigate up `n` node parents. |

Normally, `-x` is the very first pipeline command and is followed by other commands that perform filtering or substitution.

Examples should help you to understand this idea better.

```go
// file foo.go
package foo

func bar() {
	println(1)
	println(2)
	println(3)
}
```
```bash
# Find all println() calls.
$ gogrep -x 'println($*_)' foo.go
foo.go:4:2: println(1)
foo.go:5:2: println(2)
foo.go:6:2: println(3)

# The first -v discards matches that contain a literal 1.
# The second -v discards matches that contain a literal 2.
$ gogrep -x 'println($*_)' -v 1 -v 2 foo.go
foo.go:6:2: println(3)

# -p 2 traverses two nodes up, leading us to the containing *ast.BlockStmt.
$ gogrep -x 'println($*_)' -v 1 -v 2 -p 2 foo.go
foo.go:3:12: { println(1); println(2); println(3); }
```

There are a lot of **attributes** and they're quite underdocumented.

You can learn them from the [source code](https://github.com/mvdan/gogrep/blob/24e8804e5b3cbe82de972195f127eb3c3592d94b/parse.go#L362) though.

An example below will illustrate the concept of the attributes.

```bash
# Will match both numerical and string "+" operations.
gogrep -x '$lhs + $rhs'

# Matches only string concatenations.
gogrep -x '$lhs + $rhs' -a 'type(string)'
```

By default, `gogrep` does not perform a search inside test files. If you want to include these files, use `-tests` argument.

# VS Code extension overview

The extension exposes gogrep search commands (`Ctrl+Shift+P` or `Cmd+Shift+P`):

![](https://habrastorage.org/webt/t6/to/yt/t6toytgl4xxl07-twzwidbguzlu.jpeg)

Every command creates a search pattern prompt:

![](https://habrastorage.org/webt/y8/4q/f9/y84qf9p7mxjajc0oligrbt_8tho.jpeg)

Search results are printed to the **output channel** named `gogrep`:

![](https://habrastorage.org/webt/pj/c6/h-/pjc6h-nobw_bpbly_51t-cgi-sk.jpeg)

To perform a search and replace, divide the "Find" and "Replace" patterns with `->` token:

![](https://habrastorage.org/webt/72/7c/yy/727cyy4mbar0zq937ya_qfzrovo.jpeg)

The trailing `!` serves as a `-w` switch. If you end your search and replace patterns with it, your files will be updated. Otherwise, the replacement results are printed to the output channel.

An example of how you can find `append()` calls that can be combined:

![](https://habrastorage.org/webt/hn/nc/zc/hnnczclqvnxb1maimtze2qlkrqo.gif)

By default, the extension commands are not bound to any hotkey combination. If you want quick access to them, you can assign any shortcut, following your sense of what is ergonomic.

The extension can automatically install the `gogrep` binary for `linux-amd64`, `windows-amd64` and `darwin-amd64`. If something goes wrong or you're using a different platform, consider building `gogrep` from the source:

```bash
GO111MODULE=on go get mvdan.cc/gogrep
```

If you have a feature request (or a bug report), consider opening the [issue on the GitHub](https://github.com/quasilyte/vscode-gogrep/issues/new).

# Closing words

I hope this tutorial will make `gogrep` more accessible to people.

If you're using JetBrains IDE, you may be familiar with [structural search and replace](https://www.jetbrains.com/help/idea/structural-search-and-replace.html) (SSR) mechanism. It's basically doing the same thing, but you're can't use it outside of the IDE, so `gogrep` wins here.

In case you find yourself doing the same refactoring commands, again and again, consider to use the [ruleguard](https://github.com/quasilyte/go-ruleguard) with `-fix` option on the file save hook.

```go
// These 3 rules will find Fprint* calls with Stdout arguments and
// will replace them with Print* equivalents.
// The patterns inside Match() are gogrep patterns.
m.Match(`fmt.Fprint(os.Stdout, $*args)`).Suggest(`fmt.Print($args)`)
m.Match(`fmt.Fprintln(os.Stdout, $*args)`).Suggest(`fmt.Println($args)`)
m.Match(`fmt.Fprintf(os.Stdout, $*args)`).Suggest(`fmt.Printf($args)`)
```

Additional resources:

* [Daniel Martí gogrep talk](https://talks.godoc.org/github.com/mvdan/talks/2018/gogrep.slide)
* [gogrep pattern examples](https://github.com/quasilyte/go-ruleguard/blob/master/rules.go)
* [golang.org/x/tools/cmd/eg](https://godoc.org/golang.org/x/tools/cmd/eg)
