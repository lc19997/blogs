+++
date = "Tue Jun 15 00:48:18 MSK 2021"
title = "A single point of exit"
tags = [
    "[shortread]",
]
description = "A better way to write your `main()` function."
draft = false
+++

> There are other similar articles, like [Why you shouldn't use func main in Go](https://pace.dev/blog/2020/02/12/why-you-shouldnt-use-func-main-in-golang-by-mat-ryer.html). This post addresses the issue from a slightly different angle.

`tl;dr`: You program should probably have only **one** [os.Exit()](https://golang.org/pkg/os/#Exit) call, if any.

That includes all indirect calls: [log.Fatal()](https://golang.org/pkg/log/#Fatal) and any other function that calls `os.Exit()` at some point.

If your main looks like this, then this article is for you:

```go
func main() {
    x, err := doSomething()
    defer x.Close()
    if err != nil {
        log.Fatalf("failed to do something: %+v", err)
    }
    y, err := doSomethingElse(x)
    if err != nil {
        log.Fatalf("failed to do something else: %+v", err)
    }
    // ... and so on
}
```

What's the problem here? It calls `log.Fatal()` several times.

Why is that a problem?

* Do you see a deferred `x.Close()` call? If `doSomethingElse()` fails, the `log.Fatalf()` will be executed. That will lead to the `os.Exit()` quitting the program without executing any deferred calls.

* It's hard to refactor that code. If you'll keep the code as is and move it to a separate function, you'll end up with a function that can exit your program.

* It's even worse if you have `log.Fatal()` calls somewhere below the execution tree. For example, if `doSomethingElse` can exit on its own, we may not have a chance to log an error inside our main function. This makes the program flow more complicated than it could be.

Good news: you can fix these problems with one simple trick. Adhere to the single exit point idiom.

```go
func main() {
    if err := mainNoExit(); err != nil {
        log.Fatalf("error: %+v", err)
    }
}

func mainNoExit() error {
    x, err := doSomething()
    defer x.Close()
    if err != nil {
        return fmt.Errorf("failed to do something: %+v", err)
    }
    y, err := doSomethingElse(x)
    if err != nil {
        return fmt.Errorf("failed to do something else: %+v", err)
    }
    // ... and so on
}
```

You can call that `mainNoExit()` in any way you like. Here are some other options:

* `mainImpl()`
* `appMain()`
* move it to another package and call it `otherpkg.Main()`

As a bonus, you get a function (mainNoExit) that is far easier to test than the original main.

If your program needs to exit with different exit codes, consider this:

```go
func main() {
    if err, exitCode := mainNoExit(); err != nil {
        log.Printf("error: %+v", err)
        os.Exit(exitCode)
    }
}

// Note: mainNoExit returns 2 values now.
func mainNoExit() (error, int) {
    x, err := doSomething()
    defer x.Close()
    if err != nil {
        return fmt.Errorf("failed to do something: %+v", err), 1
    }
    y, err := doSomethingElse(x)
    if err != nil {
        return fmt.Errorf("failed to do something else: %+v", err), 1
    }
    // ... and so on
}
```

If you're using some CLI framework, it can still be possible to decompose the logic a little bit and avoid spreading the baddies across your code.

Let's suppose that we're using [github.com/cespare/subcmd](https://github.com/cespare/subcmd) package. The signature for a subcommand is `func ([]string)`.

We need a wrapper that would provide us the interface we want. It could be a manual function wrapping, a wrapper framework, or a function factory. Choose your poison.

I'll use a manual function wrapping here.

```go
func main() {
    log.SetFlags(0)

    cmds := []subcmd.Command{
        {
            Name:        "bench",
            Description: "run benchmark tests",
            Do:          benchMain,
        },

        // ... and so on
    }

    subcmd.Run(cmds)
}

func benchMain(args []string) {
    if err := cmdBench(args); err != nil {
        log.Fatalf("bench: error: %v", err)
    }
}

func cmdBench(args []string) error {
    // Actual implementation...
}
```

The [go-critic](https://github.com/go-critic/go-critic) static analyzer can detect some "exit after defer" cases. There is a [go-critic#issue1022](https://github.com/go-critic/go-critic/issues/1022) that raises the topic we're discussing here.

A long story short, using the single exit pattern can help you to avoid some confusing edge cases that make the static analyzers go crazy.

Let me re-iterate why having a single point of exit is a good thing:

* It leads to a better code structure. Easier to decompose and move the code around.

* Your main package may suddenly become easier to test.

* The program flow becomes simpler.

* Static analyzers will thank you.

* Less `log.Fatal()` things that are [bad](https://quasilyte.dev/blog/post/log-fatal-vs-log-panic/).
