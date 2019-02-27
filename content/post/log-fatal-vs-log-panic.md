+++
date = "Tue Jan  8 01:46:18 MSK 2019"
title = "log.Fatal vs log.Panic"
tags = [
    "[shortread]",
    "[go]",
]
description = "tl;dr: avoid `os.Exit` near deferred calls."
draft = false
+++

> Update: probably the best alternative to `log.Fatalf` inside `main` is `log.Printf` followed by a return statement. If your main function contains a lot of such exit points, consider using a [step driven evaluation](https://quasilyte.github.io/blog/post/step-pattern/) pattern.

I personally don't like `log.Fatal/Fatalf/Fatalln`. I feel sad because of their ubiquity in Go examples as a form of reaction to an error. I personally prefer `log.Panic`.

The `log.Panic` vs `log.Fatal` is essentially `panic` vs `os.Exit(1)`. The latter will not let deferred calls to run. Most of the time, it's not what you want. It also makes testing much harder. It's quite simple to stop panic in test by the means of `recover`. It's much harder to cope with a code that does `os.Exit` somewhere while you're trying to do end2end testing without loosing coverage info.

I have written `exitAfterDefer` check for the [go-critic](https://github.com/go-critic/go-critic) linter. It warns about `log.Fatal` if the function where it's used deferred any calls prior to that line.

For example, that check would generate a warning for this kind of code:

```go
defer os.Remove(filename)
if err != nil {
    log.Fatalf("error: %v", err)
}
```

> warning: log.Fatalf clutters `defer os.Remove(filename)`

If you can, avoid `log.Fatal`. Don't reject `log.Panic` just because most people follow "don't panic" mantra too heavily. Exit is worse that panic, period. If you have a choice, choose something that does less potential damage.

So, you know what to do now:

```diff
if err != nil {
-   log.Fatalf("ooops: %v", err)
+   log.Panicf("ooops: %v", err)
}
```