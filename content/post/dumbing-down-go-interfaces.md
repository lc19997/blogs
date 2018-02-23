+++
date = "2017-04-26"
title = "Dumbed-down Go interfaces"
tags = [
    "[go]",
    "[golang]",
    "[error handling]",
]
description = "Making verbose code simpler by wrapping interface methods."
draft = true
+++

## io.Writer

If you write Go, odds are high that you familiar with
`io.Writer#Write` signature well. 
Write returns both error and number of bytes written.
Every time you call this method, 
error should be checked before you 
try to write to that writer again.

This interface is inconvenient to 
use when you want to call `Write` multiple times and get 
total number of bytes pushed during these invocations.
You also do not want to loose error if it ever occurs.

```go
func f1(w io.Writer, s fmt.Stringer, parts [][]byte) (int, error) {
	written, err := w.Write([]byte(s.String()))
	if err != nil {
		return written, err
	}
	for _, part := range parts {
		n, err := w.Write(part)
		written += n
		if err != nil {
			return written, err
		}
	}
	return written, nil
}
```

The boilerplate of error propagation can be annoying,
Go has no things like [try!](https://doc.rust-lang.org/std/macro.try.html)
in Rust, but it does not mean that there is no solutions.

## Dumbing it down

Code presented above can be considered idiomatic.
Error checking is explicit and in-place (near error occurrence).
It is OK when you have small amount of `f1`-like function,
there is nothing wrong in small code duplication.
When you reach your limit of DRY violation, consider
possible changes. 

Except writing, we do two things in that function: 
counting bytes and checking for errors. 
To dumb down interface we need to wrap it in a type 
that does additional job for us.

I will present two examples that simplify `f1` function.
First `f2` does not sacrifice anything but improvement is 
slight. Second `f3` is more expressive, 
but may not be always appropriate.

## Without manual counting

If errors are more important than counting, we may wish to hide 
obscuring code to concentrate on error handling.

```go
type countingWriter struct {
	written int       // Accumulates 1st return value of Write.
	dst     io.Writer // Wrapped writer.
}

func (cw *countingWriter) WriteAndCount(p []byte) error {
	n, err := cw.dst.Write(p)
	cw.written += n
	return err
}
```

The benefits are: 
less varibles in scope (no need for temporary `n` 
and `written` accumulator), single return value 
makes it easier to compose calls. 

```go
func f2(w io.Writer, s fmt.Stringer, parts [][]byte) (int, error) {
	cw := countingWriter{dst: w}
	err := cw.WriteAndCount([]byte(s.String()))
	if err != nil {
		return cw.written, err
	}
	for _, part := range parts {
		err := cw.WriteAndCount(part)
		if err != nil {
			return cw.written, err
		}
	}
	return cw.written, nil
}
```

## Without eager error handling

Explicit error handling in `f1`, 
after all, is not important.
That function does not try to fix error conditions, 
it just passes them back to the caller. 
In the end, `f1` does only one important thing: 
it writes.
It is possible to make a write method that returns... nothing.
  
```go
type safeWriter struct {
	err     error     // Error that occured during writing.
	written int       // Bytes written before the error occured.
	dst     io.Writer // Wrapped writer.
}

func (w *safeWriter) SafeWrite(p []byte) {
	if w.err != nil {
		return
	}
	n, err := w.dst.Write(p)
	w.err = err
	w.written += n
}
```

Note that even if we do not return an error, 
we store it in the safeWriter. 
When first error occurs, safeWriter will continue to look like 
before, but consequent write calls are ignored.
After all operations we wish to execute are made, 
we return an error (which can be nil) and 
a total number of bytes written.

```go
func f3(w io.Writer, s fmt.Stringer, parts [][]byte) (int, error) {
	sw := safeWriter{dst: w}
	sw.SafeWrite([]byte(s.String()))
	for _, part := range parts {
		sw.SafeWrite(part)
	}
	return sw.written, sw.err
}
```

> Not all errors require immediate handling, we use that fact in
> safeWriter.

## Not only io.Writer

`io.Writer` was selected to show this wrapping technique
on concrete example, but it can be applied to any methods
that clobber code because of the contract it establish.

For `f1` it could be better to define `writeAll` that 
takes `[][]byte` and does the looping inside.
It works, but the solution is less generic than `f3`
and probably less composable. The balance of 
**too concrete** and **over-abstract** code is hard, so the decision
is always halfway subjective.
