+++
date = "Wed Feb 27 22:15:32 MSK 2019"
title = "Step driven evaluation"
tags = [
    "[shortread]",
    "[go]",
]
description = "A pattern for writing multi-step programs."
draft = false
+++

If you heard about [table driven tests](https://github.com/golang/go/wiki/TableDrivenTests), the idea described in this article will be easier to grasp, since it's the same technique, but used outside of the tests.

Suppose you have a function that executes a lot of other functions. This function probably does two main things:

1. It checks for all returned errors as they occur.
2. It passes one function outputs as the inputs for another.

```go
// process is an example pipeline-like function.
func queryFile(filename, queryText string) (string, error) {
	data, err := readData(filename)
	if err != nil {
		return nil, errors.Errorf("read data: %v", err)
	}
	rows, err := splitData(data)
	if err != nil {
		return nil, errors.Errorf("split data: %v", err)
	}
	q, err := compileQuery(queryText)
	if err != nil {
		return nil, errors.Errorf("compile query: %v", err)
	}
	rows, err = filterRows(rows, q)
	if err != nil {
		return nil, errors.Errorf("filter rows: %v", err)
	}
	result, err := rowsToString(rows)
	if err != nil {
		return nil, errors.Errorf("rows to string: %v", err)
	}
	return result, nil
}
```

This function consists of 5 steps. Five relevant calls, to be precise. Everything else is a distraction. The order of those calls matter, it's a sequence, the algorithm.

Let's re-write code above using the step driven evaluation.

```go
func queryFile(filename, queryText string) ([]row, error) {
	var ctx queryFileContext
	steps := []struct {
		name string
		fn   func() error
	}{
		{"read data", ctx.readData},
		{"split data", ctx.splitData},
		{"compile query", ctx.compileQuery},
		{"filter rows", ctx.filterRows},
		{"rows to string", ctx.rowsToString},
	}
	for _, step := range steps {
		if err := step.fn(); err != nil {
			return errors.Errorf("%s: %v", step.name, err)
		}
	}
	return ctx.result
}
```

The pipeline is now explicit, it's easier to adjust steps order and to insert or remove them. It is also trivial to add debug logging inside that loop, you need only one new statement as opposed to `N` statements near every function call.

This approach shines with 4+ step, when the complexity of introducing a new type like `queryFileContext` is inferior to the benefits.

```go
// queryFileContext might look like the struct below.

type queryFileContext struct {
	data   []byte
	rows   []row
	q      *query
	result string
}
```

Methods like `queryFileContext.splitData` just call the same function while updating the `ctx` object state.

```go
func (ctx *queryFileContext) splitData() error {
	var err error
	ctx.rows, err = splitData(ctx.data)
	return err
}
```

This pattern works particularly well for `main` functions.

```go
func main() {
	ctx := &context{}

	steps := []struct {
		name string
		fn   func() error
	}{
		{"parse flags", ctx.parseFlags},
		{"read schema", ctx.readSchema},
		{"dump schema", ctx.dumpSchema}, // Before transformations
		{"remove builtin constructors", ctx.removeBuiltinConstructors},
		{"add adhoc constructors", ctx.addAdhocConstructors},
		{"validate schema", ctx.validateSchema},
		{"decompose arrays", ctx.decomposeArrays},
		{"replace arrays", ctx.replaceArrays},
		{"resolve generics", ctx.resolveGenerics},
		{"dump schema", ctx.dumpSchema}, // After transformations
		{"decode combinators", ctx.decodeCombinators},
		{"dump decoded combinators", ctx.dumpDecodedCombinators},
		{"codegen", ctx.codegen},
	}

	for _, step := range steps {
		ctx.debugf("start %s step", step.name)
		if err := step.fn(); err != nil {
			log.Fatalf("%s: %v", step.name, err)
		}
	}
}
```

An additional benefit is the ease of testing. Even though we use `log.Fatalf`, [which is a bad thing](https://quasilyte.github.io/blog/post/log-fatal-vs-log-panic/), it's trivial to re-create this pipeline inside a test and run a set of steps that fail a test instead of doing `os.Exit`.

You can also omit some CLI-related steps inside tests, like `"dump schema"` or `"codegen"`. You can also inject test-specific steps into that list.

There are few drawbacks, as always:

1. You need to introduce a new type and probably a few methods for it.
2. It's not always straightforward to figure out appropriate context object layout so
   it satisfies the needs of the entire pipeline without getting overly complex.

Try using it, maybe you'll like it.
