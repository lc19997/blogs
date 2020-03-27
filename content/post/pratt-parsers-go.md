+++
date = "Fri Mar 27 17:08:04 MSK 2020"
title = "Let's Go write Pratt parsers!"
tags = [
    "[go]",
    "[habr-translation]",
    "[compilers]",
]
description = "Learn how to write Pratt parsers in Go."
draft = false
+++

<center>If you found a typo or a misspelling, please [file an issue](https://github.com/quasilyte/blog-src/issues/new) or send a PR that fixes it.</center>

![](https://hsto.org/webt/gi/eg/ji/giegjidcnqpsbkpiwlbrj9evce4.png)

> Original (ru): https://habr.com/ru/post/494316/.

[Recursive descent parsing](https://en.wikipedia.org/wiki/Recursive_descent) works well when you can continue parsing using the current context and a given token.

Some [expressions](https://en.wikipedia.org/wiki/Expression_(computer_science)) make parsing harder: postfix, infix and other. They introduce a problem: you don't know which kind of expression you're processing until you've already parsed half of it. Most of the time you also care about [operations precedence](https://en.wikipedia.org/wiki/Order_of_operations) and their [associativity](https://en.wikipedia.org/wiki/Operator_associativity), so the result [AST](https://en.wikipedia.org/wiki/Abstract_syntax_tree) has a proper shape.

It's hard to handle infix expression without some extra hints for the recursive descent approach. We can view a [Pratt algorithm](https://en.wikipedia.org/wiki/Pratt_parser) as a missing part that makes parsing of any kinds of expressions simple.

In this article, we'll write a parser for a [Go](https://golang.org/) dialect (more on that dialect later). 

## Go++ and ++Go

Since this article is about parsing, we'll cut some corners for [lexical analysis](https://en.wikipedia.org/wiki/Lexical_analysis):

* [go/scanner](https://golang.org/pkg/go/scanner/) converts a text into a token stream
* [go/token](https://golang.org/pkg/go/token/) describes the token types that `scanner` produces

For convenience, we'll introduce our `Token` type to combine token tag and value:

```go
type Token struct {
	kind  token.Token // Token category (tag), INT/IDENT/ADD
	value string      // Associated textual value
}
```

The [scanner.Scanner](https://golang.org/pkg/go/scanner/#Scanner) will be wrapped in `lexer` with the following interface:

* `Peek()` - returns the next token without consuming it; `lookahead(0)`
* `Consume()` - returns the next token from a stream

As for the AST, we're not going to use [go/ast](https://golang.org/pkg/go/ast/) package.

Since Go does not have any right-associative operators, we'll make `<<` (bitwise shift left) operator right-associative for the demonstration purposes.

In normal Go, both increment and decrement are statements, not expressions. In our dialect, they'll be expressions. On top of that, there will be prefix increment and decrement expressions as well, so you can parse `Go++ + ++Go` successfully!

> Don't be confused when the text does not present all functions and type definitions. You can browse [the complete implementation](https://github.com/quasilyte/pratt-parsers-go) after you understand the concept itself.

## Handling prefix expressions

Our first goal is to parse something simple like variables and prefix operators.

This is how our AST types will look like:

```go
// exprNode is a closed sum type that includes all possible expressions.
type exprNode interface {
	expr()
}

type nameExpr struct {
	Value string
}

type prefixExpr struct {
	Op  token.Token // Operation type, e.g. '+' or '-'
	Arg exprNode    // Unary operation argument
}

func (e *nameExpr) expr()   {}
func (e *prefixExpr) expr() {}
```

The main parsing function, `parseExpr()`, can be implemented with a switch statement:

```go
func (p *exprParser) parseExpr() exprNode {
	tok := p.lexer.Consume()
	switch tok.kind {
	case token.IDENT:
		return p.parseName(tok)
	case token.ADD, token.SUB:
		return p.parsePrefixExpr(tok)
	case token.LPAREN:
		return p.parseParenExpr(tok)
	// ... and so on.
	}
}

func (p *exprParser) parseName(tok Token) exprNode {
	return &nameExpr{Value: tok.value}
}

func (p *exprParser) parsePrefixExpr(tok Token) exprNode {
	arg := p.parseExpr()
	return &prefixExpr{Op: tok.kind, Arg: arg}
}
```

This switch-based solution can end up being quite unmaintainable.

It's possible to put parsing methods like `parsePrefixExpr()` into a `map` or some other data structure that supports efficient indexing. It would be more efficient to use a slice (or array): tokens are usually dense and it's a rare case to have more than 256 kinds of tokens.

Since all parsing methods have the same signature, let's define a `prefixParselet` type for them:

```go
type prefixParselet func(Token) exprNode
```

Parser gets a new `map[token.Token]prefixParselet` field to store all prefix parselets. That table is initialized during the parser construction:

```go
func newExprParser() *exprParser {
	p := &exprParser{
		prefixParselets: make(map[token.Token]prefixParselet),
	}

	prefixExpr := func(kinds ...token.Token) {
		for _, kind := range kinds {
			p.prefixParselets[kind] = p.parsePrefixExpr
		}
	}

	p.prefixParselets[token.IDENT] = p.parseName
	prefixExpr(token.ADD, token.SUB)

	return p
}
```

`prefixExpr()` helper function makes it easier to add new prefix operators. It's possible to go further and make it more declarative (see more below).

```go
func (p *exprParser) parseExpr() exprNode {
  tok := p.lexer.consume()
  prefix, ok := p.prefixParselets[tok.kind]
  if !ok {
    // Parse error: unexpected token
  }
  return prefix(tok)
}
```

> The example above can handle errors in at least 2 ways. I tend to use `panic+recover` in parsers, but you can use `{exprNode, error}` return values and
  handle errors more traditionally. If you choose the `panic` approach, don't forget to `recover` inside your parser entry method and return
  parsing error as a value.

## Handling infix expressions

Let's try parsing `x+y` now.

The current implementation will return `nameExpr{Value:"x"}` and stop at the `+` token.

To handle that, a new `infixParselet` type will have an extra argument: an expression that was parsed before the infix parselet was called.

For the `x+y` case, that extra argument would be `x`.

```go
type infixParselet func(left exprNode, tok Token) exprNode
```

Just like with prefix parselets, we add a new `map` field to a parser.

We'll express binary operators like `+` and `-` with `binaryExpr` type:

```go
func (p *exprParser) parseBinaryExpr(left exprNode, tok token) exprNode {
	right := p.parseExpr()
	return &binaryExpr{Op: tok.kind, Left: left, Right: right}
}
```

The next step is to make `parseExpr()` method aware of infix parselets:

```go
func (p *exprParser) parseExpr() exprNode {
	tok := p.lexer.Consume()
	prefix, ok := p.prefixParselets[tok.kind]
	if !ok {
		// Parse error: unexpected token
	}
	left := prefix(tok)
	tok = p.lexer.Peek()
	infix, ok := p.infixParselets[tok.kind]
	if !ok {
		return left
	}
	p.lexer.Consume() // skip/discard for the previously peeked token
	return infix(left, tok)
}
```

This implementation has two problems:

1. All expressions are parsed as right-associative: `x-y-z` => `x-(y-z)`
2. All operations have identical precedence: `x*y+z` => `x*(y+z)`

## Handling precedence and associativity

To solve both problems, we need a `{token.Token => priority}` mapping. This mapping can be a global variable or a part of the parser state. To keep relevant pieces together, we'll store precedence tables inside the parser.

```go
func newExprParser() *exprParser {
	// ...
	p.prefixPrecedenceTab = map[token.Token]int{
		token.ADD: 4,
		token.SUB: 4,
	}
	p.infixPrecedenceTab = map[token.Token]int{
		token.ADD: 2,
		token.SUB: 2,
		token.MUL: 3,
		token.QUO: 3,
	}
	// ...
}
```

The final `parseExpr()` version takes `precedence` argument:

```go
func (p *exprParser) parseExpr(precedence int) exprNode {
	tok := p.lexer.Consume()
	prefix, ok := p.prefixParselets[tok.kind]
	if !ok {
		// Parse error: unexpected token
	}
	left := prefix(tok)

	for precedence < p.infixPrecedenceTab[p.lexer.Peek().kind] {
		tok := p.lexer.Consume()
		infix := p.infixParselets[tok.kind]
		left = infix(left, tok)
	}

	return left
}
```

With the help of a new argument, `parseExpr()` knows when to continue and when to stop. 

Every parselet now needs to pass its `precedence` during the `parseExpr()` call:

```go
func (p *exprParser) parseBinaryExpr(left exprNode, tok Token) exprNode {
	right := p.parseExpr(p.infixPrecedenceTab[tok.kind])
	return &binaryExpr{Op: tok.kind, Left: left, Right: right}
}
```

`parseBinaryExpr()` binds expressions as left-associative. To have right-associative parsing, just subtract 1 from the operation priority:

```go
func (p *exprParser) rparseBinaryExpr(left exprNode, tok Token) exprNode {
	right := p.parseExpr(p.infixPrecedenceTab[tok.kind] - 1)
	return &binaryExpr{Op: tok.kind, Left: left, Right: right}
}
```

We defined `<<` as right-associative operation, this is why we'll use `rparseBinaryExpr()` to handle it.

## Parsing the rest

A function call is `infixParselet` that handles `'('` token and collets all arguments with `parseExpr(0)` until it reaches `')'` token.

```go
func (p *exprParser) parseCallExpr(left exprNode, tok Token) exprNode {
	if p.lexer.Peek().kind == token.RPAREN {
		// A call without arguments.
		p.lexer.Consume()
		return &callExpr{fn: left}
	}

	var args []exprNode
	for {
		args = append(args, p.parseExpr(0))
		if p.lexer.Peek().kind != token.COMMA {
			break
		}
		p.lexer.Consume()
	}
	p.expect(token.RPAREN)
	return &callExpr{fn: left, args: args}
}
```

Grouping operator (parentheses) is `prefixParselet` that handles `'('` token and a single argument with `parseExpr(0)`, then it expects `')'` token.

```go
func (p *exprParser) parseParenExpr(tok Token) exprNode {
	x := p.parseExpr(0)
	p.expect(token.RPAREN)
	return x
}
```

Any postfix operation is a very simple `infixParselet`:

```go
func (p *exprParser) parsePostfixExpr(left exprNode, tok Token) exprNode {
	return &postfixExpr{Op: tok.kind, Arg: left}
}
```

## The final moves

As with everything else, there is some room for improvement.

For example, instead of filling a precedence table separately, one can add a `precedence` argument to helper functions inside the parser constructor:

```go
prefixExpr := func(precedence int, kinds ...token.Token) {
	for _, kind := range kinds {
		p.prefixParselets[kind] = p.parsePrefixExpr
		p.prefixPrecedenceTab[kind] = precedence
	}
}
```

With this, we can re-write the initialization code:

```go
prefixExpr(6,
	token.ADD,
	token.SUB,
	token.INC,
	token.DEC,
)
postfixExpr(7,
	token.INC,
	token.DEC,
)
leftAssocBinaryExpr(3,
	token.ADD,
	token.SUB,
)
leftAssocBinaryExpr(4,
	token.MUL,
	token.QUO,
	token.REM,
)
rightAssocBinaryExpr(3,
	token.SHL,
)
```

Magic constants can be replaced with named precedence groups, like `PrecAdd=3` and `PrecMult=4`.

To make parser creation less expensive, you can initialize a "grammar" object separately and then pass it into `newExprParser`. You'll need to change `prefixParselet` and `infixParselet` signatures to include a new argument - `*exprParser`.

Separate precedence tables can be avoided with a structure that holds both a parselet and associated operation priority. It's also possible to make both parselets an `interface` and define a separate type for every parselet kind.

## Conclusion

Pratt parsers became my favorite way to build hand-written parsers. I wanted to write a simple example in Go and share it with the world for quite a long time now. I finally did it, I guess.

The final parser version can be found at [github.com/quasilyte/pratt-parsers-go](https://github.com/quasilyte/pratt-parsers-go).

This article is heavily inspired by [Pratt Parsers: Expression Parsing Made Easy](https://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/) (Bob Nystrom). I strongly recommend you to read the original article as well. It has a better wording and examples are in Java (if you're into that).
