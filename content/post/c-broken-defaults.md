+++
date = "2016-12-26"
title = "C broken defaults"
tags = [
    "[c]",
    "[language design]",
    "[rants]",
]
description = "Trying to enumerate what C defaults are wrong."
draft = false
+++

## State of the C

C fits its niche quite well.
If you want relatively simple, ubiquitous and efficient language
there is no much room for selection.

It "evolves so slowly" because it is already quite complete.
Most of the parts that can be improved without making C
yet another bloated language require breaking changes.

> C could be designed better if we accept older code invalidation.

...but in reallity it is impossible to achieve.
If you are using C, you must know many of its quirks,
use external static code analyzers and read carefully
a lots of 
[safe coding standards](https://www.securecoding.cert.org/confluence/display/c/SEI+CERT+C+Coding+Standard).

This post describes subjects that I believe should be
changed in order to get a better language.
Note that C is mostly unsafe by design;
it trusts programmer nearly as much as assemblers do.
The main target is not making C higher level, but rather
reconsider the defaults and make best practice enforcements easier.

## Mutability defaults

Programming language should force you to think about
your code more thoroughly. Whenever there is a choice,
the most safe and strict choice should be favoured for a default.

Mutable state must have explicit eye-catcher.
We generally should care more about marking potentially
tricky code rather than const-correct code 
[Rust language](https://www.rust-lang.org) also takes this approach).

> All variables and aggregate type members 
> should be immutable by default.

```c
typedef struct Str Str;
struct Str {
  mut char* data;
  mut size_t len;
};

// Both arguments are "const Str*"
bool str_eq(Str*, Str*); 

// Mutable pointers marked so explicitly
void str_copy(Str* dst, mut Str* src);
```

Compiler should warn if the variable marked as mutable,
but needs not to be. 

## Tag names

There could be a rationale for separate "namespace" for user-defined
types like structs, unions and enums.
C has no real namespaces, so if we put everything into single
symbol table it will bloat and compilation time can increase
marginally. 

Everything is fine except that 90% of people instantly typedef
anonymous structs. Or, if they want to be able to forward
declare it inside other header, 
[smarter typedef is done](http://www.embedded.com/electronics-blogs/programming-pointers/4024450/Tag-vs-Type-Names).
This leads us to registering same symbol inside two tables.
Not only this is not convenient, it is also inefficient.

> Tag symbols for type names = mistake.

## Multiple variable declarations

Multiple declarations on the same line do not improve code readability.
C is not about typing fewer keywords => no real gain in using this syntax.
They can also be a source of confusion for amateurs (when they define
both pointer and non-pointer variables of type T).

> Declaring multiple variables in one statement should be forbidden.

## Builtin types

More builtin primitive types would be convenient.
Language would feel more coherent if things like size_t,
int32_t and bool were builtin.
Currently, we must include at least 3 headers to have most
useful primitive types: "stddef.h" for size_t,
"stdint.h" for fixed width types and "stdbool.h" to
avoid ugly _Bool. Predefined NULL of special type
would be great as well, but this is C++-ism.

Talking about breaking existing code, I prefer int32 as a type name
opposed to int32_t.

> Most useful primitive types should be builtin.

## Array decay

If you want to pass an "array" of known length,
[C provides no help for that](http://www.drdobbs.com/architecture-and-design/cs-biggest-mistake/228701625).
You can try something like [Cello](http://libcello.org/learn/a-fat-pointer-library) 
to fix this, but then you lose an ability
to pass sub-arrays without copying (address plus offset). 

Most projects I have ever seen define some kind of "fat pointer"
structure. That is, structure of `{void*, size_t}`.
The problem is: this structure is vital, universal and useful,
but it is missing from the standard library =>
every project defines their own fat pointer.
I demand "stdarray.h".

Every homebrew array is incompatible with someone else's array. 
We end up with two kinds of APIs as a result:
one which expects two separate arguments
for data and its length and another which exposes custom array type.

As an addition, you will most likely need `{void*, size_t, size_t}` 
structure to express fixed-size container that is partially filled.
This is essential to build extendable arrays (C++ calls them vectors).
There are many useful fundamental data structures, but we need 
to start from something. Array seems like a good and easy first step.

> Arrays with length must be better supported by the language.

## Aliasing defaults

Additional pointer qualifier is needed to make aliasing 
possible only with explicit marker.
If scope has more than one non-const `T*` then it 
should be marked either `alias` or `restrict`.
Abscence of qualifier is an error.

If pointers have different type, `restrict` is 
implied, but this can be redefined by explicit `alias`.
This is needed to avoid breaking of 
[strict aliasing](http://blog.regehr.org/archives/1307) rules.

Alias takes one or more arguments that specify what
pointer could be aliased. If `a` aliases `b`, then
`b` gets implicit `alias(a)` qualifier.

> There should be more "restrict" 
> and const pointers than mutable and/or aliased pointers.

```c
void copy(char* restrict dst, char* restrict src);
void move(char* alias(src) dst, char* src);
```

```c
// If we specify dst as const, no need to mark
// other pointer as restrict or alias.
void copy(const char* dst, char* src);
// But in case of move we want to pass aliased 
// pointers sometimes.
void move(const alias(src) char* dst, char* src);
```

`alias` is choosed as a keyword because GCC already have 
similar attribute 
[may_alias](https://gcc.gnu.org/onlinedocs/gcc-4.0.2/gcc/Type-Attributes.html).

## Statement-orientation

Expression-oriented languages are simply put, more expressive.
There is no runtime cost because compiler can easily determine
whenever particular construct is used inside lvalue context.

> Expression-oriented is better than statement-oriented.

But there is an important exception:

> Assignments should be statements, not expressions.

```c
void f(ErrorCode code) {
  puts(switch_expr (code) {
    case E_FOO: "foo error!";
    case E_BAR: "bar error!";
    default: "unknown error!";
  });
}
```

One can argue that you can define separate function which
uses same switch, but returns necessary value.
This helps to avoid ugly "break", but introduces a new function.
Other solution is to use 
[conditional operator](https://en.wikipedia.org/wiki/%3F).
When formatted properly, it emulates "case" expression well.
Too bad I have yet to see a compiler that checks controlling
expressions to be in sequential order (like enum constants)
to perform optimizations akin to switch.

## Constness erasure

In modern code, casting away cv-qualifier is almost always a bad idea.
Potentially, it can lead to undefined behavior.
As long as const can be casted away, compiler can not make
strong assumptions about it. Again, this affects both hypothetical
perfomance and overall language safety.

> It should be impossible to cast away const quallifiers.

## Missing features

This section briefly describes controversial features
from my wishlist. Completely optional things.

Strict/strong typedefs were proposed for C++ more than once now. 
Check [this document](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3515.pdf).
C could benefit from type-checked typedefs,
but it can also lead to code pollution with casts if
used wildly. If you interested in making C code more
reliable via types, try 
[CQual](http://www.cs.umd.edu/~jfoster/cqual/) tool.

## To be continued

I have not yet covered: 

* dumb preprocessor;
* ambigious and clumsy syntax;
* inabillity to initialize global const data in non-trivial way
  at compile time;
* permitted duplicates in enum values;

...and some other things I dislike in C.

Updates are not promised, but possible.

