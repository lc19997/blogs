+++
date = "2017-09-23"
title = "Writing Emacs Lisp compiler intrinsics"
tags = [
    "[emacs lisp]",
    "[compilers]",
]
description = "Describes how to extend Emacs Lisp bytecode compiler."
draft = false
+++

## The problem

Given a particular programming task in Emacs Lisp, 
you almost always should implement it with `function`.
Sometimes, `macro` is a proper tool.  
Even more rarely, `cl-define-compiler-macro` may be your choice.

All these methods are safe to use. 
They almost certanly will not break with newer Emacs versions.

One interesting task that can not be implemented
efficiently with approaches that are outlined above is
imperative [return statement](https://en.wikipedia.org/wiki/Return_statement).

Emacs Lisp lacks this kind of flow control and
this is a problem we are goind to attack today.

All examples are done around Emacs Lisp [factorial](https://rosettacode.org/wiki/Factorial#Emacs_Lisp)
function that is re-written to use `return` form.

At the end of this article, `%return` intrinsic will be
developed, which takes a single expression and returns
evaluated result from the called subroutine.

## Quick glance at cl-return

Builtin `cl-lib` package exports `cl-return` and `cl-return-from` macros
that both doing the same thing. They use `throw` and expect
`catch` to exist; this means that `cl-block` is mandatory.

```lisp
(defun factorial/if (x)            (defun factorial/cl (x)
  (if (= x 1)                        (cl-block 'ret
      1                                (when (= x 1)
    (* x (factorial/if (1- x)))))        (cl-return-from 'ret 1))
                                       (* x (factorial/cl (1- x)))))
                                  
(disassemble 'factorial/if)        (disassemble 'factorial/cl)
;; 0       dup                     ;; 0       constant  --cl-block-\(quote\ ret\)--
;; 1       constant  1             ;; 1       pushcatch 2
;; 2       eqlsign                 ;; 4       dup  
;; 3       goto-if-nil 1           ;; 5       constant  1
;; 6       constant  1             ;; 6       eqlsign 
;; 7       return                  ;; 7       goto-if-nil 1
;; 8:1     dup                     ;; 10      constant  throw
;; 9       constant  factorial/if  ;; 11      constant  --cl-block-\(quote\ ret\)--
;; 10      stack-ref 2             ;; 12      constant  1
;; 11      sub1                    ;; 13      call      2
;; 12      call      1             ;; 14      discard   
;; 13      mult                    ;; 15:1    dup 
;; 14      return                  ;; 16      constant  factorial/cl
;;                                 ;; 17      stack-ref 2
;;                                 ;; 18      sub1      
;;                                 ;; 19      call      1
;;                                 ;; 20      mult      
;;                                 ;; 21      pophandler 
;;                                 ;; 22:2    return     
```

While visually looking acceptable, 
this solution does not satisfy performance requirements.  
It **is** slower than idiomatic `if` solution.

Did you noticed that compiled `factorial/if` has two return points?
Return functionality is already there, we just have no any way to express it
at the source code level.

## Ways to control byte compilation

Our goal is to expand `(%return X)` into `X` evaluation plus
`return` instruction that follows it.

At least two ways exist:

1. [Advise](https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html) `byte-compile-form` function via `advice-add` (or [deprecated `defadvice`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Porting-old-advice.html#Porting-old-advice)).
2. Use `byte-defop-compiler` macro. Our choice.

First approach is useful if your handler should be executed **before**
`byte-compile-form`, and/or independently from it.

With second approach, handler is executed only when matching form
or instruction is being compiled.

The main source of information about compiler internals is  
`$EMACS/lisp/emacs-lisp/bytecomp.el`.  
The most important functions have useful documentation. 

## Compiler handler

`(defmacro byte-defop-compiler (function &optional compile-handler) ...)`

> Add a compiler-form for FUNCTION.
> If function is a symbol, then the variable "byte-SYMBOL" must name
> the opcode to be used.  **If function is a list**, the **first element
> is the function** and the second element is the bytecode-symbol.
> **The second element may be nil, meaning there is no opcode**.
> **COMPILE-HANDLER is the function to use to compile this byte-op**.

```lisp      
(byte-defop-compiler (%return nil) %byte-compile-return)
;;                    ^       ^    ^
;;                    |       |    |
;;                    |       |    COMPILE-HANDLER: handler itself
;;                    |       FUNCTION: second element - no opcode
;;                    FUNCTION: first element - function to match

;; The `byte-defop-compiler-1' can be used for convenience.
;; It expands to the same form as above.
(byte-defop-compiler-1 %return %byte-compile-return)
```

The `compile-handler` function receives one argument - a list
that represents matched form.
In other words, all invocations of `%return` function are
passed into that handler.

```lisp
;; `form' argument may be `(%return X)' or
;; any other list that has `%return' in head position.
(defun %byte-compile-return (form)
  ;; (1)
  (unless byte-compile--for-effect
    (error "Should not use `%%return' result"))
  ;; (2)
  (setq byte-compile--for-effect nil)
  (pcase form
    (`(%return ,v)
     ;; Eval return argument and emit "return" instruction.
     ;; Note that `%byte-compile-out' is used,
     ;; not `byte-compile-out'; see (3).
     (byte-compile-form v)
     (%byte-compile-out 'byte-return))
    (_ (error "Invalid return form: %S" form))))
```

(1) In Emacs Lisp, every form yields result, the return value.
When result is not used, it is sayed that expression is evaluated
for side-effects only. This detail is important for byte compiler,
because it should emit `discard` instruction when result is ignored.

```lisp
;; Case 1: `aset' is used for side-effects only.
(progn
  (aset arr idx val)
  :res)
;; 0       varref    arr  | [arr]
;; 1       varref    idx  | [arr idx]
;; 2       varref    val  | [arr idx val]
;; 3       aset           | [val]
;; 4       discard        | []
;; 5       constant  :res | [:res]
;; 6       return         | :res returned

;; Case 2: `aset' return value is used.
(progn
  (aset arr idx val))
;; 0       varref    arr | [arr]
;; 1       varref    idx | [arr idx]
;; 2       varref    val | [arr idx val]
;; 3       aset          | [val]
;; 4       return        | val is returned
```

The `byte-compile--for-effect` is all about that.
If it is non-nil, expression result is used in some way.
We do not want `%return` to be used as an expression,
this is why that guard clause exists.
In particular, `(setq x (%return y))` is impossible, thanks
to that compile-time check.

`byte-compile--for-effect` is dynamically scoped (special) variable.
It is set inside `byte-compile-form` to the value of it's `for-effect` argument.

(2) From `byte-compile-form` documentation:

> If for-effect is non-nil, byte-compile-form will output a byte-discard
> before terminating (ie no value will be left on the stack).
> A byte-compile handler may, when byte-compile--for-effect is non-nil, choose
> output code which does not leave a value on the stack, and then set
> byte-compile--for-effect to nil (to prevent byte-compile-form from
> outputting the byte-discard).

It describes the reason why `%byte-compile-return` sets
`byte-compile--for-effect` to `nil`.
We do not want extra `discard` after `%return`,
it will never be executed anyway.

(3) Look at the `byte-compile-out` closely:

```lisp
(defun byte-compile-out (op &optional operand)
  (push (cons op operand) byte-compile-output)
  (if (eq op 'byte-return)
      ;; This is actually an unnecessary case, because there should be no
      ;; more ops behind byte-return.
      (setq byte-compile-depth nil)
    (setq byte-compile-depth
	  (+ byte-compile-depth (byte-compile-stack-adjustment op operand)))
    (setq byte-compile-maxdepth (max byte-compile-depth byte-compile-maxdepth))))
```

It **specifically** checks for `byte-return` that we wish to emit.
`%byte-compile-out` is essentialy a copy of `byte-compile-out`, but without
unwanted check:

```lisp
(defun %byte-compile-out (op &optional arg)
  (push (cons op arg) byte-compile-output)
  (setq byte-compile-depth
        (+ byte-compile-depth (byte-compile-stack-adjustment op arg)))
  (setq byte-compile-maxdepth (max byte-compile-depth byte-compile-maxdepth)))
```

## Trying out new intrinsic

```lisp
(defun factorial/if (x)            (defun factorial/ret (x)
  (if (= x 1)                        (when (= x 1)
      1                                (%return 1))
    (* x (factorial/if (1- x)))))    (* x (factorial/ret (1- x))))                
                                 
(disassemble 'factorial/if)        (disassemble 'factorial/ret)     
;; 0       dup                     ;; 0       dup       
;; 1       constant  1             ;; 1       constant  1
;; 2       eqlsign                 ;; 2       eqlsign   
;; 3       goto-if-nil 1           ;; 3       goto-if-nil 1
;; 6       constant  1             ;; 6       constant  1
;; 7       return                  ;; 7       return    
;; 8:1     dup                     ;; 8:1     dup       
;; 9       constant  factorial/if  ;; 9       constant  factorial/ret
;; 10      stack-ref 2             ;; 10      stack-ref 2
;; 11      sub1                    ;; 11      sub1      
;; 12      call      1             ;; 12      call      1
;; 13      mult                    ;; 13      mult      
;; 14      return                  ;; 14      return    
```

In the end, we get exactly the same byte code output.
This would not be possible without compiler tweaking.

Strictly speaking, you can not use `defun` and get `%return` like that,
a function must be **compiled** in order to trigger compiler handler,
otherwise you will get `Symbol’s function definition is void: %return` error.

You can define a dedicated `%defun` macro that always does byte compilation.
The simplified version can look like this:

```lisp
(defmacro %defun (name params &rest body)
  (declare (indent defun))
  `(defalias ',name
     (byte-compile
      (lambda ,params
        ,@body))))
```

With such `%defun`, users can get `%return` transparently,
no explicit compilation is needed.

If are looking for more practical examples where `return` is useful,
check out [bool-listp.el](/blog/code/bool-listp.el).
