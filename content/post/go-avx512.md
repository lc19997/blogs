+++
date = "Fri Jun  8 13:46:49 MSK 2018"
title = "Go AVX-512 support"
tags = [
    "[go]",
    "[asm]",
    "[avx-512]",
]
description = "Your guide in Go AVX-512 world. Links to docs, articles and so on."
draft = false
+++

## About this document

This article is going to be up-to-date source of AVX-512 information in Go context.  
It references other posts, official documentation and other useful resources.

It's short. By purpose.  
Only English content is referenced (original + translated).

## Documentation

* [AVX-512 support in Go assembler](https://software.intel.com/en-us/articles/avx-512-support-in-go-assembler):
  short reference that focuses on Go-specific implementation of AVX-512.
  It describes how to use all AVX-512 special features in Go assembly syntax as well as some encoder details.
  Contains some examples.

* [Disassembling Go AVX-512](/post/disassembling-go-avx512):
  how to disassemble and inspect AVX-512 machine code (given that `go tool objdump` can't do it).
