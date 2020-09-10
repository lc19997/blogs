+++
date = "Thu Sep 10 22:01:24 MSK 2020"
title = "YAML is your JSON5"
tags = [
    "[yaml5]",
    "[yaml]",
    "[json5]",
    "[shortread]",
]
description = "Let me tell you about the YAML5 idea."
draft = false
+++

TL;DR:

* Write YAML as [JSON5](https://json5.org/) (but use `#` for comments)
* Enforce this style with [yaml5 lint](https://github.com/quasilyte/yaml5)

```js
# JSON5? YAML?
{a: 1, b: ['x', 'y']}
```

What you see above is a valid [YAML 1.2](https://yaml.org/spec/1.2/spec.html) document.<br>
It uses the flow-style syntax for objects and arrays.

If you look at the [JSON5](https://json5.org/) feature list, you can deduce that YAML is a superset of JSON5.<br>
The only difference is the single-line comment syntax.

Yes, we can [go and re-write](https://github.com/go-critic/go-critic/pull/966) all YAML files in a JSON5 style.

[YAML5](https://github.com/quasilyte/yaml5) is the "JSON with comments and trailing commas" that some of us were waiting for. Just take YAML and write it like JSON5, the `yaml5 lint` tool can enforce our promise of not using any features outside of the JSON5 subset:

```bash
$ cat bad.yaml
foo:
  - a
  - key: val

$ yaml5 lint bad.yaml 
bad.yaml:1:4: used a key-value outside of an object
bad.yaml:2:3: use a flow array syntax [] instead
bad.yaml:2:5: unquoted strings are not allowed
bad.yaml:3:8: used a key-value outside of an object
bad.yaml:3:10: infinity value should not be used
```

I'm planning to implement `yaml5 fmt` tool that would pretty-print a YAML document as YAML5 document.
