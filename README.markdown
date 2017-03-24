# Rosa - text labeling language

[![Quicklisp](http://quickdocs.org/badge/rosa.svg)](http://quickdocs.org/rosa/)
[![Build Status](https://travis-ci.org/t-sin/rosa.svg)](https://travis-ci.org/t-sin/rosa)

Rosa is a text labeling language.

Or for Japanese, see [this article](http://octahedron.hatenablog.jp/entry/2017/03/24/011008)

> Stat rosa pristina nomine, nomina nuda tenemus.



## Basis


Rosa is a language to attach meta data on text block.
Text file written in rosa can be regarded as like a multi-value key-value data.

Here, a pair in rosa data structure, it consist of just one **label** and multiple **bodies**.
**Bodies** are ordered by appearance from head of text data.
Because of it, array of **bodies** is simply called **body**.

**Label** is a meta data of **body**.
It is a string represented as regex `[a-z][a-z0-9-]*`.

**Body** is a value of **label**.
It is a array of strings.
Note that each strings in **body** can include newline.


## Syntax

Rosa has an ASCII-based and minimal syntax.
Here briefly describe rosa syntax.
For BNF definition, see `SYNTAX.md`.

Rosa treats two characters at line head as special: `:` and `;`.
**Label** line starts with colon (`:`).
**Comment** line starts with semicolon (`;`).


### Labels

Syntactically, these are two kind of **label**, distinguished by if the **body** includes newline.

If the **body** has no newline, **label** called *inline*.
Otherwise, **label** called *block*.

When the *label line* has space, it is regarded as *inline* label.
**Label** and **body** separated with first appeared space (` `).
If the line has no space, it is called **block**.
**Label** and **body** separated with first appeared newline.
For instance, see below how to write two kind of labels:

```
:label1 this is first body of inline label
:label2

this is a body of block label.

:label1 this is second body of inline label
```

Twice or more appearance of **label** is allowed, because of each **body** stored in one array with appearance order.


### Comments

Comments can be used.
It's start with `;`.

An example bellow:

```
:text1

This line is visible.
; This line is invisible! Whoa!
This line is read.
```


### Escape sequences

To denotes colon and semicolon themselves at line head, use escape sequence.

For colon, this is it: `::`.
For semicolon, this is it: `:;`.

An example bellow:

```
:text1

:: is read as ':'
:; is read as ';'
```


## Usage

Rosa has a CLI front-end for parse rosa markup language.
`rosa` is it.

`rosa` can do three things:

- listing labels in input
- picking body up from input
- dumping entire key-value structure from input

Here is synopsis for `rosa`:

```
$ rosa SUBCOMMAND [OPTIONS] [PARAMETERS] [FILE]
```

If FILE is not supplied, rosa reads from standard input.


### Subcommands

Subcommands has its shorthand; the first letter.
For instance, shorthand of `index` is `i`.


#### Listing labels

```
$ rosa index [OPTIONS] [FILE]
```

List all labels in FILE.
By default, output formatted as plain text.


#### Picking body up

```
$ rosa pick [OPTIONS] LABEL [FILE]
```

Pick up the body(ies) mapped with LABEL.
By default, rosa returns only first body.

If `-a` supplied, all bodies print as formatting type (see 'Output formatting').

If `-n NUM` supplied, set a number of bodies picked up.


#### Dumping entire pairs

```
$ rosa dump [OPTIONS] [FILE]
```

Dump entire key-value structure in FILE.
By default, output formatted as S-expression.


### Output formatting

Rosa can print pretty output several formats.
To tell format, use options bellow:

- S-expression: `-s`
    - Common Lisp plist and vector syntax
- JSON: `-j`
- YAML: `-y`


## Installation

Use [roswell](https://github.com/roswell/roswell/).

```
$ ros install t-sin/rosa
```

If you feel `rosa` command is very slow, compile `rosa`.

```
$ cd ~/.roswell/bin/
$ ros build rosa.ros
```


## Author

- Shinichi Tanaka (shinichi.tanaka45@gmail.com)

## Copyright

Copyright (c) 2016 Shinichi TANAKA (shinichi.tanaka45@gmail.com)

## License

Licensed under the MIT License.
