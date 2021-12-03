# Rosa - text tagging language

![rosa](https://raw.githubusercontent.com/t-sin/rosa/master/rosa.png)

[![Quicklisp](http://quickdocs.org/badge/rosa.svg)](http://quickdocs.org/rosa/)
[![Build Status](https://travis-ci.org/t-sin/rosa.svg)](https://travis-ci.org/t-sin/rosa)
[![Coverage Status](https://coveralls.io/repos/github/t-sin/rosa/badge.svg?branch=master)](https://coveralls.io/github/t-sin/rosa?branch=master)

Rosa is a text tagging language. It provides a notation to embedding meta data into text file, like ID3 tag for mp3.

For Japanese, see [this article](http://octahedron.hatenablog.jp/entry/2017/03/24/011008)

> Stat rosa pristina nomine, nomina nuda tenemus.


## Installation

Use [roswell](https://github.com/roswell/roswell/).

```
$ ros install t-sin/rosa
```

## Examples

- write rosa notation, parse it

```lisp
CL-USER> (setf readme "
:title Rosa - text tagging language
:author Shinichi TANAKA
:modify-month 2016-02
:modify-month 2017-04
:body

Rosa is a text tagging language.

Or for Japanese, see [this article](http://octahedron.hatenablog.jp/entry/2017/03/24/011008)

> Stat rosa pristina nomine, nomina nuda tenemus.

...")

CL-USER> (with-input-from-string (in readme)
           (rosa:peruse in))
; not human readable...
#<HASH-TABLE :TEST EQL :COUNT 4 {1002704953}>

CL-USER> (with-input-from-string (in readme)
           (rosa:peruse-as-plist in))
; wow! readable!
(:|title| #("Rosa - text tagging language") :|author| #("Shinichi TANAKA")
 :|modify-month| #("2016-02" "2017-04") :|body|
 #("
Rosa is a text tagging language.

Or for Japanese, see [this article](http://octahedron.hatenablog.jp/entry/2017/03/24/011008)

> Stat rosa pristina nomine, nomina nuda tenemus.

..."))
```

- serialize hash table into rosa notation

```lisp
CL-USRE> (with-input-from-string (in readme)
           (rosa:indite (rosa:peruse-as-plist in)))
":title Rosa - text tagging language
:author Shinichi TANAKA
:modify-month 2016-02
:modify-month 2017-04
:body

Rosa is a text tagging language.

Or for Japanese, see [this article](http:://octahedron.hatenablog.jp/entry/2017/03/24/011008)

> Stat rosa pristina nomine, nomina nuda tenemus.

...
"
```


## Basis

Rosa is a language to attach meta data on text block.
It is ID3 tag-like thing for text file.

Here, a pair in rosa data structure, it consist of just one **label** and multiple **bodies**.
**Bodies** are ordered by appearance from head of text data.
Because of it, array of **bodies** is simply called **body**.

**Label** is a name of **body**.
It is a string represented as regex `[not-space-chars]+`.

**Body** is a value of **label**.
It is a array of strings.
Note that each strings in **body** can include newline.


## Syntax

Rosa has an ASCII-based and minimal syntax.
Here briefly describe rosa syntax.
For BNF definition, see `SYNTAX.md`.

Rosa treats `:` at line head as special.
**Label** lines starts with colon (`:`).


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


#### List notation

When same label appear multiple times, all values are stored. Because of that, we can represent a list, like this:

```
:tags programming
:tags lisp
:tags common-lisp
```

Although that is ugly. In rosa, we can represent list-thing with shorthand notation. The following example is equivalent of above:

```
:tags>
- programming
- lisp
- common-lisp
```


### Escape sequences

To denotes colon and semicolon themselves at line head, use escape sequence.

For colon, this is it: `::`.

An example bellow:

```
:text1

:: is read as ':'
```


## Usage

### As library

To parse plain text as rosa, use one of `rosa:peruse` or `rosa:peruse-as-plist`.

Rosa can sirialize key-value data. `indite` serialize hash-table and plist into string like this:

#### Label equality

If you want, you can characterize label in using `peruse`. For instance, we can regards upcased strings are same as downcased strings, like this:

```lisp
CL-USER> (with-input-from-string (in ":label is upcased")
            (rosa:peruse-as-plist in #'string-upcase))
(:LABEL #("is upcased"))
```

Note that the label returned is `:LABEL`, is not `:|label|`.


### As CLI front-end

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


## Author

- Shinichi Tanaka (shinichi.tanaka45@gmail.com)

## Copyright

Copyright (c) 2017 Shinichi TANAKA (shinichi.tanaka45@gmail.com)

## License

Rosa is licensed under the Lisp GNU Lesser General Public License.
