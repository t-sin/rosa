# Rosa - Simple markup language for named text parts

Rosa is a simple markup language for named text parts.


## Basis

Rosa's *named text* is a pair of strings, consists of **name** and **text**.

**Name** is a name of **text**.
**Text** is just one line string, or is multi line strings.

A line starts with colon (`:`) denotes **name**.
A line starts with semicolon (`;`) denotes **comment**.

To denotes colon and semicolon themselves at line head, use escape sequence; is write twice the character.
For colon, this is it: `::`.
For semicolon, this is it: `;;`.

If the line has space (` `), it is called **inline**.
In this case, **name** is a string from next character of colon to previous character of first space, and **text** is a string from next character of first space to end of line. 

If the line has no space, it is called **block**.
In this case, **name** is a string from next character of colon to end of line.
**text** is successor lines until appears one of **inline** name, **block** name or **comment**. 


### Example

Let's see an example:

```
:title README for rosa
:author Shinichi TANAKA
:date 2016-05-01

:abstract

Rosa is a simple markup language for named text parts.

:basis

Rosa's *named text* is a pair of strings, consists of **name** and **text**.

**Name** is a name of **text**.
**Text** is just one line string, or is multi line strings.

;comment

phew, english... I'm tired now...

```

`title`, `author`, `date`, `abstract` and `basis` are **names** included this examples.

Three inline **names** have following **texts**:

- `title`: `README for rosa`
- `author`: `Shinichi TANAKA`
- `date`: `2016-05-01`

Two block **names** have following **texts**:

- `abstract`:

```
Rosa is a simple markup language for named text parts.
```

- `basis`:

```
Rosa's *named text* is a pair of strings, consists of **name** and **text**.

**Name** is a name of **text**.
**Text** is just one line string, or is multi line strings.
```

Note that the line starts with semicolon (`;comment`) is ignored.


## usage

There is a CLI front-end for parse rosa markup language.
`rosa` is it.
Installation guide is told on the next chapter.

Here is sypnosis for `rosa`:

```
$ rosa name [file]
```

Basically, text data come from stdin, specify `name` you want.
If the target text is in a text file, additionaly specify `file`.

When you need entire structure of target text, specify `:all` to `name`.
Then `rosa` returns a structure represented as S-expression.

(I think, It's convinient that, if specified `:yml` to `name` then `rosa.ros` returns a structure as YAML... But not implemented, yet)


## installation

### For citizen of Land of Lisp

For **citizen of Land of Lisp**, comrade, clone rosa into your load path and type the magic words `(ql:quickload :rosa)`.
To install `rosa`, type `ros install rosa`.


### For common user

For common user, sorry, rosa has no binary for distribution now.
We take your some munites, but installation is not too difficult, because of some great work.

Brief install instruction is bellow (on Linux)

```
# Move to temporary directory (to build binary)
$ cd ~/tmp

# Install roswell to build rosa
$ sudo apt install libcurl4-openssl-dev automake
$ git clone https://github.com/roswell/roswell
$ cd roswell
$ ./bootstrap && make && sudo make install
$ ros setup

# install rosa
$ cd ~/.roswell/local-projects
$ git clone https://github.com/t-sin/rosa
$ ros install rosa

# test rosa
$ rosa
Rosa - Simple markup ranguage for named text parts
usage: rosa.ros name [file]

DESCRIPTION
    rosa.ros get text named `name` from `file` and output stdout.
    now, rosa.ros only returned first text of all appearing.

PARAMETERS
      name: required. target name. if supplied `:all`, output all pairs name
            and text as S expression.
      file: not required. input files. if not supplied, read data from stdin.

```

For detail, see some Common Lisp documents bellow:

* [Roswell installation](https://github.com/roswell/roswell/wiki/1.-Installation)
* [About roswell script](https://github.com/roswell/roswell/wiki/2.-Roswell-as-a-Scripting-Environment)



## author

* shinichi tanaka (shinichi.tanaka45@gmail.com)

## Copyright

Copyright (c) 2016 Shinichi TANAKA (shinichi.tanaka45@gmail.com)

## License

Licensed under the MIT License.
