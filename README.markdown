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

## installation

## author

* shinichi tanaka (shinichi.tanaka45@gmail.com)

## Copyright

Copyright (c) 2016 Shinichi TANAKA (shinichi.tanaka45@gmail.com)

## License

Licensed under the MIT License.
