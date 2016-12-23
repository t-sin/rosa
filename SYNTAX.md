# syntax as EBNF

```
# special character
<SPACE> ::= ? space (` `) ?
<COLON> ::= ? colon (`:`) ?
<SEMICOLON> ::= ? semicolon (`;`) ?
<EOF> ::= ? end-of-file ?
<EOL> ::= ? end-of-line (e.g. LF) ?
<LABEL_IDENTIFER> ::= ? represented as regex `[a-z][a-z-]*` ?
<NOT_DELIMITER> ::= ? represent as regex `[^;:]` ?
<ANYCHAR> ::= ? any charactor excluding <EOF> and <EOL> ?

<TEXT> ::= <LINE>* <EOF>
<LINE> ::= <ELEMENT> | <PLAIN> | <EOL>
<ELEMENT> ::= <INLINE_LABEL> | <BLOCK_LABEL> | <COMMENT> | <ESCAPE>
<ESCAPE> ::= <ESCAPE_COLON> | <ESCAPE_SEMICOLON>

<INLINE_LABEL> ::= <COLON> <LABEL_IDENTIFER> <SPACE> <ANYCHAR>+ <EOL>
<BLOCK_LABEL> ::= <COLON> <LABEL_IDENTIFER> <EOL>
<COMMENT> ::= <SEMICOLON> <ANYCHAR>+ <EOL> | <SEMICOLON> <EOL>

<ESCAPE_COLON> ::= <COLON> <COLON> <ANYCHAR>* <EOL>
<ESCAPE_SEMICOLON> ::= <COLON> <SEMICOLON> <ANYCHAR>* <EOL>

<PLAIN> ::= <NOT_DELIMITER> <ANYCHAR>* <EOL>
```
