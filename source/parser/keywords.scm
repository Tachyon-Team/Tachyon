;;;============================================================================

;;; File: "keywords.scm", Time-stamp: <2010-06-08 20:50:37 feeley>

;;; Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

;;;============================================================================

;;; The keywords of ECMAScript 5 (ECMA-262 5th edition / September 2009).

;;;----------------------------------------------------------------------------

(define keywords '(

;; Keyword
"break"
"case"
"catch"
"continue"
"debugger"
"default"
"delete"
"do"
"else"
"finally"
"for"
"function"
"if"
"in"
"instanceof"
"new"
"return"
"switch"
"this"
"throw"
"try"
"typeof"
"var"
"void"
"while"
"with"

;; FutureReservedWord
"class"
"const"
"enum"
"export"
"extends"
"import"
"super"

;; FutureReservedWord (in strict mode)
"implements"
"interface"
"let"
"package"
"private"
"protected"
"public"
"static"
"yield"

;; NullLiteral
"null"

;; BooleanLiteral
"false"
"true"

))

(define token-remap
  '((*eoi*       . EOI)
    (NULLTOKEN   . NULL)
    (TRUETOKEN   . TRUE)
    (FALSETOKEN  . FALSE)
    (CONSTTOKEN  . CONST)
    (VOIDTOKEN   . VOID)
    (DELETETOKEN . DELETE)
    (THISTOKEN   . THIS)
    (INTOKEN     . IN)
    (ANDEQUAL    . BITANDEQUAL)
    (OREQUAL     . BITOREQUAL)
    (XOREQUAL    . BITXOREQUAL)
    (OPENBRACE   . LBRACE)
    (CLOSEBRACE  . RBRACE)
    (+           . PLUS)
    (|(|         . LPAREN)
    (=           . EQUAL)
    (<           . LT)
    (:           . COLON)
    (|\||        . BITOR)
    (!           . EXCL)
    (|[|         . LBRACK)
    (|]|         . RBRACK)
    (/           . DIV)
    (-           . MINUS)
    (|,|         . COMMA)
    (*           . MULT)
    (|)|         . RPAREN)
    (>           . GT)
    (&           . BITAND)
    (~           . BITNOT)
    (?           . QUESTION)
    (|;|         . SEMICOLON)
    (^           . BITXOR)
    (%           . MOD)
    (|.|         . PERIOD)))

;;;============================================================================
