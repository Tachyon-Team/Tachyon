;; _________________________________________________________________________
;;
;;             Tachyon : A Self-Hosted JavaScript Virtual Machine
;;
;;
;;  This file is part of the Tachyon JavaScript project. Tachyon is
;;  distributed at:
;;  http://github.com/Tachyon-Team/Tachyon
;;
;;
;;  Copyright (c) 2011, Universite de Montreal
;;  All rights reserved.
;;
;;  This software is licensed under the following license (Modified BSD
;;  License):
;;
;;  Redistribution and use in source and binary forms, with or without
;;  modification, are permitted provided that the following conditions are
;;  met:
;;    * Redistributions of source code must retain the above copyright
;;      notice, this list of conditions and the following disclaimer.
;;    * Redistributions in binary form must reproduce the above copyright
;;      notice, this list of conditions and the following disclaimer in the
;;      documentation and/or other materials provided with the distribution.
;;    * Neither the name of the Universite de Montreal nor the names of its
;;      contributors may be used to endorse or promote products derived
;;      from this software without specific prior written permission.
;;
;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;;  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;;  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;;  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
;;  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;;  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;;  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;; _________________________________________________________________________

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
"atomic" ;; ********* extensions *********
"future" ;; ********* extensions *********

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
