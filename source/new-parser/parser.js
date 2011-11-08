/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Parser implementation.

@author
Maxime Chevalier-Boisvert
*/

// TODO: option to keep comments?


/*
Unlike white space characters, line terminators have some influence over the
behaviour of the syntactic grammar. In general, line terminators may occur
between any two tokens, but there are a few places where they are forbidden by
the syntactic grammar. Line terminators also affect the process of automatic
semicolon insertion (7.9). A line terminator cannot occur within any token
except a StringLiteral. Line terminators may only occur within a StringLiteral
token as part of a LineContinuation.

Line terminators:
\u000A  Line Feed            <LF>
\u000D  Carriage Return      <CR>
\u2028  Line separator       <LS>
\u2029  Paragraph separator  <PS>

LineTerminator ::
<LF>
<CR>
<LS>
<PS>

LineTerminatorSequence ::
<LF>
<CR> [lookahead ∉ <LF> ]
<LS>
<PS>
<CR> <LF>
*/


/*
There are three basic rules of semicolon insertion:
1. When, as the program is parsed from left to right, a token (called the
offending token) is encountered that is not allowed by any production of the
grammar, then a semicolon is automatically inserted before the offending token
if one or more of the following conditions is true:
• The offending token is separated from the previous token by at least one LineTerminator.
• The offending token is }.

2. When, as the program is parsed from left to right, the end of the input
stream of tokens is encountered and the parser is unable to parse the input
token stream as a single complete ECMAScript Program, then a semicolon is
automatically inserted at the end of the input stream.

3. When, as the program is parsed from left to right, a token is encountered
that is allowed by some production of the grammar, but the production is a
restricted production and the token would be the first token for a terminal or
nonterminal immediately following the annotation “[no LineTerminator here]”
within the restricted production (and therefore such a token is called a
restricted token), and the restricted token is separated from the previous
token by at least one LineTerminator, then a semicolon is automatically
inserted before the restricted token.

However, there is an additional overriding condition on the preceding rules:
a semicolon is never inserted automatically if the semicolon would then be
parsed as an empty statement or if that semicolon would become one of the two
semicolons in the header of a for statement (see 12.6.3).

TODO: if complete *statement* parsed and line terminator occurs, pretend
semicolon inserted? If a semicolon could have occurred and would be ending
the statement***. NO! Must be implemented as the spec says...

a = b + c
(d + e).print()
==>
a = b + c(d + e).print()
*/


/*
The following tokens are ECMAScript keywords and may not be used as Identifiers
in ECMAScript programs.

Keyword :: one of

break
do
instanceof
typeof
case
else
new
var
catch
finally
return
void
continue
for
switch
while
debugger
function
this
with
default
if
throw
delete
in
try
*/


/*
HexEscapeSequence ::
x HexDigit HexDigit
UnicodeEscapeSequence ::
u HexDigit HexDigit HexDigit HexDigit

\b
\u0008
backspace
<BS>

\t
\u0009
horizontal tab
<HT>

\n
\u000A
line feed (new line)
<LF>

\v
\u000B
vertical tab
<VT>

\f
\u000C
form feed
<FF>

\r
\u000D
carriage return
<CR>

\"
\u0022
double quote
"

\'
\u0027
single quote
'

\\
\u005C
backslash
\

*/




// TODO: look at grammar productions for kinds of expressions, statements, program-level



// TODO: automatic semicolon unit test





function Parser()
{



}




// consume vs peek?


// TODO: standard way of generating operator/expression parsing?
// priority, keywords







