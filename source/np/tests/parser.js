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
 *  TO, THE IMPLIED WARRANTIES OF MERCHApNTABILITY AND FITNESS FOR A
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
Test suite for the parser
*/
tests.parser = tests.testSuite();


// TODO



/*
// TODO: test automatic semicolon insertion, multiple cases
// TODO: insert example cases

{ 1 2 } 3
is not a valid sentence in the ECMAScript grammar, even with the automatic
semicolon insertion rules. In contrast, the source
{ 1 
2 } 3
is also not a valid ECMAScript sentence, but is transformed by automatic
semicolon insertion into the following:
{ 1 
;2 ;} 3;
which is a valid ECMAScript sentence.


for (a; b 
)

is not a valid ECMAScript sentence and is not altered by automatic semicolon
insertion because the semicolon is needed for the header of a for statement.
Automatic semicolon insertion never inserts one of the two semicolons in the
header of a for statement


The source
return 
a + b
is transformed by automatic semicolon insertion into the following:
return; 
a + b;


The source
a = b 
++c
is transformed by automatic semicolon insertion into the following:
a = b; 
++c;


The source
if (a > b) 
else c = d
is not a valid ECMAScript sentence and is not altered by automatic semicolon
insertion before the else token, even though no production of the grammar
applies at that point, because an automatically inserted semicolon would then
be parsed as an empty statement.


The source
a = b + c 
(d + e).print()
is not transformed by automatic semicolon insertion, because the parenthesised
expression that begins the second line can be interpreted as an argument list
for a function call:
a = b + c(d + e).print()
*/

