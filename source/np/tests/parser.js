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

/**
Validate an AST after parsing
*/
function parserTestAST(ast, validator)
{
    assert (
        ast instanceof ASTNode,
        'invalid ast node'
    );

    print('AST:\n' + ast);

    if (validator)
        validator(ast);
}

/**
Generate a validator function to test an expression result
*/
function testExprResult(result)
{
    function validator(ast)
    {
        if ((ast instanceof ASTProgram) === false ||
            (ast.numChildren() !== 1) ||
            (ast.children[0] instanceof ASTExpr) === false)
            error('malformed ast');

        var expr = ast.children[0];

        var r = evalASTExpr(expr);

        if (r !== result)
        {
            error(
                'invalid expression result:\n' +
                r + '\n' +
                'expected:\n' +
                result
            );
        }
    }

    return validator;
}

/**
Create a parser unit test from a source string
*/
function parserTestStr(srcStr, validator)
{
    function test()
    {
        var ast = parseString(srcStr, 'string');

        parserTestAST(ast, validator);
    }

    return test;
}

/**
Create a parser unit test from a source file
*/
function parserTestSrc(srcFile, validator)
{
    function test()
    {
        var ast = parseFile(srcFile);

        parserTestAST(ast, validator);
    }

    return test;
}

/**
Lexer test
*/
tests.parser.lexer = function ()
{
    var str = readFile('np/tests/test-lexer.js')

    var lexer = new Lexer(str);

    for (;;)
    {
        var l = lexer.readToken();

        assert (
            l instanceof Lexeme,
            'invalid lexeme object: ' + l
        );

        if (l.type === 'eof')
            break;
    }
}

tests.parser['var1'] = parserTestStr(
    'var foo;'
);

tests.parser['var2'] = parserTestStr(
    'var foo, bar, bif'
);

tests.parser['debugger'] = parserTestStr(
    'debugger;'
);

tests.parser['multi-stmt'] = parserTestStr(
    'var foo; debugger; var bif;'
);

tests.parser['var-num'] = parserTestStr(
    'var n = 1;'
);

tests.parser['add1'] = parserTestStr(
    'var s = x + y;'
);

tests.parser['add2'] = parserTestStr(
    '1 + 1 + 1 + 1 + 1;',
    testExprResult(5)
);

tests.parser['addsub'] = parserTestStr(
    '1 + 2 - 3;',
    testExprResult(0)
);

tests.parser['addmul'] = parserTestStr(
    '1 + 2 * 3;',
    testExprResult(7)
);

tests.parser['mulladd'] = parserTestStr(
    '2 * 2 + 3;',
    testExprResult(7)
);

tests.parser['muladdmul'] = parserTestStr(
    '1 * 2 + 1 * 5;',
    testExprResult(7)
);

tests.parser['paren'] = parserTestStr(
    '3 * (1 + 4);',
    testExprResult(15)
);

tests.parser['minus1'] = parserTestStr(
    'var s = -x;'
);

tests.parser['minus2'] = parserTestStr(
    '-3;',
    testExprResult(-3)
);

tests.parser['minus3'] = parserTestStr(
    '10 + -3;',
    testExprResult(7)
);

tests.parser['assign1'] = parserTestStr(
    'a = 3;'
);

tests.parser['assign2'] = parserTestStr(
    'a = b = c;'
);

tests.parser['assign3'] = parserTestStr(
    'a = b + c;'
);

tests.parser['assign3'] = parserTestStr(
    'a = b = c + d;'
);

// FIXME
tests.parser['assign4'] = parserTestStr(
    'a = b = c * d + e;'
);



//tests.parser.fib = parserTestSrc('np/tests/test-fib.js');

//tests.parser.recfn = parserTestSrc('np/tests/test-recfn.js');

// Number formats
//tests.parser.numbers = parserTestSrc('np/tests/test-numbers.js');

// Various kinds of statements
//tests.parser.stmts = parserTestSrc('np/tests/test-stmts.js');

// JavaScript code captured in the wild
//tests.parser.wild1 = parserTestSrc('np/tests/test-wild1.js');
//tests.parser.wild2 = parserTestSrc('np/tests/test-wild2.js');




// TODO: SunSpider/V8 benchmarks




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

