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
Unit tests for AST->IR translation code

@author
Maxime Chevalier-Boisvert
*/

/**
Test suite for AST->IR translation code
*/
tests.ir = tests.testSuite();

/**
Helper functions for this test suite
*/
tests.ir.helpers = {};

/**
Setup function for the IR test suite
*/
tests.ir.init = function ()
{
    // Perform a partial initialization of the Tachyon runtime
    // without compiling machine code
    initPrimitives(config.hostParams, true);
}

/**
Parse a source code string, copy the resulting IR and validate it
*/
tests.ir.helpers.testSource = function (sourceStr, printOut, hostParams)
{
    var params = hostParams? config.hostParams:config.clientParams

    // Parse the source string
    var ast = parse_src_str(sourceStr, params);

    // Translate the AST to IR
    var ir = unitToIR(ast, params);

    if (printOut === true)
        print(ir);

    // Copy the resulting function
    ir.copy();

    // Validate the IR
    ir.validate();

    // Perform lowering on the IR
    lowerIRFunc(ir, params);

    if (printOut === true)
        print(ir);

    // Validate the IR
    ir.validate();

    // Return the generated IR for further testing
    return ir;
};

/**
Apply a function to each instruction in an IR function and its sub-functions
*/
tests.ir.helpers.forEachInstr = function (ir, instrFunc, testModule)
{
    // Get the list of all functions in the tree
    var flist = ir.getChildrenList();

    // Apply the function to each instruction of each function
    for (var i = 0; i < flist.length; ++i)
    {
        var func = flist[i];

        if (func === ir && testModule !== true)
            continue;

        for (var it = func.virginCFG.getInstrItr(); it.valid(); it.next())
        {
            instrFunc(it.get());
        }
    }
};

/**
Object literal expression
*/
tests.ir.objectExpr = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var o = {                           \
                'a':1,                          \
                b: 'foo'                        \
            };                                  \
            print(o);                           \
        "
    );
};

/**
Array literal expression
*/
tests.ir.arrayExpr = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var o = [                           \
                'a', 1, 'foo', (2+2)            \
            ];                                  \
            print(o);                           \
        "
    );
};

/**
Arithmetic expressions
*/
tests.ir.arithExprs = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var o = a + b * (c - d) % (e / f);  \
            print(o);                           \
        "
    );
};

/**
Assignment operators
*/
tests.ir.assignExprs = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var a = 1;                          \
            var b = 2;                          \
            var c;                              \
            c = a;                              \
            c += a;                             \
            c -= a;                             \
            c *= a;                             \
            c /= a;                             \
            c %= a;                             \
            c &= a;                             \
            c |= a;                             \
            c ^= a;                             \
            c <<= a;                            \
            c >>= a;                            \
            c >>>= a;                           \
            print(c);                           \
        "
    );
};

/**
In expression
*/
tests.ir.inExpr = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var a = b in c;                     \
            print(a);                           \
        "
    );
};

/**
While loop statement
*/
tests.ir.whileStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            while (v)                           \
            {                                   \
                print(v);                       \
                                                \
                if (t)                          \
                    break;                      \
                else                            \
                    continue;                   \
            }                                   \
        "
    );
};

/**
Do-while loop statement
*/
tests.ir.doWhileStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            do                                  \
            {                                   \
                print(v);                       \
                                                \
                if (t)                          \
                    break;                      \
                else                            \
                    continue;                   \
            } while (v);                        \
        "
    );
};

/**
For loop statement
*/
tests.ir.forStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            for (var i = 0; i < 10; ++i)        \
            {                                   \
                print(i);                       \
            }                                   \
                                                \
            print(i);                           \
        "
    );
};

/**
For-in loop statement
*/
tests.ir.forInStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            for (var propName in obj)           \
            {                                   \
                print(propName);                \
            }                                   \
                                                \
            print(propName);                    \
        "
    );
};

/**
For-in loop with a break statement
*/
tests.ir.forInBreak = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function foo(table)                 \
            {                                   \
                for (var c in table)            \
                {                               \
                    break;                      \
                }                               \
            }                                   \
        "
    );
};

/**
Continue and break labels
*/
tests.ir.loopLabels = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            while (true)                        \
            {                                   \
                if (i == 0)                     \
                    continue;                   \
                else if (i == 1)                \
                    continue foo;               \
                else if (i == 2)                \
                    break;                      \
                else                            \
                    break foo;                  \
            }                                   \
        "
    );
};

/**
Infinite loop
*/
tests.ir.infiniteLoop = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                var i = 0;                      \
                while (true)                    \
                {                               \
                    ++i;                        \
                }                               \
                print(i);                       \
            }                                   \
        "
    );
};

/**
Nested loops
*/
tests.ir.nestedLoops = function ()
{
    tests.ir.helpers.testSource(
        "                                           \
            function nest()                         \
            {                                       \
                var v = 0;                          \
                for (var i = 0; i < 10; ++i)        \
                    for (var j = 0; j < 10; ++j)    \
                        v = i + j;                  \
                                                    \
                return v;                           \
            }                                       \
        "
    );
};

/**
Switch statement
*/
tests.ir.switchStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function switcher(v)                \
            {                                   \
                var o = 0;                      \
                                                \
                switch (v)                      \
                {                               \
                    case 1:     o = 1; break;   \
                    case 'a':   o = 2;          \
                    default:    o = -1;         \
                }                               \
                                                \
                return 0;                       \
            }                                   \
        "
    );
};

/**
Try statement
*/
tests.ir.tryStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function foo(v)                     \
            {                                   \
                var a = 0;                      \
                                                \
                try                             \
                {                               \
                    a = 1;                      \
                    a = bar(v);                 \
                }                               \
                                                \
                catch (e)                       \
                {                               \
                    print(a);                   \
                    a = 2;                      \
                    print(a);                   \
                }                               \
                                                \
                finally                         \
                {                               \
                    print(a);                   \
                }                               \
                                                \
                return a;                       \
            }                                   \
        "
    );
};

/**
With statement
*/
tests.ir.withStmt = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo(obj, x, y)             \
            {                                   \
                with (obj)                      \
                {                               \
                    x = 3;                      \
                    print(y);                   \
                }                               \
            }                                   \
        "
    );

    // Ensure that there are get and put property instructions in the IR
    var hasGet = false;
    var hasPut = false;
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (instr instanceof CallFuncInstr && 
                instr.uses[0].funcName == 'getPropVal')
                hasGet = true;
            if (instr instanceof CallFuncInstr && 
                instr.uses[0].funcName == 'putPropVal')
                hasPut = true;
        }
    );
    assert (
        hasGet && hasPut,
        'With statement code missing get or put property instruction'
    );
};

/**
Fibonacci function
*/
tests.ir.fibFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function fib(n)                     \
            {                                   \
                if (n < 2)                      \
                    return n;                   \
                                                \
                return fib(n-1) + fib(n-2);     \
            }                                   \
        "
    );
};

/**
Nested if with constants and return statements in a function
*/
tests.ir.ifRetCstFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                           \
            function foo()                          \
            {                                       \
                if (true)                           \
                {                                   \
                    var v = foo;                    \
                                                    \
                    if (true)                       \
                        return 0;                   \
                    else                            \
                        return 1;                   \
                }                                   \
            }                                       \
        "
    );
};

/**
Nested if with variables and return statements in a function
*/
tests.ir.ifRetFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                           \
            function foo(a, b)                      \
            {                                       \
                if (a)                              \
                {                                   \
                    var v = foo;                    \
                                                    \
                    if (b)                          \
                        return 0;                   \
                    else                            \
                        return 1;                   \
                }                                   \
            }                                       \
        "
    );
};

/**
Constructor function
*/
tests.ir.ctorFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function ctor(a0)                   \
            {                                   \
                this.a0 = a0;                   \
            }                                   \
            var obj = new ctor(1);              \
            print(obj);                         \
        "
    );
};

/**
Closure function
*/
tests.ir.closFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                var v = 1;                      \
                                                \
                function bar()                  \
                {                               \
                    v++;                        \
                    print(v);                   \
                }                               \
                                                \
                bar();                          \
                print(v);                       \
            }                                   \
        "
    );
};

/**
Arguments object
*/
tests.ir.argsObj = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo(a0, a1)                \
            {                                   \
                arguments[0] = 3;               \
                                                \
                function bar()                  \
                {                               \
                    a1 = 2;                     \
                    print(a0);                  \
                }                               \
                                                \
                bar();                          \
                print(a0);                      \
            }                                   \
        "
    );

    // Ensure that there are no direct references to argument values
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            for (var j = 0; j < instr.uses.length; ++j)
            {
                assert (
                    instr.uses[j].getValName() != 'a0' && 
                    instr.uses[j].getValName() != 'a1',
                    'argument values should not be used directly'
                );
            }
        }
    );
};

/**
Inlining of function calls
*/
tests.ir.inlining = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                bar(1337);                      \
            }                                   \
                                                \
            function bar(a0)                    \
            {                                   \
                print(a0);                      \
            }                                   \
        "
    );

    // Find the bar call instruction
    var callInstr;
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (instr instanceof CallFuncInstr &&
                instr.getArg(0) === ConstValue.getConst(1337))
                callInstr = instr;
        }
    );

    // Find the bar function
    var barFunc = ir.getChild('bar');

    // Inline the bar call
    inlineCall(callInstr, barFunc);

    //print(ir);

    // Validate the resulting IR
    ir.validate();
};

/**
Inlining of function calls with exception handling
*/
tests.ir.inliningExcept = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                try                             \
                {                               \
                    bar(1337);                  \
                }                               \
                                                \
                catch (e)                       \
                {                               \
                    print('nooo!')              \
                }                               \
            }                                   \
                                                \
            function bar(a0)                    \
            {                                   \
                print(a0);                      \
            }                                   \
        "
    );

    // Find the bar call instruction
    var callInstr;
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (instr instanceof CallFuncInstr &&
                instr.getArg(0) === ConstValue.getConst(1337))
                callInstr = instr;
        }
    );

    // Find the bar function
    var barFunc = ir.getChild('bar');

    //print(ir);

    // Inline the bar call
    inlineCall(callInstr, barFunc);

    //print(ir);

    // Validate the resulting IR
    ir.validate();
};

/**
Inlining of function calls with exception handling and throw
*/
tests.ir.inliningThrow = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                try                             \
                {                               \
                    bar(1337);                  \
                }                               \
                                                \
                catch (e)                       \
                {                               \
                    print('nooo!')              \
                }                               \
            }                                   \
                                                \
            function bar(a0)                    \
            {                                   \
                throw 'zomg';                   \
            }                                   \
        "
    );

    // Find the bar call instruction
    var callInstr;
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (instr instanceof CallFuncInstr &&
                instr.getArg(0) === ConstValue.getConst(1337))
                callInstr = instr;
        }
    );

    // Find the bar function
    var barFunc = ir.getChild('bar');

    //print(ir);

    // Inline the bar call
    inlineCall(callInstr, barFunc);

    //print(ir);

    // Validate the resulting IR
    ir.validate();
};

