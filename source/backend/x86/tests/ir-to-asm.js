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
Unit tests for the ir translation part of the x86 backend.

@author
Maxime Chevalier-Boisvert
*/

/**
Test suite for x86 code generation
*/
tests.x86 = tests.x86 || tests.testSuite();

/**
Test the IR to ASM translation
*/
tests.x86.irToAsm = function ()
{
    // Get the host compilation parameters
    var params = config.hostParams;

    // Get a reference to the backend object
    var backend = config.hostParams.backend;

    // If we are not running under the x86 backend, stop
    if (!(backend instanceof x86.Backend))
        return;

    // Test the execution of a piece of code
    function test(str, retVal, argVals)
    {
        if (argVals === undefined)
            argVals = [];

        var ast = parse_src_str(str, params);
        var ir = unitToIR(ast, params);
        lowerIRFunc(ir, params);

        irFunc = ir.getChild('test');

        print(irFunc);
        print('');

        // Generate the machine code for the function
        var code = backend.genCode(irFunc, params);

        var blockAddr = code.codeBlock.getAddress();

        var argTypes = [];
        for (var i = 0; i < argVals.length; ++i)
            argTypes.push('int');

        var ctxPtr = backend.x86_64? [0,0,0,0,0,0,0,0]:[0,0,0,0];

        var ret = callTachyonFFI(
            argTypes,
            'int',
            blockAddr,
            ctxPtr,
            argVals
        );

        if (ret !== retVal)
        {
            error(
                'invalid return value for:\n' +
                '\n' +
                irFunc + '\n' +
                '\n' +
                'with assembly:\n' +
                '\n' +
                code.assembler.toString(true) + '\n' +
                '\n' +
                'got:\n' +
                ret + '\n' +
                'expected:\n' +
                retVal
            );
        }
    }

    // Simple IIR add tests, 1 argument, no spills needed
    test('                                  \
        function test(ctx, v1)              \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:ret pint";             \
                                            \
            return iir.add(v1, pint(7));    \
        }                                   \
        ',
        10,
        [3]
    );
    test('                                  \
        function test(ctx, v1)              \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:ret pint";             \
                                            \
            return iir.add(pint(3), v1);    \
        }                                   \
        ',
        6,
        [3]
    );

    // Simple IIR multiplication
    test('                                  \
        function test(ctx, a)               \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg a pint";           \
            "tachyon:ret pint";             \
                                            \
            a *= pint(3);                   \
            return a;                       \
        }                                   \
        ',
        3,
        [1]
    );

    // IIR multiplication by 2, optimizes to left shift
    test('                                  \
        function test(ctx, a)               \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg a pint";           \
            "tachyon:ret pint";             \
                                            \
            return a * pint(2);             \
        }                                   \
        ',
        6,
        [3]
    );

    // Multiple simple IIR operations, 2 arguments, no spills needed
    test('                                  \
        function test(ctx, v1, v2)          \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:arg v2 pint";          \
            "tachyon:ret pint";             \
                                            \
            var x0 = iir.add(v1, v2);       \
            var x1 = iir.mul(x0, pint(3));  \
            var x2 = iir.sub(x1, pint(2));  \
                                            \
            return x2;                      \
        }                                   \
        ',
        13,
        [2,3]
    );

    // 3 arguments, IIR add
    test('                                  \
        function test(ctx, v1, v2, v3)      \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:arg v2 pint";          \
            "tachyon:arg v3 pint";          \
            "tachyon:ret pint";             \
                                            \
            var x = iir.add(v1, v2);        \
            var x = iir.add(x, v3);         \
                                            \
            return x;                       \
        }                                   \
        ',
        6,
        [1,2,3]
    );

    // Many IIR operations, several registers needed
    test('                                  \
        function test(ctx, v1, v2, v3)      \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:arg v2 pint";          \
            "tachyon:arg v3 pint";          \
            "tachyon:ret pint";             \
                                            \
            var x1 = iir.add(v1, pint(1));  \
            var x2 = iir.add(v1, pint(2));  \
            var x3 = iir.add(v1, pint(3));  \
            var x4 = iir.add(v1, pint(4));  \
            var x5 = iir.add(v1, pint(5));  \
            var x6 = iir.add(v1, pint(6));  \
            var x7 = iir.add(v1, pint(7));  \
                                            \
            var y = iir.mul(x1, x2);        \
            var y = iir.mul(y, x3);         \
            var y = iir.mul(y, x4);         \
            var y = iir.mul(y, x5);         \
            var y = iir.mul(y, x6);         \
            var y = iir.mul(y, x7);         \
                                            \
            var z = iir.sub(y, pint(1));    \
                                            \
            return z;                       \
        }                                   \
        ',
        5039,
        [0,1,2]
    );

    // Many IIR operations, spills needed in 32-bit and 64-bit
    test('                                  \
        function test(ctx, v1, v2, v3)      \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:arg v2 pint";          \
            "tachyon:arg v3 pint";          \
            "tachyon:ret pint";             \
                                            \
            var x01 = v1 * pint(1);         \
            var x02 = v1 * pint(2);         \
            var x03 = v1 * pint(3);         \
            var x04 = v1 * pint(4);         \
            var x05 = v1 * pint(5);         \
            var x06 = v1 * pint(6);         \
            var x07 = v1 * pint(7);         \
            var x08 = v1 * pint(8);         \
            var x09 = v1 * pint(9);         \
            var x10 = v1 * pint(10);        \
            var x11 = v1 * pint(11);        \
            var x12 = v1 * pint(12);        \
            var x13 = v1 * pint(13);        \
            var x14 = v1 * pint(14);        \
            var x15 = v1 * pint(15);        \
            var x16 = v1 * pint(16);        \
            var x17 = v1 * pint(17);        \
            var x18 = v1 * pint(18);        \
                                            \
            var y = x01 + x02;              \
            var y = y + x03;                \
            var y = y + x04;                \
            var y = y + x05;                \
            var y = y + x06;                \
            var y = y + x07;                \
            var y = y + x08;                \
            var y = y + x09;                \
            var y = y + x10;                \
            var y = y + x11;                \
            var y = y + x12;                \
            var y = y + x13;                \
            var y = y + x14;                \
            var y = y + x15;                \
            var y = y + x16;                \
            var y = y + x17;                \
            var y = y + x18;                \
            var z = y - v2 - v3;            \
                                            \
            return z;                       \
        }                                   \
        ',
        166,
        [1,2,3]
    );

    // Variations of arithmetic instructions, spills needed
    test('                                      \
        function test(ctx, v1, v5, v8, v10)     \
        {                                       \
            "tachyon:cproxy";                   \
            "tachyon:arg ctx rptr";             \
            "tachyon:arg v1 pint";              \
            "tachyon:arg v8 pint";              \
            "tachyon:arg v5 pint";              \
            "tachyon:arg v10 pint";             \
            "tachyon:ret pint";                 \
                                                \
            var x01 = v1 * pint(2);             \
            var x02 = pint(2) * v1;             \
            var x03 = v1 + pint(3);             \
            var x04 = pint(4) + v1;             \
            var x05 = v1 + v8;                  \
            var x06 = v8 + v1;                  \
            var x07 = v10 - pint(7);            \
            var x08 = pint(5) - v10;            \
            var x09 = v10 % pint(9);            \
            var x10 = pint(9) % pint(5);        \
            var x11 = v1 * pint(3);             \
            var x12 = pint(3) * v1;             \
            var x13 = v10 / pint(5);            \
            var x14 = v8 / pint(2);             \
            var x15 = v8 * pint(2);             \
            var x16 = v1 * pint(16);            \
            var x17 = v1 * pint(17);            \
            var x18 = v1 * pint(18);            \
                                                \
            var y = x01 + x02;                  \
            var y = y + x03;                    \
            var y = y + x04;                    \
            var y = y + x05;                    \
            var y = y + x06;                    \
            var y = y + x07;                    \
            var y = y + x08;                    \
            var y = y + x09;                    \
            var y = y + x10;                    \
            var y = y + x11;                    \
            var y = y + x12;                    \
            var y = y + x13;                    \
            var y = y + x14;                    \
            var y = y + x15;                    \
            var y = y + x16;                    \
            var y = y + x17;                    \
            var y = y + x18;                    \
                                                \
            return y;                           \
        }                                       \
        ',
        113,
        [1,5,8,10]
    );

    // If statement, return value merge
    test('                                  \
        function test(ctx, v1, v2)          \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:arg v2 pint";          \
            "tachyon:ret pint";             \
                                            \
            if (v1 !== pint(0))             \
                var y = v2 + pint(1);       \
            else                            \
                var y = v2 - pint(1);       \
                                            \
            return y;                       \
        }                                   \
        ',
        8,
        [1, 7]
    );

    // Comparison test
    test('                                  \
        function test(ctx, v1, v2)          \
        {                                   \
            "tachyon:cproxy";               \
            "tachyon:arg ctx rptr";         \
            "tachyon:arg v1 pint";          \
            "tachyon:arg v2 pint";          \
            "tachyon:ret pint";             \
                                            \
            var sum = pint(0);              \
                                            \
            if (v1 !== pint(0))             \
                sum += pint(1);             \
            if (v2 === pint(0))             \
                sum += pint(555);           \
            if (v1 < pint(1))               \
                sum += pint(1);             \
            if (v1 <= v2)                   \
                sum += pint(1);             \
            if (v1 > v2)                    \
                sum += pint(555);           \
            if (v2 >= v1)                   \
                sum += pint(1);             \
                                            \
            return sum;                     \
        }                                   \
        ',
        4,
        [-3, 7]
    );
    
    // Simple loop, counter incrementation
    test('                                      \
        function test(ctx, v1, v2)              \
        {                                       \
            "tachyon:cproxy";                   \
            "tachyon:arg ctx rptr";             \
            "tachyon:arg v1 pint";              \
            "tachyon:arg v2 pint";              \
            "tachyon:ret pint";                 \
                                                \
            var sum = v2;                       \
                                                \
            for (var i = pint(0); i < v1; ++i)  \
                sum = pint(2) + sum;            \
                                                \
            return sum;                         \
        }                                       \
        ',
        27,
        [10, 7]
    );

    // Loop with nested if statement, modulo operator
    test('                                      \
        function test(ctx, v1, v2)              \
        {                                       \
            "tachyon:cproxy";                   \
            "tachyon:arg ctx rptr";             \
            "tachyon:arg v1 pint";              \
            "tachyon:arg v2 pint";              \
            "tachyon:ret pint";                 \
                                                \
            var sum = v2;                       \
                                                \
            for (var i = pint(0); i < v1; ++i)  \
            {                                   \
                if (i % pint(2) === pint(0))    \
                    sum += i;                   \
            }                                   \
                                                \
            return sum;                         \
        }                                       \
        ',
        22,
        [10, 2]
    );

    // Nested loops with if statement, modulo operator
    test('                                          \
        function test(ctx, v1, v2, v3)              \
        {                                           \
            "tachyon:cproxy";                       \
            "tachyon:arg ctx rptr";                 \
            "tachyon:arg v1 pint";                  \
            "tachyon:arg v2 pint";                  \
            "tachyon:arg v3 pint";                  \
            "tachyon:ret pint";                     \
                                                    \
            var sum = v3;                           \
                                                    \
            for (var i = pint(0); i < v1; ++i)      \
            {                                       \
                for (var j = pint(0); j < v2; ++j)  \
                {                                   \
                    if (j % pint(2) === pint(0))    \
                        sum += j;                   \
                }                                   \
            }                                       \
                                                    \
            return sum;                             \
        }                                           \
        ',
        102,
        [5, 10, 2]
    );









    /*
    // FIXME: memory-memory moves cause issues!
    // Nested loops and if, spills needed
    test('                                          \
        function test(ctx, v1, v2, v3)              \
        {                                           \
            "tachyon:cproxy";                       \
            "tachyon:arg ctx rptr";                 \
            "tachyon:arg v1 pint";                  \
            "tachyon:arg v2 pint";                  \
            "tachyon:arg v3 pint";                  \
            "tachyon:ret pint";                     \
                                                    \
            var sum = v3;                           \
                                                    \
            for (var i = pint(0); i < v1; ++i)      \
            {                                       \
                for (var j = pint(0); j < v2; ++j)  \
                {                                   \
                    if (j % pint(2) === pint(0))    \
                    {                               \
                        var x01 = j + pint(1);      \
                        var x02 = j + pint(2);      \
                        var x03 = j + pint(3);      \
                        var x04 = j + pint(4);      \
                        var x05 = j + pint(5);      \
                        var x06 = j + pint(6);      \
                        var x07 = j + pint(7);      \
                        var x08 = j + pint(8);      \
                        var x09 = j + pint(9);      \
                        var x10 = j + pint(10);     \
                        var x11 = j + pint(11);     \
                        var x12 = j + pint(12);     \
                        var x13 = j + pint(13);     \
                        var x14 = j + pint(14);     \
                        var x15 = j + pint(15);     \
                        var x16 = j + pint(16);     \
                        var x17 = j + pint(17);     \
                        var x18 = j + pint(18);     \
                    }                               \
                    else                            \
                    {                               \
                        var x01 = v1 + pint(1);     \
                        var x02 = v1 + pint(2);     \
                        var x03 = v1 + pint(3);     \
                        var x04 = v1 + pint(4);     \
                        var x05 = v1 + pint(5);     \
                        var x06 = v1 + pint(6);     \
                        var x07 = v1 + pint(7);     \
                        var x08 = v1 + pint(8);     \
                        var x09 = v1 + pint(9);     \
                        var x10 = v1 + pint(10);    \
                        var x11 = v1 + pint(11);    \
                        var x12 = v1 + pint(12);    \
                        var x13 = v1 + pint(13);    \
                        var x14 = v1 + pint(14);    \
                        var x15 = v1 + pint(15);    \
                        var x16 = v1 + pint(16);    \
                        var x17 = v1 + pint(17);    \
                        var x18 = v1 + pint(18);    \
                    }                               \
                                                    \
                    var y = x01 + x02;              \
                    var y = y + x03;                \
                    var y = y + x04;                \
                    var y = y + x05;                \
                    var y = y + x06;                \
                    var y = y + x07;                \
                    var y = y + x08;                \
                    var y = y + x09;                \
                    var y = y + x10;                \
                    var y = y + x11;                \
                    var y = y + x12;                \
                    var y = y + x13;                \
                    var y = y + x14;                \
                    var y = y + x15;                \
                    var y = y + x16;                \
                    var y = y + x17;                \
                    var y = y + x18;                \
                                                    \
                    sum += y;                       \
                }                                   \
            }                                       \
                                                    \
            return sum;                             \
        }                                           \
        ',
        1007,
        [5, 10, 2]
    );
    */










    // Integer cast, 16-bit operand add
    test('                                          \
        function test(ctx, v1, v5, v8, v10)         \
        {                                           \
            "tachyon:cproxy";                       \
            "tachyon:arg ctx rptr";                 \
            "tachyon:arg v1 pint";                  \
            "tachyon:arg v8 pint";                  \
            "tachyon:arg v5 pint";                  \
            "tachyon:arg v10 pint";                 \
            "tachyon:ret pint";                     \
                                                    \
            var x1 = iir.icast(IRType.i16, v5);     \
            var x2 = iir.icast(IRType.i16, v8);     \
                                                    \
            var y = x1 + x2;                        \
            y = iir.icast(IRType.pint, y);          \
                                                    \
            return y;                               \
        }                                           \
        ',
        13,
        [1,5,8,10]
    );





    // TODO: various size operands + spills after + if merge





    // TODO: test boxInt + JS boxed add + ret unboxInt






    // TODO: peephole optimizer
    // Assembler.addPattern(patternFunc?) ?
    // Need this to be somewhat more optimized...? pattern should have a
    // start instruction? Map patterns based on start instructions?
    //
    // Can always start simple, build on
    // Assembler.optimize() function... Hardcoded patterns to start with.





}

