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

    // Simple IIR add tests, no spills needed
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

    /* TODO
    // Multiple simple IIR operations
    compileStr('                        \
    function add (v1, v2)               \
    {                                   \
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
    ');
    */

    /*
    compileStr('                        \
    function add (v1, v2, v3)           \
    {                                   \
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
    ');
    */
}

