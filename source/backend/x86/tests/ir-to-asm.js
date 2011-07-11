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
    // Get a reference to the backend object
    var backend = config.hostParams.backend;

    // TODO
    //if (!(backend instanceof x86.Backend))

    // Check if we are running in 32-bit or 64-bit
    const x86_64 = PLATFORM_64BIT;

    // Test the execution of a piece of code
    function test(genFunc, retVal, argVals)
    {
        if (argVals === undefined)
            argVals = [];

        // Create an assembler to generate code into
        var assembler = new x86.Assembler(x86_64);

        // Generate the code
        genFunc(assembler);

        // Assemble to a code block
        var codeBlock = assembler.assemble();

        var blockAddr = codeBlock.getAddress();

        var argTypes = [];
        for (var i = 0; i < argVals.length; ++i)
            argTypes.push('int');

        var ctxPtr = x86_64? [0,0,0,0,0,0,0,0]:[0,0,0,0];

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
                'invalid return value for:\n'+
                '\n' +
                assembler.toString(true) + '\n' +
                '\n' +
                'got:\n' +
                ret + '\n' +
                'expected:\n' +
                retVal
            );
        }
    }

    /*
    // Loop until 10
    test(
        function (a) { with (a) {
            mov(eax, 0);
            var LOOP = label('LOOP');
            add(eax, 1);
            cmp(eax, 10);
            jb(LOOP);
            ret();
        }},
        10
    );

    // Jump with a large offset (> 8 bits)
    test(
        function (a) { with (a) {
            mov(eax, 0);
            var LOOP = label('LOOP');
            add(eax, 1);
            cmp(eax, 15);
            for (var i = 0; i < 400; ++i)
                nop();
            jb(LOOP);
            ret();
        }},
        15
    );

    // Arithmetic
    test(
        function (a) { with (a) {
            push(regb);
            push(regc);
            push(regd);

            mov(rega, 4);       // a = 4
            mov(regb, 5);       // b = 5
            mov(regc, 3);       // c = 3
            add(rega, regb);    // a = 9
            sub(regb, regc);    // b = 2
            mul(regb);          // a = 18, d = 0
            mov(regd, -2);      // d = -2
            imul(regd, rega);   // d = -36
            mov(rega, regd);    // a = -36

            pop(regd);
            pop(regc);
            pop(regb);

            ret();
        }},
        -36
    );

    // Stack manipulation, sign extension
    test(
        function (a) { with (a) {
            sub(regsp, 1);
            var sloc = mem(8, regsp, 0);
            mov(sloc, -3);
            movsx(rega, sloc);
            add(regsp, 1);
            ret();
        }},
        -3
    );
    
    // fib(20), function calls
    test(
        function (a) { with (a) {
            var CALL = new x86.Label('CALL');
            var COMP = new x86.Label('COMP');
            var FIB = new x86.Label('FIB');

            push(regb);
            mov(rega, 20);
            call(FIB);
            pop(regb);
            ret();

            // FIB
            addInstr(FIB);
            cmp(rega, 2);
            jge(COMP);
            ret();

            // COMP
            addInstr(COMP);
            push(rega);         // store n
            sub(eax, 1);        // eax = n-1
            call(FIB);          // fib(n-1)
            mov(regb, rega);    // eax = fib(n-1)
            pop(rega);          // eax = n
            push(regb);         // store fib(n-1)
            sub(rega, 2);       // eax = n-2
            call(FIB);          // fib(n-2)
            pop(regb);          // ebx = fib(n-1)
            add(rega, regb);    // eax = fib(n-2) + fib(n-1)
            ret();
        }},
        6765
    );

    // SSE2 floating-point computation
    test(
        function (a) { with (a) {
            mov(rega, 2);
            cvtsi2sd(xmm0, rega);
            mov(rega, 7);
            cvtsi2sd(xmm1, rega);
            addsd(xmm0, xmm1);
            cvtsd2si(rega, xmm0);
            ret();
        }},
        9
    );

    // Floating-point comparison
    test(
        function (a) { with (a) {
            mov(rega, 10);
            cvtsi2sd(xmm2, rega);       // xmm2 = 10
            mov(rega, 1);
            cvtsi2sd(xmm1, rega);       // xmm1 = 1
            mov(rega, 0);
            cvtsi2sd(xmm0, rega);       // xmm0 = 0
            var LOOP = label('LOOP');
            addsd(xmm0, xmm1);
            ucomisd(xmm0, xmm2);
            jbe(LOOP);
            cvtsd2si(rega, xmm0);
            ret();
        }},
        11
    );
    */
}

