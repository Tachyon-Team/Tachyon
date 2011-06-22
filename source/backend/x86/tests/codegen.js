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
Unit tests for the x86 backend.

@author
Maxime Chevalier-Boisvert
*/

/**
Test suite for x86 code generation
*/
tests.x86 = tests.testSuite();

/**
Test x86 instruction encodings
*/
tests.x86.encoding = function ()
{
    // Test encodings for 32-bit and 64-bit
    function test(codeFunc, enc32, enc64)
    {
        // Test either a 32-bit or 64-bit encoding
        function testEnc(enc, x86_64)
        {
            // Report an encoding error
            function encError()
            {
                error(
                    'invalid ' + (x86_64? 64:32) + '-bit encoding for:\n' +
                    assembler.toString() + '\n' +
                    '\n' +
                    'produced:\n' +
                    codeBlock.toString() + ' (' + codeBlock.size + ' bytes)\n' +
                    'expected:\n' +
                    encBlock.toString() + ' (' + encBlock.size + ' bytes)'
                );
            }

            assert (
                typeof enc === 'string',
                'encoding must be provided as a hex string'
            );

            assert (
                enc.length % 2 == 0,
                'encoding string should have multiple of 2 length'
            );

            // Compute the number of bytes in the encoding
            var numBytes = enc.length / 2;

            // Create a code block to write the encoding into
            var encBlock = new CodeBlock(numBytes);

            // For each encoding byte
            for (var i = 0; i < numBytes; ++i)
            {
                var num = parseInt(enc.substr(2*i, 2), 16);

                assert (
                    typeof num === 'number',
                    'invalid encoding string: "' + enc + '"'
                );

                // Write the byte into the code block
                encBlock.writeByte(num);
            }

            // Create an assembler to write code into
            var assembler = new x86.Assembler(x86_64);

            // Produce the assembly
            codeFunc(assembler);

            // Assemble the code to a machine code block
            var codeBlock = assembler.assemble();

            // Check that the encoding length matches
            if (codeBlock.size !== encBlock.size)
                encError();

            // Compare all bytes in the block
            for (var i = 0; i < codeBlock.size; ++i)
            {
                if (codeBlock.readByte(i) !== encBlock.readByte(i))
                    encError();
            }
        }

        assert (
            !(enc64 === false && (enc32 === false || enc32 === undefined)),
            'no 32-bit or 64-bit encoding available for testing'
        );

        // Test the available 32-bit or 64-bit encodings
        if (enc32 !== false)
            testEnc(enc32, false);
        if (enc64 === undefined)
            testEnc(enc32, true);
        if (enc64 !== undefined && enc64 !== false)
            testEnc(enc64, true);
    }

    // add
    test(
        function (a) { a.add(a.al, 3); },
        '0403'
    );
    test(
        function (a) { a.add(a.cl, a.bl); },
        '00D9'
    );
    test(
        function (a) { a.add(a.cl, a.dh); },
        '00F1'
    );
    test(
        function (a) { a.add(a.cl, a.spl); },
        false,
        '4000E1'
    );
    test(
        function (a) { a.add(a.cx, a.bx); },
        '6601D9'
    );
    test(
        function (a) { a.add(a.rdx, a.r14); },
        false,
        '4C01F2'
    );
    test(
        function (a) { a.add(a.edx, a.mem(32, a.eax)); },
        '0310',
        '670310'
    );
    test(
        function (a) { a.add(a.mem(32, a.eax), a.edx); },
        '0110',
        '670110'
    );
    test(
        function (a) { a.add(a.mem(64, a.rax), a.rdx); },
        false, 
        '480110'
    );
    test(
        function (a) { a.add(a.mem(32, a.rax), a.edx); }, 
        false, 
        '0110'
    );

    // and
    test(
        function (a) { a.and(a.ebp, a.r12d); }, 
        false, 
        '4421E5'
    );

    // cmp
    test(
        function (a) { a.cmp(a.ecx, a.edi); },
        '39F9'
    );   
    test(
        function (a) { a.cmp(a.rdx, a.mem(64, a.r12)); },
        false,
        '493B1424'
    );   

    // cpuid
    test(
        function (a) { a.cpuid(); }, 
        '0FA2'
    );    

    // imul
    test(
        function (a) { a.imul(a.edx, a.ecx); },
        '0FAFD1'
    );
    test(
        function (a) { a.imul(a.rsi, a.rdi); },
        false,
        '480FAFF7'
    );
    test(
        function (a) { a.imul(a.r14, a.r9); }, 
        false, 
        '4D0FAFF1'
    );

    // mov
    test(
        function (a) { a.mov(a.eax, 7); }, 
        'B807000000'
    );
    test(
        function (a) { a.mov(a.eax, -3); }, 
        'B8FDFFFFFF'
    );
    test(
        function (a) { a.mov(a.eax, a.ebx); }, 
        '89D8'
    );
    test(
        function (a) { a.mov(a.eax, a.ecx); }, 
        '89C8'
    );
    test(
        function (a) { a.mov(a.ecx, a.mem(32, a.esp, -4)); }, 
        '8B4C24FC',
        '678B4C24FC'
    );

    // movsx
    test(
        function (a) { a.movsx(a.ax, a.al); },
        '660FBEC0'
    );
    test(
        function (a) { a.movsx(a.edx, a.al); },
        '0FBED0'
    );
    test(
        function (a) { a.movsx(a.rax, a.bl); },
        false,
        '480FBEC3'
    );
    test(
        function (a) { a.movsx(a.ecx, a.ax); },
        '0FBFC8'
    );
    test(
        function (a) { a.movsx(a.r11, a.cl); },
        false,
        '4C0FBED9'
    );
    test(
        function (a) { a.movsxd(a.r10, a.mem(32, a.esp, 12)); },
        false,
        '674C6354240C'
    );

    // movzx
    test(
        function (a) { a.movzx(a.si, a.bl); },
        '660FB6F3'
    );
    test(
        function (a) { a.movzx(a.ecx, a.al); },
        '0FB6C8'
    );
    test(
        function (a) { a.movzx(a.rcx, a.bl); },
        false,
        '480FB6CB'
    );
    test(
        function (a) { a.movzx(a.ecx, a.ax); },
        '0FB7C8'
    );
    test(
        function (a) { a.movzx(a.r11, a.cl); },
        false,
        '4C0FB6D9'
    );

    // mul
    test(
        function (a) { a.mul(a.edx); }, 
        'F7E2'
    );
    test(
        function (a) { a.mul(a.r15); },
        false,
        '49F7E7'
    );
    test(
        function (a) { a.mul(a.r10d); },
        false,
        '41F7E2'
    );

    // nop
    test(
        function (a) { a.nop(); }, 
        '90'
    );

    // not
    test(
        function (a) { a.not(a.ax); }, 
        '66F7D0'
    );
    test(
        function (a) { a.not(a.eax); }, 
        'F7D0'
    );
    test(
        function (a) { a.not(a.rax); }, false, 
        '48F7D0'
    );
    test(
        function (a) { a.not(a.r11); }, 
        false, 
        '49F7D3'
    );
    test(
        function (a) { a.not(a.mem(32, a.eax)); }, 
        'F710', 
        '67F710'
    );
    test(
        function (a) { a.not(a.mem(32, a.esi)); },
        'F716', 
        '67F716'
    );
    test(
        function (a) { a.not(a.mem(32, a.edi)); }, 
        'F717', 
        '67F717'
    );
    test(
        function (a) { a.not(a.mem(32, a.edx, 55)); },
        'F75237', 
        '67F75237'
    );
    test(
        function (a) { a.not(a.mem(32, a.edx, 1337)); },
        'F79239050000', 
        '67F79239050000'
    );
    test(
        function (a) { a.not(a.mem(32, a.edx, -55)); },
        'F752C9', 
        '67F752C9'
    );
    test(
        function (a) { a.not(a.mem(32, a.edx, -555)); },
        'F792D5FDFFFF', 
        '67F792D5FDFFFF'
    );
    test(
        function (a) { a.not(a.mem(32, a.eax, 0, a.ebx)); }, 
        'F71418', 
        '67F71418'
    );
    test(
        function (a) { a.not(a.mem(32, a.rax, 0, a.rbx)); }, 
        false, 
        'F71418'
    );
    test(
        function (a) { a.not(a.mem(32, a.rax, 0, a.r12)); }, 
        false, 
        '42F71420'
    );
    test(
        function (a) { a.not(a.mem(32, a.r15, 0, a.r12)); }, 
        false, 
        '43F71427'
    );
    test(
        function (a) { a.not(a.mem(32, a.r15, 5, a.r12)); }, 
        false, 
        '43F7542705'
    );
    test(
        function (a) { a.not(a.mem(32, a.r15, 5, a.r12, 8)); }, 
        false, 
        '43F754E705'
    );
    test(
        function (a) { a.not(a.mem(32, a.r15, 5, a.r13, 8)); }, 
        false, 
        '43F754EF05'
    );
    test(
        function (a) { a.not(a.mem(64, a.r12)); }, 
        false,
        '49F71424'
    );
    test(
        function (a) { a.not(a.mem(32, a.r12, 5, a.r9, 4)); }, 
        false, 
        '43F7548C05'
    );
    test(
        function (a) { a.not(a.mem(32, a.r12, 301, a.r9, 4)); }, 
        false, 
        '43F7948C2D010000'
    );
    test(
        function (a) { a.not(a.mem(32, a.eax, 5, a.edx, 4)); }, 
        'F7549005',
        '67F7549005'
    );
    test(
        function (a) { a.not(a.mem(64, a.eax, 0, a.edx, 2)); },
        false,
        '6748F71450'
    );
    test(
        function (a) { a.not(a.mem(32, a.esp)); },
        'F71424',
        '67F71424'
    );
    test(
        function (a) { a.not(a.mem(32, a.esp, 301)); }, 
        'F794242D010000',
        '67F794242D010000'
    );
    test(
        function (a) { a.not(a.mem(32, a.rsp)); },
        false,
        'F71424'
    );
    test(
        function (a) { a.not(a.mem(32, a.rsp, 0, a.rbx)); },
        false,
        'F7141C'
    );
    test(
        function (a) { a.not(a.mem(32, a.rsp, 3, a.rbx)); },
        false,
        'F7541C03'
    );
    test(
        function (a) { a.not(a.mem(32, a.rsp, 3)); },
        false,
        'F7542403'
    );
    test(
        function (a) { a.not(a.mem(32, a.ebp)); },
        'F75500',
        '67F75500'
    );
    test(
        function (a) { a.not(a.mem(32, a.ebp, 13)); },
        'F7550D',
        '67F7550D'
    );
    test(
        function (a) { a.not(a.mem(32, a.ebp, 13, a.edx)); },
        'F754150D',
        '67F754150D'
    );
    test(
        function (a) { a.not(a.mem(32, a.rip)); },
        false,
        'F79500000000'
    );
    test(
        function (a) { a.not(a.mem(32, a.rip, 13)); },
        false,
        'F7950D000000'
    );
    test(function (a) { a.not(a.mem(32, undefined, 0, a.r8, 8)); }, 
        false, 
        '42F714C500000000'
    );
    test(function (a) { a.not(a.mem(32, undefined, 5)); }, 
        'F71505000000', 
        'F7142505000000'
    );

    // or
    test(
        function (a) { a.or(a.edx, a.esi); },
        '09F2'
    );

    // pop
    test(
        function (a) { a.pop(a.eax); }, 
        '58',
        false
    );
    test(
        function (a) { a.pop(a.ebx); },
        '5B',
        false
    );

    // push
    test(
        function (a) { a.push(a.eax); },
        '50',
        false
    );
    test(
        function (a) { a.push(a.bx); }, 
        '6653', 
        false
    );
    test(
        function (a) { a.push(a.ebx); },
        '53',
        false
    );
    test(
        function (a) { a.push(1); },
        '6A01',
        false
    );

    // ret
    test(
        function (a) { a.ret(); },
        'C3'
    );
    test(
        function (a) { a.ret(5); },
        'C20500'
    );

    // sal
    test(
        function (a) { a.sal(a.cx, 1); },
        '66D1E1'
    );
    test(
        function (a) { a.sal(a.ecx, 1); },
        'D1E1'
    );
    test(
        function (a) { a.sal(a.al, a.cl); },
        'D2E0'
    );
    test(
        function (a) { a.sal(a.ebp, 5); },
        'C1E505'
    );

    // shr
    test(
        function (a) { a.shr(a.r14, 7); },
        false,
        '49C1EE07'
    );

    // test
    test(
        function (a) { a.test(a.al, 4); },
        'A804'
    );
    test(
        function (a) { a.test(a.cl, 255); },
        'F6C1FF'
    );
    test(
        function (a) { a.test(a.dl, 7); },
        'F6C207'
    );
    test(
        function (a) { a.test(a.ah, 12); },
        'F6C40C'
    );
    test(
        function (a) { a.test(a.dil, 9); },
        false,
        '40F6C709'
    );

    // xchg
    test(
        function (a) { a.xchg(a.ax, a.dx); }, 
        '6692'
    );
    test(
        function (a) { a.xchg(a.eax, a.edx); }, 
        '92'
    );
    test(
        function (a) { a.xchg(a.rax, a.r15); },
        false,
        '4997'
    );
    test(
        function (a) { a.xchg(a.r14, a.r15); }, 
        false, 
        '4D87FE'
    );

    // xor
    test(
        function (a) { a.xor(a.eax, a.eax); },
        false, 
        '31C0'
    );

    // Simple loop from 0 to 10
    test(
        function (a) { with (a) {
            mov(eax, 0);
            var LOOP = label('LOOP');
            add(eax, 1);
            cmp(eax, 10);
            jb(LOOP);
            ret();
        }},
        'B80000000083C00183F80A72F8C3'
    );
}

/**
Test the execution of x86 code snippets
*/
tests.x86.codegen = function ()
{
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

    // GP register aliases for 32-bit and 64-bit
    var rega = x86_64? x86.regs.rax:x86.regs.eax;
    var regb = x86_64? x86.regs.rbx:x86.regs.ebx;
    var regc = x86_64? x86.regs.rcx:x86.regs.ecx;
    var regd = x86_64? x86.regs.rdx:x86.regs.edx;

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

    // fib(20)
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
}

