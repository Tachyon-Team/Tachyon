function test32bitEnc()
{
    // Test an encoding
    function test(codeFunc, expectEnc)
    {
        // Report an encoding error
        function encError()
        {
            var expectBlock = new CodeBlock(expectEnc.length);
            for (var i = 0; i < expectEnc.length; ++i)
                expectBlock.writeByte(expectEnc[i]);

            error(
                'invalid encoding for:\n' +
                assembler.toString() + '\n' +
                '\n' +
                'produced:\n' +
                codeBlock.toString() + ' (' + codeBlock.size + ' bytes)\n' +
                'expected:\n' +
                expectBlock.toString() + ' (' + expectBlock.size + ' bytes)'
            );
        }

        var assembler = new x86.Assembler(false);

        // Produce the assembly
        codeFunc(assembler);

        // Assemble the code to a machine code block
        var codeBlock = assembler.assemble();

        // Check that the encoding length matches
        if (codeBlock.size !== expectEnc.length)
            encError();

        // Compare all bytes in the block
        for (var i = 0; i < codeBlock.size; ++i)
        {
            if (codeBlock.readByte(i) !== expectEnc[i])
            {
                print(codeBlock.readByte(i));
                print(expectEnc[i]);
                encError();
            }
        }
    }

    // add
    test(function (a) { a.add(a.al, 3); }, [0x04, 0x03]);
    // TODO: this actually uses a ModR/M byte
    //test(function (a) { a.add(a.eax, 3); }, [0x05, 0x00, 0x00, 0x00, 0x03]);

    // nop
    test(function (a) { a.nop(); }, [0x90]);

    // pop
    test(function (a) { a.pop(a.eax); }, [0x58]);
    test(function (a) { a.pop(a.ebx); }, [0x5B]);

    // push
    test(function (a) { a.push(a.eax); }, [0x50]);
    test(function (a) { a.push(a.ebx); }, [0x53]);

    // ret
    test(function (a) { a.ret(); }, [0xC3]);
    test(function (a) { a.ret(5); }, [0xC2, 0x00, 0x05]);







}

test32bitEnc();






/*
var assembler = new x86.Assembler(false);

with (assembler)
{
    //push(eax);
    //push(ebx);

    //add(eax, ebx);

    push(eax);

    nop();

    ret();

    //pop(ebx);
    //pop(eax);
}

print('');
print('assembly: ');
print(assembler.toString(true));

// Assemble to code block
var codeBlock = assembler.assemble();

print('');
print('code block: ');
print(codeBlock.size + ' bytes');
print(codeBlock);
*/



/*
TODO: call, with callTachyonFFI?
var ret = callTachyonFFI(
    ['void*', 'int'],
    'void*',
    funcPtr, 
    ctxPtr,
    [heapAddr, heapSize]
);
*/



