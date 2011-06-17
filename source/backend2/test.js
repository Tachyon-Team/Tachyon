var assembler = new x86.Assembler(false);

with (assembler)
{
    //push(eax);
    //push(ebx);

    //add(eax, ebx);

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
