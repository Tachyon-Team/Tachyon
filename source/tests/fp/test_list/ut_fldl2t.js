var a = new x86.Assembler(x86.target.x86);
const reg = a.register;
const ESP = reg.esp;
const EAX = reg.eax;
const EBX = reg.ebx;
const ECX = reg.ecx;
const EDX = reg.edx;
const $   = a.immediateValue;
const mem = a.memory;
const _   = function (reg) { return mem(0,reg); };
const _12   = function (reg) { return mem(12,reg); };
const _16   = function (reg) { return mem(16,reg); };

/* decimal [0..255] to hex */
dec2hex = function(d)
{
    return (d <= 15 ? '0' : '') + d.toString(16);
}

/* f is a memoryBlock that contains a float */
float2hex = function(f, w)
{
    hex = '';
    if ([32, 64, 80].indexOf(w) != -1)
    {
        switch (w)
        {
            case 32:
                L = 4;
                break
            case 64:
                L = 8;
                break
            case 80:
                L = 10;
                break
        }
        for (var i = L-1; i >= 0; i--)
            hex += dec2hex(b[i]);
    }
    return hex;
}
        

var b = allocMemoryBlock(8, false);

var addr = getBlockAddr(b, 0);
var c = (addr[3] << 24) + (addr[2] << 16) + (addr[1] << 8) + addr[0];

a.codeBlock.bigEndian = false;

a.
mov($(c), EAX).
fldl2t().
fstMem(mem(0,EAX), 64, false).
ret();              

a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it

block.link();

var result = execMachineCodeBlock(block); // execute the code generated

print('0x' + float2hex(b, 64) +  ' expected: 0x400a934f0979a371');
