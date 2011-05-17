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
    //return (d <= 15 ? '0' : '') + d.toString(16);
    print(d.toString(16));
    return d.toString(16);
}

/* b is a memoryBlock that contains a 1 or many floats,
   indexed by i, of width w
*/
float2hex = function(b, i, w)
{
    var hex = '';
    if ([32, 64, 80].indexOf(w) != -1)
    {
        switch (w)
        {
            case 32:
                L = 4;
                break;
            case 64:
                L = 8;
                break;
            case 80:
                L = 10;
                break;
        }
        for (var k = L-1; k >= 0; k--) {
            print(dec2hex(64));
            print('i;k;b[i*8 + k]: '+ i + ';' + k + ';' + b[i*8 + k]);
            
            hex += dec2hex(b[i*8 + k]);
        }
    }
    return hex;
}
        
var b1 = allocMemoryBlock(64, false);
var addr = getBlockAddr(b1, 0);
var c1 = (addr[3] << 24) + (addr[2] << 16) + (addr[1] << 8) + addr[0];
//var b2 = allocMemoryBlock(64, false);
//addr = getBlockAddr(b2, 0);
//var c2 = (addr[3] << 24) + (addr[2] << 16) + (addr[1] << 8) + addr[0];

a.codeBlock.bigEndian = false;

a.
fld1().
fld1().fadd(1, true).
fld1().fadd(1, true).
fld1().fadd(1, true).
fld1().fadd(1, true).
fld1().fadd(1, true).
fld1().fadd(1, true).
fld1().fadd(1, true).
mov($(c1), EAX).
fstMem(mem(0,EAX), 64, true).
//fdecstp().
//fstMem(mem(8,EAX), 64, false).
//fdecstp().
//fstMem(mem(16,EAX), 64, false).
//fdecstp().
//fstMem(mem(24,EAX), 64, false).
//fdecstp().
//fstMem(mem(32,EAX), 64, false).
//fdecstp().
//fstMem(mem(40,EAX), 64, false).
//fdecstp().
//fstMem(mem(48,EAX), 64, false).
//fdecstp().
//fstMem(mem(56,EAX), 64, false).
ret();              

a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it

block.link();

var result = execMachineCodeBlock(block); // execute the code generated



print('======');
print(dec2hex(16));
print('======');
print('0x' + float2hex(b1, 0, 64) +  ' expected: 0x');
/*
print('0x' + float2hex(b1, 1, 64) +  ' expected: 0x');
print('0x' + float2hex(b1, 2, 64) +  ' expected: 0x');
print('0x' + float2hex(b1, 3, 64) +  ' expected: 0x');
print('0x' + float2hex(b1, 4, 64) +  ' expected: 0x');
print('0x' + float2hex(b1, 5, 64) +  ' expected: 0x');
print('0x' + float2hex(b1, 6, 64) +  ' expected: 0x');
print('0x' + float2hex(b1, 7, 64) +  ' expected: 0x');
*/
freeMemoryBlock(b1);
//freeMemoryBlock(b2);
