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

var FIB       = a.labelObj("FIB");
var BASE_CASE = a.labelObj("BASE_CASE");
var RECURSION = a.labelObj("RECURSION");

a.codeBlock.bigEndian = false;

// In cdecl calling convention, registers ebx, esi, edi, and ebp
// are callee-save

a.push(reg.ebx)
a.push(reg.esi);
a.push(reg.edi);
a.push(reg.ebp);


/*
a.mov($(40), EAX);
a.mov($(2), EBX);
a.imul(EBX, EAX);
*/

/*
a.mov($(2), EBX);
a.imul(EBX, EAX, $(3));
*/

/*
a.mov($(2), EBX);
a.imul(EBX, EAX, $(-500));
*/

/*
a.mov($(40), EAX);
a.mov($(2), EBX);
a.imul(EBX, EAX);
*/

/*
a.mov($(2), mem(-4,ESP), 32);
a.mov($(40), EAX);
a.imul(mem(-4,ESP), EAX);
*/


a.mov($(500), EAX);
a.mov($(200), ECX);

a.imul(ECX);

a.pop(reg.ebp);
a.pop(reg.edi);
a.pop(reg.esi);
a.pop(reg.ebx);
a.ret();


a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it

var result = execMachineCodeBlock(block); // execute the code generated

print('result: ' + result);

freeMachineCodeBlock(block);

