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

a.codeBlock.bigEndian = false;

// In cdecl calling convention, registers ebx, esi, edi, and ebp
// are callee-save
               
a.
gen8(0xD9).
ret();

a.codeBlock.assemble();

print(a.codeBlock.listingString());
