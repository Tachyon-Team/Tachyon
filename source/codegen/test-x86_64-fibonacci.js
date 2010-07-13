var a = new x86_Assembler(x86_Assembler.target.x86_64);
const reg = a.register;
const ESP = reg.esp;
const EAX = reg.eax;
const EBX = reg.ebx;
const RAX = reg.rax;
const RBX = reg.rbx;
const $   = a.immediateValue;
const mem = a.memory;
const _   = function (reg) { return mem(0,reg); };
const _12   = function (reg) { return mem(12,reg); };
const _16   = function (reg) { return mem(16,reg); };

var FIB       = a.codeBlock.label("FIB");
var BASE_CASE = a.codeBlock.label("BASE_CASE");
var RECURSION = a.codeBlock.label("RECURSION");

a.codeBlock.bigEndian = false;

a.
mov($(10), RAX).

label(FIB).
    cmp($(2), RAX).
    jge(RECURSION).

label(BASE_CASE).
    ret().

label(RECURSION).
    push(RAX).
    add($(-1), RAX).
    call(FIB).

    mov(RAX, RBX).
    pop(RAX).
    push(RBX).
    add($(-2), RAX).
    call(FIB).

    pop(RBX).
    add(RBX, RAX).
    ret();

a.codeBlock.assemble();

print(a.codeBlock.listingString());

// For now, v8 cannot be run in 64 bits mode so REX prefixes
// keep being interpreted as DEC instructions
//var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
//print(execMachineCodeBlock(block)); // execute the code generated
//freeMachineCodeBlock(block);
