var a = new x86.Assembler(x86.target.x86);
const reg = a.register;
const ESP = reg.esp;
const EAX = reg.eax;
const EBX = reg.ebx;
const $   = a.immediateValue;
const mem = a.memory;
const _   = function (reg) { return mem(0,reg); };
const _12   = function (reg) { return mem(12,reg); };
const _16   = function (reg) { return mem(16,reg); };

var FIB       = a.labelObj("FIB");
var BASE_CASE = a.labelObj("BASE_CASE");
var RECURSION = a.labelObj("RECURSION");

a.codeBlock.bigEndian = false;

a.
mov($(40), EAX).

label(FIB).
    cmp($(2), EAX).
    jge(RECURSION).

label(BASE_CASE).
    ret().

label(RECURSION).
    push(EAX).
    add($(-1), EAX).
    call(FIB).

    mov(EAX, EBX).
    pop(EAX).
    push(EBX).
    add($(-2), EAX).
    call(FIB).

    pop(EBX).
    add(EBX, EAX).
    ret();

a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
print(execMachineCodeBlock(block)); // execute the code generated
freeMachineCodeBlock(block);
