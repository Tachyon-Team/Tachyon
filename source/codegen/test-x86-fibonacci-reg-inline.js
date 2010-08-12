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

var GET_RET1  = a.labelObj("GET_RET1");
var SELF1     = a.labelObj("SELF1");
var RET1      = a.labelObj("RET1");
var GET_RET2  = a.labelObj("GET_RET2");
var SELF2     = a.labelObj("SELF2");
var RET2      = a.labelObj("RET2");
var GET_RET3  = a.labelObj("GET_RET3");
var SELF3     = a.labelObj("SELF3");
var RET3      = a.labelObj("RET3");
var FIB       = a.labelObj("FIB");
var BASE_CASE = a.labelObj("BASE_CASE");
var RECURSION = a.labelObj("RECURSION");

a.codeBlock.bigEndian = false;

a.

    /* patch up absolute address references to RET1 */
    call(GET_RET1).
    mov(EAX,mem(-16,EAX)).

    /* patch up absolute address references to RET2 */
    call(GET_RET2).
    mov(EAX,mem(-21,EAX)).

    /* patch up absolute address references to RET3 */
    call(GET_RET3).
    mov(EAX,mem(-21,EAX)).

    mov($(40), EAX).
    mov($(1234567), EDX). /* will be patched with absolute address of RET1 */
    jmp(FIB).
label(GET_RET1).call(SELF1).label(SELF1).pop(EAX).add($(5), EAX).ret().
label(RET1).
    ret().

label(FIB).
    cmp($(2), EAX).
    jge(RECURSION).

label(BASE_CASE).
    jmp(EDX).

label(RECURSION).
    push(EDX).
    push(EAX).
    add($(-1), EAX).
    mov($(1234567), EDX). /* will be patched with absolute address of RET2 */
    cmp($(2), EAX).
    jge(RECURSION).
    jmp(EDX).
label(GET_RET2).call(SELF2).label(SELF2).pop(EAX).add($(5), EAX).ret().
label(RET2).
    mov(EAX, EBX).
    pop(EAX).
    push(EBX).
    add($(-2), EAX).
    mov($(1234567), EDX). /* will be patched with absolute address of RET3 */
    cmp($(2), EAX).
    jge(RECURSION).
    jmp(EDX).
label(GET_RET3).call(SELF3).label(SELF3).pop(EAX).add($(5), EAX).ret().
label(RET3).
    pop(EBX).
    add(EBX, EAX).
    pop(EDX).
    jmp(EDX);

a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
print(execMachineCodeBlock(block)); // execute the code generated
freeMachineCodeBlock(block);
