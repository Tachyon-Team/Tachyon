var a = new x86.Assembler();
const reg = a.register;
const ESP = reg.esp;
const EAX = reg.eax;
const $   = a.immediateValue;
const mem = a.memory;
const _   = function (reg) { return mem(0,reg); };
const _12   = function (reg) { return mem(12,reg); };
const _16   = function (reg) { return mem(16,reg); };

a.ld_handlers   = function () {return this.
                                      mov(_16(ESP), EAX);};
a.call_print    = function () {return this.
                                      ld_handlers().
                                      call(_12(EAX));};
a.call_add      = function () {return this.
                                      ld_handlers().
                                      call(_16(EAX));};

a.print         = function (v) { return this.
                                        add    ($(-8),   ESP).
                                        push    (v).
                                        call_print().
                                        add    ($(12),   ESP);};

// Duplicate top of stack
a.tos_dup       = function () {return this.
                                      mov(_(ESP),EAX).
                                      add($(-4),ESP).
                                      mov(EAX,_(ESP));};
a.tos_add       = function () {return this.
                                      mov(_(ESP),EAX).
                                      add($(4),  ESP).
                                      add(EAX,   _(ESP));};

var trueLabel = a.labelObj("IF_TRUE");
var falseLabel = a.labelObj("IF_FALSE");

a.
add($(128), reg.al, 8).
mov($(1), reg.al).
mov($(0), reg.ebx).
cmp($(0), reg.al).
test($(0), reg.al).
lea(_(reg.eax), reg.ebx).
je(trueLabel).

label(falseLabel).
print($(2)).
ret     ().

label(trueLabel).
print($(1)).
ret();



/*
addl    ($(-8),   ESP).
push    ($(42)).
tos_dup ().
tos_add ().
call_print().
addl    ($(12),   ESP).
*/

a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
print(execMachineCodeBlock(block)); // execute the code generated
freeMachineCodeBlock(block);
