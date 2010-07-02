/**
@fileOverview
Testbed for x86 assembly execution

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

(function () { // local namespace

// Alias
const x86 = x86_Assembler.prototype;

x86.addImm8ESP = function (n)  {return this.gen8(0x83).gen8(0xc4).gen8(n);}
x86.instr1     = function () 
{ 
    return this.gen8(0x8b).gen8(0x44).gen8(0x24).gen8(0x10); 
};

x86.instr2     = function ()    {return this.gen8(0xff).gen8(0x50).gen8(0x0c);}
x86.instr3     = function ()    {return this.gen8(0xff).gen8(0x50).gen8(0x10);}
x86.movImm8EAX = function (n)   {return this.gen8(0xb8).gen32(n);}
x86.pushEAX    = function ()    {return this.gen8(0x50);}
x86.pushCS     = function ()    {return this.gen8(0x0E);}
x86.pushImm8   = function (k)   {return this.gen8(0x6a).gen8(k);}
x86.movESP_EAX = function (n)   
{
    return this.
           gen8(0x8b).gen8(0x44).gen8(0x24).gen8(n);
};

x86.dup        = function () 
{
    return this.
           movESP_EAX(0).
           addImm8ESP(-4).
           // movl %eax, (%esp)  
           gen8(0x89).gen8(0x04).gen8(0x24);   
};

x86.add        = function()
{
    return this.
           // movl (%esp), %eax
           gen8(0x8b).gen8(0x04).gen8(0x24).     
           addImm8ESP(4).
           // addl %eax, (%esp)
           gen8(0x01).gen8(0x04).gen8(0x24); 
};

x86.print      = function()     {return this.instr1().instr2();}   

})(); // end of local namespace

function test()
{
    var a = new x86_Assembler();
    // generate some code
    a.addImm8ESP(-8); // addl  $-8,%esp      maintain 16 byte alignment for Mach!:
    a.pushImm8(123);  // push  $123          push 123
    a.instr1();       // movl  16(%esp),%eax get pointer to handlers
    a.instr2();       // call  *12(%eax)     call handlers[1]
    a.addImm8ESP(12); // addl  $12,%esp      maintain 16 byte alignment

    // Print an arbitrary number
    a.addImm8ESP(-8); // addl  $-8,%esp      maintain 16 byte alignment
    a.movImm8EAX(42); // movl  $42, %eax
    a.pushEAX();
    a.instr1();       // movl  16(%esp),%eax get pointer to handlers
    a.instr2();       // call  *12(%eax)     call handlers[1]
    a.addImm8ESP(12); // addl  $12,%esp      maintain 16 byte alignment

    // Print return address as a decimal value
    a.movESP_EAX(0);  // movl  (%esp), %eax
    a.addImm8ESP(-8); // addl  $-8,%esp      maintain 16 byte alignment
    a.pushEAX();      // push  %eax
    a.instr1();       // movl  16(%esp),%eax get pointer to handlers
    a.instr2();       // call  *12(%eax)     call handlers[1]
    a.addImm8ESP(12); // addl  $12,%esp      maintain 16 byte alignment

    // Inline add
    a.
    addImm8ESP(-8).
    pushImm8(42).
    dup().
    add().
    print().
    addImm8ESP(12);

    a.addImm8ESP(-4); // addl  $-4,%esp      maintain 16 byte alignment
    a.pushImm8(11);   // push  $11           push 11
    a.pushImm8(22);   // push  $22           push 22
    a.instr1();       // movl  16(%esp),%eax get pointer to handlers
    a.instr3();       // call  *16(%eax)     call handlers[2]
    a.addImm8ESP(12); // addl  $12,%esp      maintain 16 byte alignment

    a.ret();

    var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it

    print(execMachineCodeBlock(block)); // execute the code generated

    freeMachineCodeBlock(block);

    // To try it out do (with a d8 extended with d8-extensions.cc):
    //
    //   % d8 asm.js asm-x86.js
    //   x = 123
    //   33
    //
    // The line "x = 123" was written by the printf in handler1 of
    // file d8-extensions.cc .  The value 33, printed by the JavaScript
    // code is the sum of 11 and 22 computed by handler2.
}

test();
