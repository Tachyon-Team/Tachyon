/**
@fileOverview
Testbed for x86 assembly syntax exploration

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

(function () { // local namespace
// Alias
const x86 = x86_Assembler.prototype;

// Print behavior for diverse intruction types
x86.twoOpndOp        = function (name, source, destination)
{
    write(name + " ");
    print(source.toString() + ", " + destination.toString()); 
    return this;
};

x86.oneOpndOp       = function (name, destination)
{
    write(name + " " );
    print(destination.toString());
    return this;
};

x86.noOpndOp = function (name)
{
    print(name);
    return this;
}

// Instructions supported
x86.add  = function (source, destination) { return this.twoOpndOp("addl", source, destination); };
x86.mov  = function (source, destination) { return this.twoOpndOp("movl", source, destination); };

x86.push = function (destination)         { return this.oneOpndOp("push", destination); };
x86.call = function (destination)         { return this.oneOpndOp("call", destination); };
x86.jmp  = function (destination)         { return this.oneOpndOp("jmp ", destination); };

x86.ret  = function ()                    { return this.noOpndOp("ret"); };

x86.label = function (name)               { print(name + ":"); return this; };

// Basic types declaration
x86.IMM = 0;
x86.REG = 1;
x86.MEM = 2;
x86.LBL = 3;

// Returns an immediate object
x86.$ = function (value) 
{
    var o = {};
    o.type = x86.IMM;
    o.value = value;
    o.toString = function () { return "$" + this.value; };
    return o;
}

// Returns a register object
x86.register = function ( name )
{
    var o = {};
    o.type = x86.REG;
    o.name = name;
    o.toString = function () { return "%" + this.name; };
    return o;
}

// Declaration of supported registers
x86.register.ESP = x86.register( "esp" );
x86.register.EAX = x86.register( "eax" );

// Returns a memory object
x86.memory = function ( displacement, scale, index, base )
{
    var o = {}; 
    o.type = x86.MEM;
    o.displacement = displacement;
    o.scale = scale;
    o.index = index;
    o.base = base;

    o.toString = function ()
    {
        return ( displacement ? displacement : "" ) + "(" +
               base.toString() +
               ( index ? ", " + index.toString() : "") +
               ( scale ? ", " + scale : "") +
               ")";
    }
    return o;
}

// Syntactic sugar for displacements when using memory operands
x86.memory._ = function ( base, index, scale ) { return x86.memory( undefined, scale, index, base); };
x86.memory._12 = function ( base, index, scale ) { return x86.memory( 12, scale, index, base); };
x86.memory._16 = function ( base, index, scale ) { return x86.memory( 16, scale, index, base); };


})(); // end of local namespace


function test()
{
    // Constant declarations for a cleaner syntax when writing 
    // assembly code by hand
    const $ = x86_Assembler.prototype.$;
    const ESP = x86_Assembler.prototype.register.ESP;
    const EAX = x86_Assembler.prototype.register.EAX;
    const _ = x86_Assembler.prototype.memory._;
    const _12 = x86_Assembler.prototype.memory._12;
    const _16 = x86_Assembler.prototype.memory._16;


    var a = new x86_Assembler();

    // Macro declarations for use in this scope only
    a.dup = function () 
    { 
        return this.
        mov (_(ESP), EAX).
        add ($(-4), ESP).
        mov (EAX, _(ESP));
    }
    
    // Label declaration
    var DUP_TEST = "DUP_TEST";

    // Assembly code example
    a. 
    // Print 42
    add  ($(-8), ESP).
    mov  ($(42), EAX).
    push (EAX).
    mov  (_16(ESP), EAX).
    call (_12(EAX)).
    add  ($(12), ESP).

    // 11 + 22 using add handler
    add  ($(-4), ESP).
    push ($(11)).
    push ($(22)).
    mov  (_16(ESP), EAX).
    call (_16(EAX)).
    add  ($(12), ESP).

    // 2 * 11 using add handler
    add  ($(-4), ESP).
    push ($(11)).
    dup  ().
    mov  (_16(ESP), EAX).
    call (_16(EAX)).
    add  ($(12), ESP).
    
    // Simple jump test
    jmp  (DUP_TEST).
    label (DUP_TEST).  

    ret  ();          
}

test();
