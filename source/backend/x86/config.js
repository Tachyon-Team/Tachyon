/**
@fileOverview
Backend specific configuration for x86

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
@class x86 backend configuration
*/
function x86BackendCfg(is64bit)
{
    if (is64bit === undefined)
    {
        is64bit = false;
    }

    const that = this;
    const reg = x86.Assembler.prototype.register;
    const mem = x86.Assembler.prototype.memory;
    const width = is64bit ? 64 : 32;
    const refByteNb = width >> 3;

    /**
    Register holding stack
    @field
    */
    this.stack   = reg.rsp.subReg(width);

    /**
    Register holding the context object
    @field
    */
    this.context = reg.rcx.subReg(width);

    /**
    Layout for the context object
    @field
    */
    this.ctxLayout = null;

    /**
    Registers available for register allocation.
    @field
    */
    this.physReg = [reg.rax.subReg(width), 
                    reg.rbx.subReg(width),
                    reg.rdx.subReg(width),
                    reg.rsi.subReg(width),
                    reg.rbp.subReg(width),
                    reg.rdi.subReg(width)];

    /**
    Register index for call's return value
    @field
    */
    this.retValIndex = 0;

    /** 
    Register indexes for the corresponding CallInstr operands. The first
    operands will be assigned those registers in their order of 
    appearance. The remaining operands will be passed on the stack.
    The first position corresponds to arg 0 index, the second to arg 1, etc.
    @field
    */
    this.argsIndex = [2, 1, 0, 3];

    //For convenience in the ir-to-asm code, references to registers
    //are derived from the retValIndex and argsIndex 

    /** 
    Register for call's return value
    @field
    */
    this.retValReg = this.physReg[this.retValIndex];

    /**
    Registers for operands of CallInstr
    @field
    */
    this.argsReg = this.argsIndex.map(function (index) { 
        return that.physReg[index]; 
    });

    /** 
    Register to be used for the function pointer during a call
    @field
    */
    this.funcPtrIndex = this.physReg.length - 2;

    /**
    Temporary location in the context for cases where all registers are in use
    @field
    */
    this.tempName = 'temp';

    /**
    Stack alignement number of bytes
    Mandatory to be compatible with Mach OS X ABI function call convention
    @field
    */
    this.stackAlignByteNb = 16;

    /**
    Register type for register allocation
    @field
    */
    this.REG = x86.type.REG;

    /**
    Memory slot type for register allocation
    @field
    */
    this.MEM = x86.type.MEM;

    /**
    Registers used for passing arguments in x64 calling convention
    @field
    */
    this.x64ArgsReg = [reg.rdi, reg.rsi, reg.rdx, reg.rcx, reg.r8, reg.r9];
}

/**
    Creates the layout for the context object used by the backend and 
    assigns it to the ctxLayout field.
*/
x86BackendCfg.prototype.makeContextLayout = function (params)
{
    /**
    Run-time context layout object
    */
    var ctxLayout = new MemLayout("x86ctx", IRType.rptr, undefined, params);

    // Number of arguments passed to the function
    ctxLayout.addField(
        "numargs",
        IRType.pint
    );

    // Argument table for constructing the argument object
    ctxLayout.addField(
        "argtbl",
        IRType.ref
    );

    // Temporary slot for Memory to Memory moves
    ctxLayout.addField(
        'temp',
        IRType.box
    );

    // Slots to spill arguments that might be in registers
    this.physReg.forEach(function (reg, index) {
        ctxLayout.addField(
            'reg' + index,
            IRType.box
        );
    });



    // Finalize the context layout
    ctxLayout.finalize();

    this.ctxLayout = ctxLayout;
};


/**
x86, 32-bit configuration
*/
Target.x86_32 = new Target({
    backend         : 'backendX86',
    backendCfg      : new x86BackendCfg(false),
    endian          : 'little',
    ptrSizeBits     : 32
});

/**
x86, 64-bit configuration
*/
Target.x86_64 = new Target({
    backend         : 'backendX86',
    backendCfg      : new x86BackendCfg(true),
    endian          : 'little',
    ptrSizeBits     : 64
});


