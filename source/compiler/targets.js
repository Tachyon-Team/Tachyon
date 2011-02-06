/**
@fileOverview
Compiler target configuration.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
@class Description of target architecture for compilation
*/
function Target(cfgObj)
{
    assert (
        cfgObj.endian === 'little' || cfgObj.endian === 'big',
        'invalid endian specified'
    );

    assert (
        cfgObj.ptrSizeBits === 32 || cfgObj.ptrSizeBits === 64,
        'invalid pointer size specified'
    );

    /**
    Back-end name    
    @field
    */
    this.backend = cfgObj.backend;

    /**
    Back-end specific configuration
    @field
    */
    this.backendCfg = cfgObj.backendCfg;   

    /**
    Endianness of the target platform
    @field
    */
    this.endian = cfgObj.endian;

    /**
    Pointer size of the target platform, in bits
    @field
    */
    this.ptrSizeBits = cfgObj.ptrSizeBits;

    /**
    Pointer size of the target platform, in bytes
    @field
    */
    this.ptrSizeBytes = this.ptrSizeBits >> 3;    
}

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
    Temporary location for cases where all registers are in use
    @field
    */
    this.temp = mem(3*refByteNb, this.context);

    /**
    Stack alignement number of bytes
    Mandatory to be compatible with Mach OS X ABI function call convention
    @field
    */
    this.stackAlignByteNb = 16;

    return this;
}

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

