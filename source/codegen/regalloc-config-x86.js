/**
@fileOverview

Configuration information for the register allocator for each instruction
that might appear in the IR received by the backend.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/**
*   Root object for all allocation information.  Register indexes are
*   indexes into the physical register list containing the explicit
*   platform specific registers.
*/
IRValue.prototype.regAlloc = {

    /** Returns a register index preference for a given operand position */
    opndsRegHint:function (instr, config, position) { return null; },

    /** Returns a register index preference for the output value */
    outRegHint:function (instr, config) { return null; },

    /** Tells if operands of an instruction must be in registers */
    opndsRegRequired:false,

    /** List all register indices used by a given instruction */
    usedRegisters:function (instr, config) { return null; }
};

/**
*   Allocation information for Call Instructions 
*/
CallInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

CallInstr.prototype.regAlloc.opndsRegHint = function (instr, config, position)
{
    if (position > 0 && position - 1 < config.argsIndex.length)
    {
        return config.argsIndex[position - 1];
    } 
    else
    {
        return null;
    }
};

CallInstr.prototype.regAlloc.outRegHint = function (instr, config)
{
    return config.retValIndex;
};

CallInstr.prototype.regAlloc.usedRegisters = function (instr, config)
{
    return arrayRange(config.physReg.length);
};

/**
*   Allocation information for return instructions   
*/
RetInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

RetInstr.prototype.regAlloc.opndsRegHint = function (instr, config, position)
{
    return config.retValIndex;
};

RetInstr.prototype.regAlloc.opndsRegRequired = true;

/**
*   Allocation information for argument value instructions 
*/
ArgValInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

ArgValInstr.prototype.regAlloc.outRegHint = function (instr, config)
{
    if (instr.argIndex < config.argsIndex.length)
    {
        return config.argsIndex[instr.argIndex];
    } 
    else 
    {
        return null;
    }
};

/**
*   Allocation information for get context instructions 
*/
GetCtxInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

GetCtxInstr.prototype.regAlloc.outRegHint = function (instr, config)
{
    return config.context;
};

/**
Allocation information for division instruction
*/
DivInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

DivInstr.prototype.regAlloc.opndsRegHint = function (instr, config, position)
{
    // Operand 0 should be placed in EAX if possible (not guaranteed)
    if (position === 0) 
        return 0;
    else if (position === 1)
        return 1;
    else
        return null;
};

DivInstr.prototype.regAlloc.outRegHint =  function (instr, config)
{ 
    // The output will be in EAX
    return 0; 
};

DivInstr.prototype.regAlloc.usedRegisters = function (instr, config) 
{ 
    // EDX:EAX are reserved for the dividend,
    // EBX is reverved as a scratch register
    return [0,1,3];
};

/**
Allocation information for modulo instruction
*/
ModInstr.prototype.regAlloc = Object.create(DivInstr.prototype.regAlloc);

ModInstr.prototype.regAlloc.outRegHint =  function (instr, config)
{ 
    // The output will be in EDX
    return 3; 
};

/**
Allocation information for multiplication instruction
*/
MulInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

MulInstr.prototype.regAlloc.opndsRegHint = function (instr, config, position)
{
    if (instr.type.isSigned())
        return null;

    // Operand 0 should be placed in EAX if possible (not guaranteed)
    if (position === 0) 
        return 0;
    else
        return null;
};

MulInstr.prototype.regAlloc.outRegHint = function (instr, config)
{
    if (instr.type.isSigned())
        return null;

    // The output will be in EAX
    return 0; 
};

MulInstr.prototype.regAlloc.usedRegisters = function (instr, config) 
{
    if (instr.type.isSigned())
        return null;
 
    // EDX:EAX are reserved for the multiplier,
    return [0,3];
};

/**
Allocation information for multiplication with overflow instruction
*/
MulOvfInstr.prototype.regAlloc = Object.create(MulInstr.prototype.regAlloc);

/**
Allocation information for store instruction
*/
LoadInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

// All operands must be in registers
LoadInstr.prototype.regAlloc.opndsRegRequired = true;

/**
Allocation information for store instruction
*/
StoreInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

// All operands must be in registers
StoreInstr.prototype.regAlloc.opndsRegRequired = true;

