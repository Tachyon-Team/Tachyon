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

    /** Returns a register index preference for the return value */
    retValRegHint:function (instr, config) { return null; },

    /** Tells if operands of an instruction must be in registers */
    opndsRegRequired:false,

    /** Tells if supplementary registers are used by the instruction */
    useSuppRegs:false,

    /** List all register indexes used by a given instruction */
    usedRegisters:function (instr, config) { return null; }
};

/**
*   Allocation information for Call Instructions 
*/
CallInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

CallInstr.prototype.regAlloc.opndsRegHint = function (instr, config, position)
{
    if (position < config.argsIndex.length)
    {
        return config.argsIndex[position];
    } else
    {
        return null;
    }
};

CallInstr.prototype.regAlloc.retValRegHint = function (instr, config)
{
    return config.retValIndex;
};

CallInstr.prototype.regAlloc.useSuppRegs = true;

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

ArgValInstr.prototype.regAlloc.retValRegHint = function (instr, config)
{
    return config.argsIndex[instr.argIndex];
};

/**
*   Allocation information for get property instructions 
*/
//GetPropValInstr.prototype.regAlloc = Object.create(CallInstr.prototype.regAlloc);

//GetPropValInstr.prototype.regAlloc.opndsRegRequired = true;

/**
*   Allocation information for put property instructions 
*/
//PutPropValInstr.prototype.regAlloc = Object.create(CallInstr.prototype.regAlloc);

//PutPropValInstr.prototype.regAlloc.opndsRegRequired = true;

/**
*   Allocation information for get context instructions 
*/
GetCtxInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

GetCtxInstr.prototype.regAlloc.retValRegHint = function (instr, config)
{
    return config.context;
};

