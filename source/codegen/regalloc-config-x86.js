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

GetCtxInstr.prototype.regAlloc.outRegHint = function (instr, config)
{
    return config.context;
};





// TODO: DivInstr
// Unsigned div divides content of EDX:EAX by src and puts result in EAX:EDX
//
// Signed idiv works in the same way
//
// Dividend is uses[0], it doesn't have a fixed register
//
// Quotient (output) is EAX

/*
tu peux ajouter un opndsRegHint avec 0 comme index, ce qui correspond à EAX
dans irToAsm.config.physReg = [EAX, EBX, ECX, EDX, EBP, EDI];

ce n'est pas une garantie que l'operande soit placé dans EAX ***

(10:07:45 PM) erick.lavoie: on peut s'arranger pour que ce soit le cas
(10:08:08 PM) erick.lavoie: en bloquant EAX et EDX pour l'operation,
(10:08:38 PM) erick.lavoie: ça garanti qu'on peut faire un move du registre de l'operande vers EAX s'il n'y est pas déjà
(10:08:55 PM) erick.lavoie: donc ça devrait régler le problème de la contrainte sur l'operande

en retournant l'index 0 (EAX) et l'index 3 (EDX) avec "usedRegisters"
et en mettant useSuppRegs à true

(10:11:11 PM) maximechevalierb@gmail.com/F69F3B26: ok, et j'imagine que le met le ret val hint pour la sortie
(10:11:17 PM) erick.lavoie: oui
(10:11:47 PM) maximechevalierb@gmail.com/F69F3B26: ca guaranti que ca va prendre la sortie dans le bon registre?
(10:12:43 PM) erick.lavoie: le registre devrait être garanti si le registre est bloqué
*/

/**
Allocation information for division instruction
*/
DivInstr.prototype.regAlloc = Object.create(IRValue.prototype.regAlloc);

DivInstr.prototype.regAlloc.opndsRegHint = function (instr, config, position)
{
    // Operand 0 should be placed in EAX if possible
    if (position == 0) 
        return 0;
    else
        return null;
}

DivInstr.prototype.regAlloc.outRegHint =  function (instr, config)
{ 
    // The output will be in EAX
    return 0; 
}

DivInstr.prototype.regAlloc.usedRegisters = function (instr, config) 
{ 
    // EAX and EDX are reserved by this instruction
    return [0, 3]; 
}

// Extra registers must be reserved
DivInstr.prototype.regAlloc.useSuppRegs = true;




// TODO: ModInstr
// Clone DivInstr and change output 
// Remainder (output) is EDX

// TODO: MulInstr
// x86 *unsigned* mul only takes one reg/mem operand
// Other operand is fixed to EAX/RAX
// Dest is fixed to EDX,EAX/RDX,RAX
//
// Signed mul has flexible operands

// TODO: MulOvfInstr
// Same as MulInstr





