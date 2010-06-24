/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

//=============================================================================
// IR Core
//
// Implementation of the foundational logic of the IR instructions.
//
//=============================================================================

/**
@class Base class for all IR values
*/
function IRValue()
{
    /**
    Get a string representation of a value's name
    */
    this.getValName = function () { return 'value' };

    /**
    Produce a string representation of this value
    */
    this.toString = this.getValName;
}

/**
@class Represents constant values in the IR
@augments IRInstr
*/
function ConstValue()
{
    /**
    Default toString() implementation for constant instructions
    */
    this.toString = function() { return String(this.value); };

    /**
    Get a string representation of an instruction's value/name.
    Returns the constant's string representation directly.
    */
    this.getValName = this.toString;
}
ConstValue.prototype = new IRValue();

/**
@class Null constant value
@augments ConstValue
*/
function NullConst()
{
   this.value = null;
}
NullConst.prototype = new ConstValue();

/**
@class Undefined constant value
@augments ConstValue
*/
function UndefConst()
{
   this.value = undefined;
}
UndefConst.prototype = new ConstValue();

/**
@class Boolean constant value
@augments ConstValue
*/
function BoolConst(value)
{
    assert (typeof value == 'boolean', 'boolean constant value must be boolean');

    this.value = value;
}
BoolConst.prototype = new ConstValue();

/**
@class Integer constant value
@augments ConstValue
*/
function IntConst(value)
{
    assert (value - Math.floor(value) == 0, 'integer constant value must be integer');

    this.value = value;
}
IntConst.prototype = new ConstValue();

/**
@class Floating-point constant value
@augments ConstValue
*/
function FPConst(value)
{
    assert (typeof value == 'number', 'floating-point constant value must be number');

    this.value = value;
}
FPConst.prototype = new ConstValue();

/**
@class String constant value
@augments ConstValue
*/
function StrConst(value)
{
    assert (typeof value == 'string', 'string constant value must be string');

    this.value = value;
}
StrConst.prototype = new ConstValue();

/**
Get a string representation of a string constant
*/
StrConst.prototype.toString = function()
{
    return '"' + escapeJSString(this.value) + '"';
};

/**
Get a string representation of an instruction's value/name.
Returns the constant's string representation directly.
*/
StrConst.prototype.getValName = StrConst.prototype.toString;

/**
@class Object reference constant value
@augments ConstValue
*/
function ObjRefConst(obj)
{
   this.value = obj;
}
ObjRefConst.prototype = new ConstValue();

/**
@class Base class for all IR instructions
*/
function IRInstr()
{
    /**
    Produce a string representation of this instruction
    */
    this.toString = function ()
    {
        var output = "";

        // If this instruction's value is read, print its output name
        if (this.hasDests())
            output += this.getValName() + ' = ';

        output += this.mnemonic + (this.uses.length? ' ':'');

        for (i = 0; i < this.uses.length; ++i)
        {
            var ins = this.uses[i];

            output += ins.getValName();

            if (ins != this.uses[this.uses.length - 1])            
                output += ", ";
        }

        return output;
    };

    /**
    Get a string representation of an instruction's value/name
    */
    this.getValName = function ()
    {
        // If the output name for this instruction is set
        if (this.outName)
        {
            // Return the output/temporary name
            return this.outName;
        }
        else
        {
            // Return a name based on the instruction id number
            return '$t_' + this.instrId;
        }
    };

    /**
    Copy the instruction's generic properties
    */
    this.baseCopy = function (newInstr)
    {
        // Copy the mnemonic name
        newInstr.mnemonic = this.mnemonic;

        // Copy the output name
        newInstr.outName = this.outName;

        // Copy the instruction id
        newInstr.instrId = this.instrId;

        // The new instruction is orphaned
        newInstr.parentBlock = null;

        return newInstr;
    };

    /**
    Add a new use
    */
    this.addUse = function (use)
    {
        // Create an instance-specific array when necessary
        if (this.uses.length == 0)
            this.uses = [use];
        else
            this.uses.push(use);
    };

    /**
    Remove a use by index
    */
    this.remUse = function (index)
    {
        this.uses.splice(index, 1);
    };

    /**
    Replace a use
    */
    this.replUse = function (oldUse, newUse)
    {
        for (var i = 0; i < this.uses.length; ++i)
        {
            if (this.uses[i] === oldUse)
                this.uses[i] = newUse;
        }
    };

    /**
    Add a new destination
    */
    this.addDest = function (dest)
    {
        if (this.dests.length == 0)
            this.dests = [dest];
        else
            arraySetAdd(this.dests, dest);
    };

    /**
    Remove a destination
    */
    this.remDest = function (dest)
    {
        arraySetRem(this.dests, dest);
    };

    /**
    Replace a destination
    */
    this.replDest = function (oldDest, newDest)
    {
        for (var i = 0; i < this.dests.length; ++i)
        {
            if (this.dests[i] === oldDest)
                this.dests[i] = newdest;
        }
    };

    /**
    Test if this instruction's output is read (has uses)
    */
    this.hasDests = function () { return this.dests.length > 0; };

    /**
    Mnemonic name for this instruction    
    @field
    */
    this.mnemonic = "";

    /**
    Name of this instruction's output
    @field
    */
    this.outName = "";

    /**
    Id number for this instruction
    @field
    */
    this.instrId = 0;

    /**
    Values used/read by this instruction
    @field
    */
    this.uses = [];

    /**
    List of instructions reading this instruction's output
    @field
    */
    this.dests = [];

    /**
    Parent basic block
    @field
    */
    this.parentBlock = null;
}
IRInstr.prototype = new IRValue();

/**
@class SSA phi node instruction
@augments IRInstr
*/
function PhiInstr(values)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = "phi";

    /**
    Inputs to the phi node
    @field
    */
    this.uses = values;
}
PhiInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
PhiInstr.prototype.copy = function ()
{
    var newInstr = new PhiInstr(this.uses.slice(0));
    return this.baseCopy(newInstr);
};

//=============================================================================
// High-Level IR (HIR)
//
// Untyped, abstract operations on implicitly boxed values.
// Close to the original JavaScript language semantics.
//
//=============================================================================

/**
@class Function argument value instruction
@augments IRInstr
*/
function ArgValInstr(argName)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = "arg";

    // Set the output name as the argument name
    this.outName = argName;
}
ArgValInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
ArgValInstr.prototype.copy = function ()
{
    var newInstr = new ArgValInstr(this.outName);
    return this.baseCopy(newInstr);
};

/**
@class Logical negation instruction
@augments IRInstr
*/
function LogNotInstr(inVal)
{
    // Set the mnemonic name for the instruction
    this.mnemonic = "not";

    /**
    Input operand
    @field
    */
    this.uses = [inVal];
}
LogNotInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
LogNotInstr.prototype.copy = function ()
{
    var newInstr = new LogNotInstr(this.uses[0]);
    return this.baseCopy(newInstr);
};

/**
Arithmetic operator kinds
*/
ArithOp =
{
    ADD: 0,
    SUB: 1,
    MUL: 2,
    DIV: 3,
    MOD: 4
};

/**
@class Class for arithmetic instructions
@augments IRInstr
*/
function ArithInstr(arithOp, leftVal, rightVal)
{
    // Set the mnemonic name for the instruction
    switch (arithOp)
    {
        case ArithOp.ADD: this.mnemonic = "add"; break;
        case ArithOp.SUB: this.mnemonic = "sub"; break;
        case ArithOp.MUL: this.mnemonic = "mul"; break;
        case ArithOp.DIV: this.mnemonic = "div"; break;
        case ArithOp.MOD: this.mnemonic = "mod"; break;
    }

    /**
    Arithmetic operator
    @field
    */
    this.arithOp = arithOp;

    /**
    Arithmetic operands
    @field
    */
    this.uses = [leftVal, rightVal];
}
ArithInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
ArithInstr.prototype.copy = function ()
{
    var newInstr = new ArithInstr(this.arithOp, this.uses[0], this.uses[1]);
    return this.baseCopy(newInstr);
};

/**
Bitwise operator kinds
*/
BitOp =
{
    AND:    0,
    OR:     1,
    XOR:    2,
    NOT:    3,
    LSFT:   4,
    RSFT:   5,
    RSFTU:  6
};

/**
@class Class for bitwise instructions
@augments IRInstr
*/
function BitInstr(bitOp, leftVal, rightVal)
{
    // Set the mnemonic name for the instruction
    switch (bitOp)
    {
        case ArithOp.AND:   this.mnemonic = "and";      break;
        case ArithOp.OR:    this.mnemonic = "or";       break;
        case ArithOp.XOR:   this.mnemonic = "xor";      break;
        case ArithOp.NOT:   this.mnemonic = "not";      break;
        case ArithOp.LSFT:  this.mnemonic = "lsft";     break;
        case ArithOp.RSFT:  this.mnemonic = "rsft";     break;
        case ArithOp.RSFTU: this.mnemonic = "rsftu";    break;
    }

    /**
    Arithmetic operator
    @field
    */
    this.bitOp = bitOp;

    /**
    Arithmetic operands
    @field
    */
    this.uses = [leftVal, rightVal];
}
BitInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
BitInstr.prototype.copy = function ()
{
    var newInstr = new BitInstr(this.bitOp, this.uses[0], this.uses[1]);
    return this.baseCopy(newInstr);
};

/**
Comparison operator kinds
*/
CompOp =
{
    LT:     0,
    LTE:    1,
    GT:     2,
    GTE:    3,
    EQ:     4,
    NE:     5,
    SEQ:    6,
    NSEQ:   7
};

/**
@class Class for comparison instructions
@augments IRInstr
*/
function CompInstr(compOp, leftVal, rightVal)
{
    // Set the mnemonic name for the instruction
    switch (compOp)
    {
        case ArithOp.LT:    this.mnemonic = "lt";   break;
        case ArithOp.LTE:   this.mnemonic = "lte";  break;
        case ArithOp.GT:    this.mnemonic = "gt";   break;
        case ArithOp.GTE:   this.mnemonic = "gte";  break;
        case ArithOp.EQ:    this.mnemonic = "eq";   break;
        case ArithOp.NE:    this.mnemonic = "ne";   break;
        case ArithOp.SEQ:   this.mnemonic = "seq";  break;
        case ArithOp.NSEQ:  this.mnemonic = "nseq"; break;
    }

    /**
    Comparison operator
    @field
    */
    this.compOp = compOp;

    /**
    Arithmetic operands
    @field
    */
    this.uses = [leftVal, rightVal];
}
CompInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
CompInstr.prototype.copy = function ()
{
    var newInstr = new CompInstr(this.compOp, this.uses[0], this.uses[1]);
    return this.baseCopy(newInstr);
};

/**
@class Type query instruction
@augments IRInstr
*/
function TypeOfInstr(value)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'typeof';

    /**
    JS value
    @field
    */
    this.uses = [value];
}
TypeOfInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
TypeOfInstr.prototype.copy = function ()
{
    return this.baseCopy(new TypeOfInstr(this.uses[0]));
};

/**
@class Instance/class query instruction
@augments IRInstr
*/
function InstOfInstr(testObj, classObj)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'instanceof';

    /**
    Test object and class object
    @field
    */
    this.uses = [testObj, classObj];
}
InstOfInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
InstOfInstr.prototype.copy = function ()
{
    return this.baseCopy(new InstOfInstr(this.uses[0], this.uses[1]));
};

/**
@class Property set with value for field name
@augments IRInstr
*/
function SetPropValInstr(objVal, nameVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'setprop_val';

    /**
    Object and field name values
    @field
    */
    this.uses = [objVal, nameVal];
}
SetPropValInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
SetPropValInstr.prototype.copy = function ()
{
    var newInstr = new SetPropValInstr(this.uses[0], this.uses[1]);
    return this.baseCopy(newInstr);
};

/**
@class Property get with value for field name
@augments IRInstr
*/
function GetPropValInstr(objVal, nameVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'getprop_val';

    /**
    Object and field name values
    @field
    */
    this.uses = [objVal, nameVal];
}
GetPropValInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
GetPropValInstr.prototype.copy = function ()
{
    var newInstr = new GetPropValInstr(this.uses[0], this.uses[1]);
    return this.baseCopy(newInstr);
};

/**
@class Base class for branching instructions.
@augments IRInstr
*/
function BranchInstr()
{
    /**
    Potential branch target basic blocks
    @field
    */
    this.targets = [];
}
BranchInstr.prototype = new IRInstr();

/**
@class Unconditional jump instruction
@augments BranchInstr
*/
function JumpInstr(targetBlock)
{
    /**
    Target basic block
    @field
    */
    this.targets = [targetBlock];
}
JumpInstr.prototype = new BranchInstr();

/**
Obtain a string representation
*/
JumpInstr.prototype.toString = function()
{
    return "jump " + this.targets[0].getBlockName();
};

/**
Make a shallow copy of the instruction
*/
JumpInstr.prototype.copy = function ()
{
    var newInstr = new JumpInstr(this.targets[0]);
    return this.baseCopy(newInstr);
};

/**
@class If conditional test instruction
@augments BranchInstr
*/
function IfInstr(testVal, trueBlock, falseBlock)
{
    /**
    Test value for the branch condition
    @field
    */
    this.uses = [testVal];

    /**
    Branch targets for the true and false cases
    @field
    */
    this.targets = [trueBlock, falseBlock];
}
IfInstr.prototype = new BranchInstr();

/**
Obtain a string representation
*/
IfInstr.prototype.toString = function()
{
    return  "if " + this.uses[0].getValName() +
            " then " + this.targets[0].getBlockName() +
            " else " + this.targets[1].getBlockName()
    ;
};

/**
Make a shallow copy of the instruction
*/
IfInstr.prototype.copy = function ()
{
    var newInstr = new IfInstr(this.uses[0], this.targets[0], this.targets[1]);
    return this.baseCopy(newInstr);
};

/**
@class Function return instruction
@augments BranchInstr
*/
function RetInstr(retVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'ret';

    /**
    Return value, can be undefined
    @field
    */
    this.uses = [retVal];
}
RetInstr.prototype = new BranchInstr();

/**
Make a shallow copy of the instruction
*/
RetInstr.prototype.copy = function ()
{
    var newInstr = new RetInstr(this.uses[0]);
    return this.baseCopy(newInstr);
};

/**
@class Exception throw to exception handler. Handler may be left undefined for
interprocedural throw.
@augments BranchInstr
*/
function ThrowInstr(excVal, catchBlock)
{
    // Set the target block if a catch block is specified
    if (catchBlock == undefined)
        catchBlock = null;

    /**
    Exception value to be thrown
    @field
    */
    this.uses = [excVal];

    /**
    Catch block for this throw, may be null if unspecified
    @field
    */
    this.targets = [catchBlock];
}
ThrowInstr.prototype = new BranchInstr();

/**
Produce a string representation of the throw instruction
*/
ThrowInstr.prototype.toString = function ()
{
    var output = 'throw ' + excVal.getValName();

    if (this.targets[0] != null)
        output += 'to ' + this.targets[0].getBlockName();

    return output;
};

/**
Make a shallow copy of the instruction
*/
ThrowInstr.prototype.copy = function ()
{
    var newInstr = new ThrowInstr(this.uses[0], this.targets[0]);
    return this.baseCopy(newInstr);
};

/**
@class Exception handler instruction, for function calls. Handler may be left
undefined for interprocedural throw.
@augments BranchInstr
*/
OnExcInst = function (contBlock, catchBlock)
{
    /**
    Catch block and continue block for the exception handler
    @field
    */
    this.targets = [contBlock, catchBlock];
}
OnExcInst.prototype = new BranchInstr();

/**
Produce a string representation of the exception handler instruction
*/
OnExcInst.prototype.toString = function ()
{
    var output = 'on_exc throw';

    if (this.targets[0] != null)
        output += ' to ' + this.targets[1].getBlockName();

    output += ' else ' + this.targets[0].getBlockName();

    return output;
};

/**
Make a shallow copy of the instruction
*/
OnExcInst.prototype.copy = function ()
{
    var newInstr = new OnExcInst(this.targets[0], this.targets[1]);
    return this.baseCopy(newInstr);
};

/**
@class Exception value catch
@augments IRInstr
*/
function CatchInstr()
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'catch';
}
CatchInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
CatchInstr.prototype.copy = function ()
{
    var newInstr = new CatchInstr();
    return this.baseCopy(newInstr);
};

/**
@class Call with function object reference
@augments IRInstr
*/
function CallRefInstr(funcVal, thisVal, paramVals)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'call';

    /**
    Function value, this value and parameter values
    @field
    */
    this.uses = [funcVal, thisVal].concat(paramVals);
}
CallRefInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
CallRefInstr.prototype.copy = function ()
{
    var newInstr = new CallRefInstr(this.uses[0], this.uses[1], this.uses.slice[2]);
    return this.baseCopy(newInstr);
};

/**
@class Constructor call with function object reference
@augments IRInstr
*/
function ConstructRefInstr(funcVal, paramVals)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'construct';

    /**
    Function value, this value and parameter values
    @field
    */
    this.uses = [funcVal].concat(paramVals);
}
ConstructRefInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
ConstructRefInstr.prototype.copy = function ()
{
    var newInstr = new ConstructRefInstr(this.uses[0], this.uses.slice[1]);
    return this.baseCopy(newInstr);
};

/**
@class Instruction to create a new, empty object
@augments IRInstr
*/
function NewObjInstr()
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'new_obj';
}
NewObjInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
NewObjInstr.prototype.copy = function ()
{
    var newInstr = new NewObjInstr();
    return this.baseCopy(newInstr);
};

//=============================================================================
// Medium-Level IR (MIR)
//
// Introduction of specialized instruction forms.
// Type-specialized instruction variants.
//
//=============================================================================


// TODO: complete this section

// IR value type enumeration
IRTypes =
{
    BOXED:      0,
    POINTER:    1,
    INT8:       2,
    INT16:      3,
    INT32:      4,
    INT64:      5,
    FLOAT64:    6
};

/**
Get the name of an IR type
*/
function getIRTypeName()
{
}

/**
Get the size of an IR type in bytes
*/
function getIRTypeSize()
{
}







//=============================================================================
// Low-Level IR (LIR)
//
// Introduction of a pointer type.
// Access to memory representation.
// Calculation of memory offsets directly.
// Store and load instructions.
// Still machine agnostic. No machine-specific instructions or reg. alloc.
//
//=============================================================================

// TODO: complete this section

// TODO: load/store values by type instead of by bit width???

/**
@class Load a value from memory
@augments IRInstr
*/
function LoadInstr(numBits, ptr)
{
    // Ensure that the number of bits specified is valid
    assert (
        numBits == 8 || numBits == 16 || numBits == 32 || numBits == 64,
        'invalid number of bits for load instruction'
    );

    // Set the mnemonic name for the instruction
    this.mnemonic = 'load' + numBits;

    /**
    Number of bits to load
    @field
    */
    this.numBits = numBits;

    /**
    Address of the value to load
    @field
    */
    this.uses = [ptr];
}
LoadInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
LoadInstr.prototype.copy = function ()
{
    return this.baseCopy(new LoadInstr(this.numBits, this.uses[0]));
};

/**
@class Store a value to memory
@augments IRInstr
*/
function StoreInstr(numBits, ptr, value)
{
    // Ensure that the number of bits specified is valid
    assert (
        numBits == 8 || numBits == 16 || numBits == 32 || numBits == 64,
        'invalid number of bits for load instruction'
    );

    // Set the mnemonic name for the instruction
    this.mnemonic = 'store' + numBits;

    /**
    Number of bits to load
    @field
    */
    this.numBits = numBits;

    /**
    Memory address, value to store
    @field
    */
    this.uses = [ptr, value];
}
StoreInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
StoreInstr.prototype.copy = function ()
{
    return this.baseCopy(new StoreInstr(this.numBits, this.uses[0], this.uses[1]));
};

