/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Base class for all instructions
*/
function BaseInstr()
{
    /**
    Produce a string representation of this instruction
    */
    this.toString = function ()
    {
        var output = "";

        // If this instruction's value is read, print its output name
        if (this.hasDests())
            output += (this.outName? (this.outName + " = "):"")

        output += this.mnemonic + " ";

        for (i = 0; i < this.uses.length; ++i)
        {
            var ins = this.uses[i];

            output += ins.valToString();

            if (ins != this.uses[this.uses.length - 1])            
                output += ", ";
        }

        return output;
    }

    /**
    Get a string representation of an instruction's value/name
    */
    this.valToString = function ()
    {
        // Return the output/temporary name for this instruction
        return this.outName;
    }

    /**
    Test if this instruction's output is read (has uses)
    */
    this.hasDests = function () { return this.dests.length > 0; }

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

/**
@class Base class for constants, these are treated like regular instructions
@augments BaseInstr
*/
function ConstInstr()
{
    /**
    Default toString() implementation for constant instructions
    */
    this.toString = function() { return String(this.value); }

    /**
    Get a string representation of an instruction's value/name.
    Returns the constant's string representation directly.
    */
    this.valToString = this.toString;
}
ConstInstr.prototype = new BaseInstr();

/**
@class Null constant value
@augments ConstInstr
*/
function NullConst()
{
   this.value = null;
}
NullConst.prototype = new ConstInstr();

/**
@class Undefined constant value
@augments ConstInstr
*/
function UndefConst()
{
   this.value = undefined;
}
UndefConst.prototype = new ConstInstr();

/**
@class Boolean constant value
@augments ConstInstr
*/
function BoolConst(value)
{
    assert (typeof value == 'boolean', 'boolean constant value must be boolean');

    this.value = value;
}
BoolConst.prototype = new ConstInstr();

/**
@class Integer constant value
@augments ConstInstr
*/
function IntConst(value)
{
    assert (value - Math.floor(value) == 0, 'integer constant value must be integer');

    this.value = value;
}
IntConst.prototype = new ConstInstr();

/**
@class Floating-point constant value
@augments ConstInstr
*/
function FPConst(value)
{
    assert (typeof value == 'number', 'floating-point constant value must be number');

    this.value = value;
}
FPConst.prototype = new ConstInstr();

/**
@class String constant value
@augments ConstInstr
*/
function StrConst(value)
{
    assert (typeof value == 'string', 'string constant value must be string');

    this.value = value;

    /**
    Get a string representation of a string constant
    */
    this.toString = function() { return '"' + String(this.value) + '"'; }

    /**
    Get a string representation of an instruction's value/name.
    Returns the constant's string representation directly.
    */
    this.valToString = this.toString;
}
StrConst.prototype = new ConstInstr();

/**
@class Object reference constant value
@augments ConstInstr
*/
function ObjRefConst(obj)
{
   this.value = obj;
}
ObjRefConst.prototype = new ConstInstr();

/**
@class SSA phi node instruction
@augments BaseInstr
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
PhiInstr.prototype = new BaseInstr();

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
@augments BaseInstr
*/
function ArithInstr(arithOp, leftVal, rightVal)
{
    // Set the mnemonic name for the instruction
    switch (arithOp)
    {
        case ArithOp.ADD: this.mnemonic = "add"; break;
        case ArithOp.SUB: this.mnemonic = "sub"; break;
        case ArithOp.MUL: this.mnemonic = "mod"; break;
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
ArithInstr.prototype = new BaseInstr();

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
@augments BaseInstr
*/
function BitwiseInstr(bitOp, leftVal, rightVal)
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
BitwiseInstr.prototype = new BaseInstr();

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
@augments BaseInstr
*/
function CompInstr(compOp, leftVal, rightVal)
{
    // Set the mnemonic name for the instruction
    switch (compOp)
    {
        case ArithOp.LT:    this.mnemonic = "<";    break;
        case ArithOp.LTE:   this.mnemonic = "<=";   break;
        case ArithOp.GT:    this.mnemonic = ">";    break;
        case ArithOp.GTE:   this.mnemonic = ">=";   break;
        case ArithOp.EQ:    this.mnemonic = "==";   break;
        case ArithOp.NE:    this.mnemonic = "!=";   break;
        case ArithOp.SEQ:   this.mnemonic = "===";  break;
        case ArithOp.NSEQ:  this.mnemonic = "!==";  break;
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
CompInstr.prototype = new BaseInstr();

/**
@class Property set with value for field name
@augments BaseInstr
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
SetPropValInstr.prototype = new BaseInstr();

/**
@class Property get with value for field name
@augments BaseInstr
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
GetPropValInstr.prototype = new BaseInstr();

/**
@class Base class for branching instructions.
@augments BaseInstr
*/
function BranchInstr()
{
    /**
    Potential branch target basic blocks
    @field
    */
    this.targets = [];
}
BranchInstr.prototype = new BaseInstr();

/**
@class Unconditional jump instruction
@augments BranchInstr
*/
function JumpInstr(targetBlock)
{
    /**
    Obtain a string representation
    */
    this.toString = function() { return "jump " + this.targets[0].label; }

    /**
    Target basic block
    @field
    */
    this.targets = [targetBlock];
}
JumpInstr.prototype = new BranchInstr();

/**
@class If conditional test instruction
@augments BranchInstr
*/
function IfInstr(testVal, trueBlock, falseBlock)
{
    /**
    Obtain a string representation
    */
    this.toString = function()
    {
        return  "if " + this.uses[0].valToString() +
                " then " + this.targets[0].label +
                " else " + this.targets[1].label
        ;
    }

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
@class Function return instruction
@augments BranchInstr
*/
function RetInstr(retVal)
{
    /**
    Return value, can be undefined
    @field
    */
    this.uses = [retVal];
}
RetInstr.prototype = new BranchInstr();

/**
@class Exception throw to exception handler. Handler may be left undefined for
interprocedural throw.
@augments BranchInstr
*/
function ThrowInst(excVal, catchBlock)
{
    /**
    Produce a string representation of the throw instruction
    */
    this.toString = function ()
    {
        var output = 'throw ' + excVal.valToString();

        if (this.targets[0] != null)
            output += 'to ' + this.targets[0].label;

        return output;
    }

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
ThrowInst.prototype = new BranchInstr();

/**
@class Exception handler instruction, for function calls. Handler may be left
undefined for interprocedural throw.
@augments BranchInstr
*/
OnExcInst = function (contBlock, catchBlock)
{
    /**
    Produce a string representation of the exception handler instruction
    */
    this.toString = function ()
    {
        var output = 'on_exc throw';

        if (this.targets[0] != null)
            output += ' to ' + this.targets[1].label;

        output += ' else ' + this.targets[0].label;

        return output;
    }    

    /**
    Catch block and continue block for the exception handler
    @field
    */
    this.targets = [contBlock, catchBlock];
}
OnExcInst.prototype = new BranchInstr();

/**
@class Exception value catch
@augments BaseInstr
*/
function CatchInstr()
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'catch';
}
CatchInstr.prototype = new BaseInstr();

/**
@class Call with function object reference
@augments BaseInstr
*/
function CallRefInstr(funcVal, thisVal, paramVals)
{
    /**
    Function value, this value and parameter values
    @field
    */
    this.uses = [funcVal, thisVal].concat(paramVals);
}
CallRefInstr.prototype = new BaseInstr();

/**
@class Constructor call with function object reference
@augments BaseInstr
*/
function ConstructRefInstr(funcVal, paramVals)
{
    /**
    Function value, this value and parameter values
    @field
    */
    this.uses = [funcVal].concat(paramVals);
}
ConstructRefInstr.prototype = new BaseInstr();

