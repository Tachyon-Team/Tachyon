/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/



// TODO: unique "operator" instruction for instructions without control-flow?
// Idea: HIR instrs all map to some function defining their behavior in termsof MIR/LIR
// Perhaps not a great idea, too much into one
// Some operators will eventually have control-flow...
// Only works for HIR operators
//
// Perhaps HIR operators should have a superclass HIROpInstr, or a system to map
// them to functions/handlers

// TODO: Method to generate an instruction constructor from a handler and type
// propagation function using a closure?*****
//
// CAN make distinct constructors from closures
//
// function maker() { return function h() {}; }
// a = maker();
// b = maker();
// a === b
// false
// nA = new a();
// nB = new b();
// nA instanceof a
// true
// nA instanceof b
// false
//
// How to pass operands?
// - constructor can take 2-3 operands, can pass number of actual operands to maker function
//
// Can pass toString func, or set it in prototype directly
// - pass prototype instance to maker function
//
// Can have higher layers of maker/factory functions
//
// Can we generate copy function automatically?
// - can call newly generated function, know how many uses to pass to ctor
//
// May need to build custom instructions for some things
// - add_i32 with overflow path
// - might actually need platform-specific code
//
// Some operators can have side effects
// - May want a "write" or "side effect" flag



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
function ConstValue(value)
{
    /**
    Value of the constant
    @field
    */
    this.value = value;
}
ConstValue.prototype = new IRValue();

/**
Get a string representation of a constant instruction
*/
ConstValue.prototype.toString = function ()
{
    if (typeof this.value == 'string')
    {
       return '"' + escapeJSString(this.value) + '"';
    }

    else if (this.isGlobalConst())
    {
        return 'global';
    }
    else
    {
        return String(this.value);
    }
};

/**
Get a string representation of an instruction's value/name.
Returns the constant's string representation directly.
*/
ConstValue.prototype.getValName = ConstValue.prototype.toString;

/**
Test if a constant is an integer
*/
ConstValue.prototype.isIntConst = function ()
{
    return this.value - Math.floor(this.value) == 0;
}

/**
Global object reference constant
*/
ConstValue.prototype.isGlobalConst = function ()
{
    return this.value === ConstValue.globalConstVal;
}

/**
Map of values to IR constants
*/
ConstValue.constMap = new HashMap();

/**
Global object constant value
*/
ConstValue.globalConstVal = {};

/**
Global object reference constant
*/
ConstValue.globalConst = new ConstValue(ConstValue.globalConstVal);

/**
Get the unique constant instance for a given value
*/
ConstValue.getConst = function (value)
{
    if (!ConstValue.constMap.hasItem(value))
    {
        ConstValue.constMap.addItem(value, new ConstValue(value));
    }

    return ConstValue.constMap.getItem(value);
};

/**
@class Base class for all IR instructions
*/
function IRInstr()
{
    /**
    Test if this instruction's output is read (has uses)
    */
    this.hasDests = function () { return this.dests.length > 0; };

    /**
    Mnemonic name for this instruction    
    @field
    */
    this.mnemonic = '';

    /**
    Name of this instruction's output
    @field
    */
    this.outName = '';

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
Produce a string representation of this instruction
*/
IRInstr.prototype.toString = function ()
{
    var output = "";

    // If this instruction's value is read, print its output name
    if (this.hasDests())
        output += this.getValName() + ' = ';

    output += this.mnemonic + (this.uses.length? ' ':'');

    for (i = 0; i < this.uses.length; ++i)
    {
        var ins = this.uses[i];

        if (!(ins instanceof IRValue))
            output += '***invalid value***';
        else
            output += ins.getValName();

        if (i != this.uses.length - 1)
            output += ", ";
    }

    return output;
};

/**
Get a string representation of an instruction's value/name
*/
IRInstr.prototype.getValName = function ()
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
IRInstr.prototype.baseCopy = function (newInstr)
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
Replace a use
*/
IRInstr.prototype.replUse = function (oldUse, newUse)
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
IRInstr.prototype.addDest = function (dest)
{
    if (this.dests.length == 0)
        this.dests = [dest];
    else
        arraySetAdd(this.dests, dest);
};

/**
Remove a destination
*/
IRInstr.prototype.remDest = function (dest)
{
    arraySetRem(this.dests, dest);
};

/**
Replace a destination
*/
IRInstr.prototype.replDest = function (oldDest, newDest)
{
    for (var i = 0; i < this.dests.length; ++i)
    {
        if (this.dests[i] === oldDest)
            this.dests[i] = newdest;
    }
};

/**
@class SSA phi node instruction
@augments IRInstr
*/
function PhiInstr(values, preds)
{
    // Ensure that each value has one associated predecessor
    assert (
        values.length == preds.length,
        'must have one predecessor for each phi use'
    );

    // Set the mnemonic name for this instruction
    this.mnemonic = "phi";

    /**
    Inputs to the phi node
    @field
    */
    this.uses = values;

    /**
    Immediate predecessor blocks associated with the uses
    @field
    */
    this.preds = preds;
}
PhiInstr.prototype = new IRInstr();

/**
Produce a string representation of the phi instruction
*/
PhiInstr.prototype.toString = function ()
{
    var output = "";

    // If this instruction's value is read, print its output name
    if (this.hasDests())
        output += this.getValName() + ' = ';

    output += this.mnemonic + ' ';

    for (i = 0; i < this.uses.length; ++i)
    {
        var ins = this.uses[i];
        var pred = this.preds[i];

        output += '[' + ins.getValName() + ' ' + pred.getBlockName() + ']';

        if (i != this.uses.length - 1)
            output += ", ";
    }

    return output;
};

/**
Add an incoming value to a phi node
*/
PhiInstr.prototype.addIncoming = function (value, pred)
{
    assert (
        pred !== undefined,
        'must specify predecessor block'
    );

    this.uses.push(value);
    this.preds.push(pred);

    if (value instanceof IRInstr)
        value.addDest(this);
};

/**
Make a shallow copy of the instruction
*/
PhiInstr.prototype.copy = function ()
{
    return this.baseCopy(new PhiInstr(this.uses.slice(0), this.preds.slice(0)));
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
    URSFT:  6
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
        case BitOp.AND:   this.mnemonic = "and";      break;
        case BitOp.OR:    this.mnemonic = "or";       break;
        case BitOp.XOR:   this.mnemonic = "xor";      break;
        case BitOp.NOT:   this.mnemonic = "not";      break;
        case BitOp.LSFT:  this.mnemonic = "lsft";     break;
        case BitOp.RSFT:  this.mnemonic = "rsft";     break;
        case BitOp.RSFTU: this.mnemonic = "ursft";    break;
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
        case CompOp.LT:    this.mnemonic = "lt";   break;
        case CompOp.LTE:   this.mnemonic = "lte";  break;
        case CompOp.GT:    this.mnemonic = "gt";   break;
        case CompOp.GTE:   this.mnemonic = "gte";  break;
        case CompOp.EQ:    this.mnemonic = "eq";   break;
        case CompOp.NE:    this.mnemonic = "ne";   break;
        case CompOp.SEQ:   this.mnemonic = "seq";  break;
        case CompOp.NSEQ:  this.mnemonic = "nseq"; break;
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
function PutPropValInstr(objVal, nameVal, setVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'put_prop_val';

    /**
    Object name, field name and value to set
    @field
    */
    this.uses = [objVal, nameVal, setVal];
}
PutPropValInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
PutPropValInstr.prototype.copy = function ()
{
    var newInstr = new PutPropValInstr(this.uses[0], this.uses[1], this.uses[2]);
    return this.baseCopy(newInstr);
};

/**
@class Property get with value for field name
@augments IRInstr
*/
function GetPropValInstr(objVal, nameVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'get_prop_val';

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
    return this.baseCopy(
        new GetPropValInstr(
            this.uses[0],
            this.uses[1]
        )
    );
};

/**
@class Property deletion with value for field name
@augments IRInstr
*/
function DetPropValInstr(objVal, nameVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'del_prop_val';

    /**
    Object and field name values
    @field
    */
    this.uses = [objVal, nameVal];
}
DetPropValInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
DetPropValInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new DetPropValInstr(
            this.uses[0],
            this.uses[1]
        )
    );
};

/**
@class Property test with value for field name
@augments IRInstr
*/
function HasPropValInstr(objVal, nameVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'has_prop_val';

    /**
    Object and field name values
    @field
    */
    this.uses = [objVal, nameVal];
}
HasPropValInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
HasPropValInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new HasPropValInstr(
            this.uses[0],
            this.uses[1]
        )
    );
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
    // Set the mnemonic name for this instruction
    this.mnemonic = 'throw';

    /**
    Exception value to be thrown
    @field
    */
    this.uses = [excVal];

    /**
    Catch block for this throw, may be empty (no targets) if unspecified
    @field
    */
    this.targets = catchBlock? [catchBlock]:[];
}
ThrowInstr.prototype = new BranchInstr();

/**
Produce a string representation of the throw instruction
*/
ThrowInstr.prototype.toString = function ()
{
    var output = IRInstr.prototype.toString.apply(this);

    if (this.targets[0])
        output += ' to ' + this.targets[0].getBlockName();

    return output;
};

/**
Set the target block of the throw instruction
*/
ThrowInstr.prototype.setThrowTarget = function (catchBlock)
{
    this.targets = catchBlock? [catchBlock]:[];
}

/**
Make a shallow copy of the instruction
*/
ThrowInstr.prototype.copy = function ()
{
    var newInstr = new ThrowInstr(this.uses[0], this.targets[0]);
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
@augments ThrowInstr
*/
function CallRefInstr(funcVal, thisVal, paramVals, contBlock, catchBlock)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'call';

    /**
    Function value, this value and parameter values
    @field
    */
    this.uses = [funcVal, thisVal].concat(paramVals);

    /**
    Catch block and continue block for the exception handler
    @field
    */
    this.targets = catchBlock? [contBlock, catchBlock]:[contBlock];
}
CallRefInstr.prototype = new ThrowInstr();

/**
Produce a string representation of the call instruction
*/
CallRefInstr.prototype.toString = function ()
{
    var output = IRInstr.prototype.toString.apply(this);

    if (this.targets[1])
        output += ' throws ' + this.targets[1].getBlockName();

    output += ' continue ' + this.targets[0].getBlockName();

    return output;
};

/**
Make a shallow copy of the instruction
*/
CallRefInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new CallRefInstr(
            this.uses[0],
            this.uses[1], 
            this.uses.slice[2],
            this.targets[0],
            this.targets[1]
        )
    );
};

/**
Set the target block of the throw instruction
*/
CallRefInstr.prototype.setThrowTarget = function (catchBlock)
{
    this.targets = catchBlock? [this.targets[0], catchBlock]:[this.targets[0]];
}

/**
@class Constructor call with function object reference
@augments CallRefInstr
*/
function ConstructRefInstr(funcVal, paramVals, contBlock, catchBlock)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'construct';

    /**
    Function value and parameter values
    @field
    */
    this.uses = [funcVal].concat(paramVals);

    /**
    Catch block and continue block for the exception handler
    @field
    */
    this.targets = catchBlock? [contBlock, catchBlock]:[contBlock];
}
ConstructRefInstr.prototype = new CallRefInstr();

/**
Make a shallow copy of the instruction
*/
ConstructRefInstr.prototype.copy = function ()
{
    return this.baseCopy(
        ConstructRefInstr(
            this.uses[0],
            this.uses.slice[1],
            this.targets[0],
            this.targets[1]
        )
    );
};

/**
@class Mutable cell creation
@augments IRInstr
*/
function MakeCellInstr()
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'make_cell';
}
MakeCellInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
MakeCellInstr.prototype.copy = function ()
{
    return this.baseCopy(new MakeCellInstr());
};

/**
@class Get the value stored in a mutable cell
@augments IRInstr
*/
function GetCellInstr(cellVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'get_cell';

    /**
    Cell value to be accessed
    @field
    */
    this.uses = [cellVal];
}
GetCellInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
GetCellInstr.prototype.copy = function ()
{
    return this.baseCopy(new GetCellInstr(this.uses[0]));
};

/**
@class Set the value stored in a mutable cell
@augments IRhInstr
*/
function PutCellInstr(cellVal, setVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'put_cell';

    /**
    Cell value to be accessed, value to be set
    @field
    */
    this.uses = [cellVal, setVal];
}
PutCellInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
PutCellInstr.prototype.copy = function ()
{
    return this.baseCopy(new PutCellInstr(this.uses[0], this.uses[1]));
};

/**
@class Closure creation with closure variable arguments
@augments IRInstr
*/
function MakeClosInstr(funcVal, varVals)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'make_clos';

    /**
    Function value and closure variable values
    @field
    */
    this.uses = [funcVal].concat(varVals);
}
MakeClosInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
MakeClosInstr.prototype.copy = function ()
{
    var newInstr = new MakeClosInstr(this.uses[0], this.uses.slice[1]);
    return this.baseCopy(newInstr);
};

/**
@class Get the value stored in a closure variable
@augments IRInstr
*/
function GetClosInstr(closVal, idxVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'get_clos';

    /**
    Closure value to be accessed and index of the variable to get
    @field
    */
    this.uses = [closVal, idxVal];
}
GetClosInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
GetClosInstr.prototype.copy = function ()
{
    return this.baseCopy(new GetClosInstr(this.uses[0], this.uses[1]));
};

/**
@class Set the value stored in a closure variable
@augments IRInstr
*/
function PutClosInstr(closVal, idxVal, setVal)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'put_clos';

    /**
    Closure value to be accessed, index of the variable to set, and value to set
    @field
    */
    this.uses = [closVal, idxVal, setVal];
}
PutClosInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
PutClosInstr.prototype.copy = function ()
{
    return this.baseCopy(new PutClosInstr(this.uses[0], this.uses[1], this.uses[2]));
};

/**
@class Instruction to create a new, empty object
@augments IRInstr
*/
function NewObjectInstr()
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'new_object';
}
NewObjectInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
NewObjectInstr.prototype.copy = function ()
{
    return this.baseCopy(new NewObjectInstr());
};

/**
@class Instruction to create a new, empty array
@augments IRInstr
*/
function NewArrayInstr()
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'new_array';
}
NewArrayInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
NewArrayInstr.prototype.copy = function ()
{
    return this.baseCopy(new NewArrayInstr());
};

//=============================================================================
// Medium-Level IR (MIR)
//
// Introduction of specialized instruction forms.
// Type-specialized instruction variants.
//
//=============================================================================

//
// TODO: complete this section
//
// TODO: type conversion instructions?
// unbox, box, convert?
//

// IR value type enumeration
IRTypes =
{
    BOXED:      0,  // Boxed value type
    POINTER:    1,  // Unboxed pointer
    INT8:       2,  // Unboxed int8
    INT16:      3,  // Unboxed int16
    INT32:      4,  // Unboxed int32
    INT64:      5,  // Unboxed int64
    FLOAT64:    6   // Unboxed float64
};

/**
Get the name of an IR type
*/
function getIRTypeName(tp)
{
    switch (tp)
    {
        case BOXED:     return 'boxt';  break;
        case POINTER:   return 'ptrt';  break;
        case INT8:      return 'i8';    break;
        case INT16:     return 'i16';   break;
        case INT32:     return 'i32';   break;
        case INT64:     return 'i64';   break;
        case FLOAT64:   return 'f64';   break;
    }
}

/**
Get the size of an IR type in bytes
*/
function getIRTypeSize()
{
    // TODO: boxed and pointer type sizes are actually platform-dependent
    // Need code get appropriate size for the platform

    switch (tp)
    {
        case BOXED:     return '8'; break;
        case POINTER:   return '8'; break;
        case INT8:      return '1'; break;
        case INT16:     return '2'; break;
        case INT32:     return '4'; break;
        case INT64:     return '8'; break;
        case FLOAT64:   return '8'; break;
    }
}


// unbox, box, convert?

// TODO: UnboxValInstr

// TODO: BoxValInstr

// TODO: IntCastInstr

// TODO: FPToIntInstr

// TODO: IntToFPInstr


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
@param tp IR type to load
@param ptr memory pointer
*/
function LoadInstr(tp, ptr)
{
    // Set the mnemonic name for the instruction
    this.mnemonic = 'load_' + getIRTypeName(tp);

    /**
    IR type to load
    @field
    */
    this.tp = tp;

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
@param tp IR type to store
@param ptr memory pointer
*/
function StoreInstr(tp, ptr, value)
{
    // Set the mnemonic name for the instruction
    this.mnemonic = 'store_' + getIRTypeName(tp);

    /**
    IR type to store
    @field
    */
    this.tp = tp;

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

