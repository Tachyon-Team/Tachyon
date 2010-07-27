/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// May need to build custom instructions for some things
// - add_i32 with overflow path
// - might actually need platform-specific code
//
// Some operators can have side effects
// - May want a "write" or "side effect" flag

// TODO: should instructions have write, branch, etc. flags? eliminate BranchInstr?

// TODO: by default, some instructions should have VOID type output, eg: if, other branch instructions

//=============================================================================
// IR Core
//
// Implementation of the foundational logic of the IR instructions.
//
//=============================================================================

// IR value type enumeration
IRType =
{
    VOID:       0,  // No output value
    BOXED:      1,  // Boxed value type
    POINTER:    2,  // Unboxed pointer
    INT8:       3,  // Unboxed int8
    INT16:      4,  // Unboxed int16
    INT32:      5,  // Unboxed int32
    INT64:      6,  // Unboxed int64
    FLOAT32:    7,  // Unboxed float32
    FLOAT64:    8   // Unboxed float64
};

/**
Get the name of an IR type
*/
function getIRTypeName(tp)
{
    switch (tp)
    {
        case IRType.VOID:       return 'void';  break;
        case IRType.BOXED:      return 'boxt';  break;
        case IRType.POINTER:    return 'ptrt';  break;
        case IRType.INT8:       return 'i8';    break;
        case IRType.INT16:      return 'i16';   break;
        case IRType.INT32:      return 'i32';   break;
        case IRType.INT64:      return 'i64';   break;
        case IRType.FLOAT32:    return 'f32';   break;
        case IRType.FLOAT64:    return 'f64';   break;
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
        case IRType.VOID:       return '0'; break;
        case IRType.BOXED:      return '8'; break;
        case IRType.POINTER:    return '8'; break;
        case IRType.INT8:       return '1'; break;
        case IRType.INT16:      return '2'; break;
        case IRType.INT32:      return '4'; break;
        case IRType.INT64:      return '8'; break;
        case IRType.FLOAT32:    return '4'; break;
        case IRType.FLOAT64:    return '8'; break;
    }
}

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

    /**
    By default, all IR values have the boxed type
    */
    this.type = IRType.BOXED;
}

/**
@class Represents constant values in the IR
@augments IRValue
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
    else if (this.value instanceof Function)
    {
        if (this.value.hasOwnProperty('name'))
            return this.value.name;
        else
            return 'function';
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
Function to generate a typed instruction constructor using a closure
@param mnemonic mnemonic name of the instruction
@param inTypes array of input value types
@param outType output value type
@param protoObj prototype object for the instruction
*/
function TypedInstrMaker(mnemonic, inTypes, outType, protoObj)
{    
    /**
    Instruction constructor function instance, implemented as a closure
    */
    function InstrConstr(inputs)
    {
        // Put the arguments into an array
        if (inputs instanceof Array)
        {
            var inputArray = inputs;
        }
        else
        {
            var inputArray = [];
            for (var i = 0; i < arguments.length; ++i)
                inputArray.push(arguments[i]);
        }

        // Ensure that the argument count is valid
        assert (
            inputArray.length == inTypes.length,
            'invalid argument count (' + inputArray.length + ') to "' +
            mnemonic + '" instruction constructor'
        );

        // Ensure that each argument is valid
        for (var i = 0; i < inputArray.length; ++i)
        {
            assert (
                inputArray[i] instanceof IRValue,
                'argument ' + i + ' to "' + mnemonic + 
                '" instruction constructor is not valid IR value'
            );       

            assert (
                inputArray[i].type === inTypes[i],
                'argument ' + i + ' to "' + mnemonic + 
                '" instruction constructor does not have valid output type' +
                '(' + getIRTypeName(inputArray[i].type) + ')'
            );
        }

        // Set the mnemonic name for the instruction
        this.mnemonic = mnemonic;

        // Copy the uses of the instruction      
        this.uses = inputArray.slice(0);
    }
    
    // If no prototype object was specified, create an IRInstr instance
    if (!protoObj)
        protoObj = new IRInstr();

    // Set the constructor for the new instruction
    InstrConstr.prototype = protoObj;

    // Store the output type for the instruction
    InstrConstr.prototype.type = outType;

    /**
    Generic instruction shallow copy function
    */
    InstrConstr.prototype.copy = function ()
    {
        // Return a new instruction with the same uses
        return this.baseCopy(
            new InstrConstr(
                this.uses.slice(0)
            )
        );
    };

    // Return the new constructor instance
    return InstrConstr;
}

/**
Function to generate a generic untyped instruction constructor using a closure
@param mnemonic mnemonic name for the instruction
@param numInputs number of input operands
@param protoObj prototype object instance, new IRInstr instance by default
*/
function GenericInstrMaker(mnemonic, numInputs, protoObj)
{
    var inTypes = [];
    for (var i = 0; i < numInputs; ++i)
        inTypes.push(IRType.BOXED);

    return TypedInstrMaker(
        mnemonic, 
        inTypes, 
        IRType.BOXED,
        protoObj
    );
}

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
Get the input value for a given predecessor
*/
PhiInstr.prototype.getIncoming = function (pred)
{
    for (var i = 0; i < this.preds.length; ++i)
    {
        if (this.preds[i] === pred)
            return this.uses[i];
    }

    assert (
        false,
        'cannot get incoming for pred, invalid pred'
    );        
}

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
    this.mnemonic = 'arg';

    // Set the output name as the argument name
    this.outName = argName;
}
ArgValInstr.prototype = new IRInstr();

/**
Get a string representation of the argument instruction
*/
ArgValInstr.prototype.toString = function ()
{
    return this.outName + ' = ' + this.mnemonic;
}

/**
Make a shallow copy of the instruction
*/
ArgValInstr.prototype.copy = function ()
{
    var newInstr = new ArgValInstr(this.outName);
    return this.baseCopy(newInstr);
};

/**
@class Arithmetic add instruction
@augments IRInstr
*/
var AddInstr = GenericInstrMaker(
    'add',
     2
);

/**
@class Arithmetic subtraction instruction
@augments IRInstr
*/
var SubInstr = GenericInstrMaker(
    'sub',
     2
);

/**
@class Arithmetic multiply instruction
@augments IRInstr
*/
var MulInstr = GenericInstrMaker(
    'mul',
     2
);

/**
@class Arithmetic divide instruction
@augments IRInstr
*/
var DivInstr = GenericInstrMaker(
    'div',
     2
);

/**
@class Arithmetic modulo instruction
@augments IRInstr
*/
var ModInstr = GenericInstrMaker(
    'mod',
     2
);

/**
@class Logical negation instruction
@augments IRInstr
*/
var LogNotInstr = GenericInstrMaker(
    'not',
     1
);

/**
@class Bitwise NOT instruction
@augments IRInstr
*/
var BitNotInstr = GenericInstrMaker(
    'not',
     1
);

/**
@class Bitwise AND instruction
@augments IRInstr
*/
var BitAndInstr = GenericInstrMaker(
    'and',
     2
);

/**
@class Bitwise AND instruction
@augments IRInstr
*/
var BitOrInstr = GenericInstrMaker(
    'and',
     2
);

/**
@class Bitwise XOR instruction
@augments IRInstr
*/
var BitXorInstr = GenericInstrMaker(
    'xor',
     2
);

/**
@class Left shift instruction
@augments IRInstr
*/
var LsftInstr = GenericInstrMaker(
    'lsft',
     2
);

/**
@class Right shift instruction
@augments IRInstr
*/
var RsftInstr = GenericInstrMaker(
    'rsft',
     2
);

/**
@class Unsigned right shift instruction
@augments IRInstr
*/
var UrsftInstr = GenericInstrMaker(
    'ursft',
     2
);

/**
@class Less-than comparison instruction
@augments IRInstr
*/
var LtInstr = GenericInstrMaker(
    'lt',
     2
);

/**
@class Less-than-or-equal comparison instruction
@augments IRInstr
*/
var LteInstr = GenericInstrMaker(
    'lte',
     2
);

/**
@class Greater-than comparison instruction
@augments IRInstr
*/
var GtInstr = GenericInstrMaker(
    'gt',
     2
);

/**
@class Greater-than-or-equal comparison instruction
@augments IRInstr
*/
var GteInstr = GenericInstrMaker(
    'gte',
     2
);

/**
@class Equality comparison instruction
@augments IRInstr
*/
var EqInstr = GenericInstrMaker(
    'eq',
     2
);

/**
@class Inequality comparison instruction
@augments IRInstr
*/
var NeqInstr = GenericInstrMaker(
    'neq',
     2
);

/**
@class Strict-equality comparison instruction
@augments IRInstr
*/
var SeqInstr = GenericInstrMaker(
    'seq',
     2
);

/**
@class Strict-inequality comparison instruction
@augments IRInstr
*/
var NseqInstr = GenericInstrMaker(
    'nseq',
     2
);

/**
@class Type query instruction
@augments IRInstr
*/
var TypeOfInstr = GenericInstrMaker(
    'typeof',
     1
);

/**
@class Instance/class query instruction
@augments IRInstr
*/
var InstOfInstr = GenericInstrMaker(
    'instanceof',
     2
);

/**
@class Property set with value for field name
@augments IRInstr
*/
var PutPropValInstr = GenericInstrMaker(
    'put_prop_val',
     3
);

/**
@class Property get with value for field name
@augments IRInstr
*/
var GetPropValInstr = GenericInstrMaker(
    'get_prop_val',
     2
);

/**
@class Property deletion with value for field name
@augments IRInstr
*/
var DelPropValInstr = GenericInstrMaker(
    'del_prop_val',
     2
);

/**
@class Property test with value for field name
@augments IRInstr
*/
var HasPropValInstr = GenericInstrMaker(
    'has_prop_val',
     2
);

/**
@class Instruction to get an array containing the property names of an object
@augments IRInstr
*/
var GetPropNamesInstr = GenericInstrMaker(
    'get_prop_names',
     1
);

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
By default, branch instructions produce no output
*/
BranchInstr.prototype.type = IRType.VOID;

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
Get the target block of the throw instruction
*/
ThrowInstr.prototype.getThrowTarget = function ()
{
    return (this.targets.length > 0)? this.targets[0]:null;
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
var CatchInstr = GenericInstrMaker(
    'catch',
     0
);

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
Call instructions produce boxed values
*/
CallRefInstr.prototype.type = IRType.BOXED;

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
Get the target block of the throw instruction
*/
CallRefInstr.prototype.getThrowTarget = function ()
{
    return (this.targets.length > 1)? this.targets[1]:null;
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
var MakeCellInstr = GenericInstrMaker(
    'make_cell',
     0
);

/**
@class Get the value stored in a mutable cell
@augments IRInstr
*/
var GetCellInstr = GenericInstrMaker(
    'get_cell',
     1
);

/**
@class Set the value stored in a mutable cell
@augments IRInstr
*/
var PutCellInstr = GenericInstrMaker(
    'put_cell',
     2
);

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
var GetClosInstr = GenericInstrMaker(
    'get_clos',
     2
);

/**
@class Set the value stored in a closure variable
@augments IRInstr
*/
var PutClosInstr = GenericInstrMaker(
    'put_clos',
     3
);

/**
@class Instruction to create a new, empty object
@augments IRInstr
*/
var NewObjectInstr = GenericInstrMaker(
    'new_object',
     0
);

/**
@class Instruction to create a new, empty array
@augments IRInstr
*/
var NewArrayInstr = GenericInstrMaker(
    'new_array',
     0
);

//=============================================================================
// Medium-Level IR (MIR)
//
// Introduction of specialized instruction forms.
// Type-specialized instruction variants.
//
// Introduction of a pointer type.
// Access to memory representation.
// Calculation of memory offsets directly.
// Store and load instructions.
// Still machine agnostic, no machine-specific instructions or reg. alloc.
//
//=============================================================================

/**
@class Instruction to load a boxed value
@augments IRInstr
*/
var LoadBoxInstr = TypedInstrMaker(
    'load_box', 
    [IRType.POINTER], 
    IRType.BOXED
);

/**
@class Instruction to load a pointer value
@augments IRInstr
*/
var LoadPtrInstr = TypedInstrMaker(
    'load_ptr', 
    [IRType.POINTER], 
    IRType.POINTER
);

/**
@class Instruction to load an int32 value
@augments IRInstr
*/
var LoadI32Instr = TypedInstrMaker(
    'load_i32', 
    [IRType.POINTER],
    IRType.I32
);

/**
@class Instruction to load a float64 value
@augments IRInstr
*/
var LoadF64Instr = TypedInstrMaker(
    'load_f64', 
    [IRType.POINTER],
    IRType.F64
);

/**
@class Instruction to store a boxed value
@augments IRInstr
*/
var StoreBoxInstr = TypedInstrMaker(
    'store_box', 
    [IRType.POINTER, IRType.BOXED],
    IRType.VOID
);

/**
@class Instruction to store a pointer value
@augments IRInstr
*/
var StorePtrInstr = TypedInstrMaker(
    'store_ptr',
    [IRType.POINTER, IRType.POINTER],
    IRType.VOID
);

/**
@class Instruction to store an int32 value
@augments IRInstr
*/
var StoreI32Instr = TypedInstrMaker(
    'store_i32',
    [IRType.POINTER, IRType.INT32],
    IRType.VOID
);

/**
@class Instruction to store a float64 value
@augments IRInstr
*/
var StoreF64Instr = TypedInstrMaker(
    'store_f64',
    [IRType.POINTER, IRType.FLOAT64],
    IRType.VOID
);

/**
@class Instruction to unbox an int32 value
@augments IRInstr
*/
var UnboxI32Instr = TypedInstrMaker(
    'unbox_i32', 
    [IRType.BOXED], 
    IRType.INT32
);

/**
@class Instruction to unbox a pointer value
@augments IRInstr
*/
var UnboxPtrInstr = TypedInstrMaker(
    'unbox_ptr', 
    [IRType.BOXED], 
    IRType.POINTER
);

/**
@class Instruction to box an int32 value
@augments IRInstr
*/
var BoxI32Instr = TypedInstrMaker(
    'box_i32', 
    [IRType.INT32], 
    IRType.BOXED
);

/**
@class Instruction to box a pointer value
@augments IRInstr
*/
var BoxPtrInstr = TypedInstrMaker(
    'box_ptr', 
    [IRType.POINTER],
    IRType.BOXED
);

/**
@class Instruction to convert int32 values into float64 values
@augments IRInstr
*/
var I32ToF64Instr = TypedInstrMaker(
    'i32_to_f64', 
    [IRType.INT32], 
    IRType.FLOAT64
);

/**
@class Instruction to convert float64 values into int32 values
@augments IRInstr
*/
var F64ToI32Instr = TypedInstrMaker(
    'f64_to_i32', 
    [IRType.FLOAT32], 
    IRType.INT32
);

/**
@class Instruction to add int32 values without overflow handling
@augments IRInstr
*/
var AddI32Instr = TypedInstrMaker(
    'add_i32', 
    [IRType.INT32, IRType.INT32], 
    IRType.INT32
);

/**
@class Instruction to subtract int32 values without overflow handling
@augments IRInstr
*/
var SubI32Instr = TypedInstrMaker(
    'sub_i32', 
    [IRType.INT32, IRType.INT32], 
    IRType.INT32
);

/**
@class Instruction to multiply int32 values without overflow handling
@augments IRInstr
*/
var MulI32Instr = TypedInstrMaker(
    'mul_i32', 
    [IRType.INT32, IRType.INT32], 
    IRType.INT32
);

/**
@class Instruction to divide int32 values
@augments IRInstr
*/
var DivI32Instr = TypedInstrMaker(
    'div_i32', 
    [IRType.INT32, IRType.INT32], 
    IRType.INT32
);

/**
@class Instruction to compute the modulo of int32 values
@augments IRInstr
*/
var ModI32Instr = TypedInstrMaker(
    'mod_i32', 
    [IRType.INT32, IRType.INT32], 
    IRType.INT32
);

/**
@class Instruction to add float64 values
@augments IRInstr
*/
var AddF64Instr = TypedInstrMaker(
    'add_f64', 
    [IRType.FLOAT64, IRType.FLOAT64], 
    IRType.FLOAT64
);

/**
@class Instruction to subtract float64 values
@augments IRInstr
*/
var SubF64Instr = TypedInstrMaker(
    'sub_f64', 
    [IRType.FLOAT64, IRType.FLOAT64], 
    IRType.FLOAT64
);

/**
@class Instruction to multiply float64 values
@augments IRInstr
*/
var MulF64Instr = TypedInstrMaker(
    'mul_f64', 
    [IRType.FLOAT64, IRType.FLOAT64], 
    IRType.FLOAT64
);

/**
@class Instruction to divide float64 values
@augments IRInstr
*/
var DivF64Instr = TypedInstrMaker(
    'div_f64', 
    [IRType.FLOAT64, IRType.FLOAT64], 
    IRType.FLOAT64
);





//
// TODO: bitwise ops on int32
//

/**
@class Instruction to compute the bitwise NOT of int32 values
@augments IRInstr
*/
var NotI32Instr = TypedInstrMaker(
    'not_i32', 
    [IRType.INT32], 
    IRType.INT32
);

/**
@class Instruction to compute the bitwise AND of int32 values
@augments IRInstr
*/
var AndI32Instr = TypedInstrMaker(
    'and_i32',
    [IRType.INT32, IRType.INT32],
    IRType.INT32
);

/**
@class Instruction to compute the bitwise OR of int32 values
@augments IRInstr
*/
var AndI32Instr = TypedInstrMaker(
    'or_i32',
    [IRType.INT32, IRType.INT32],
    IRType.INT32
);









// TODO: OvfArithOp?
// OvfInstrMaker?
// int add, sub, mul
// only needed on int32 for now




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

// TODO: LIR instructions, even closer to machine arch? Not SSA?

// TODO: MoveInstr? LIR only
// No SSA output

