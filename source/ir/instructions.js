/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO:
// Some operators can have side effects
// - May want a "write" or "side effect" flag
// - May want hasSideEffects() method?

// TODO:
// May want MIR iflt, ifgt, ifeq, etc.

//=============================================================================
// IR Core
//
// Implementation of the foundational logic of the IR instructions.
//
//=============================================================================

/**
@class IR type representation object
*/
function IRTypeObj(name, size)
{
    /**
    Name of this type
    @field
    */
    this.name = name;

    /**
    Type size in bytes
    @field
    */
    this.size = size;
}
IRTypeObj.prototype = {};

/**
Obtain a string representation of an IR type
*/
IRTypeObj.prototype.toString = function ()
{
    return this.name;
}

/**
Test if the type is a pointer type
*/
IRTypeObj.prototype.isPtrType = function ()
{
    switch (this)
    {
        case IRType.RAWPTR:
        case IRType.OBJPTR:
        return true;

        default:
        return false;
    }
}

/**
Test if the type is an integer type
*/
IRTypeObj.prototype.isIntType = function ()
{
    switch (this)
    {
        case IRType.UINT8:
        case IRType.UINT16:
        case IRType.UINT32:
        case IRType.UINT64:
        case IRType.INT8:
        case IRType.INT16:
        case IRType.INT32:
        case IRType.INT64:
        return true;

        default:
        return false;
    }
}

/**
Test if the type is a floating-point type
*/
IRTypeObj.prototype.isFPType = function ()
{
    switch (this)
    {
        case IRType.FLOAT32:
        case IRType.FLOAT64:
        return true;

        default:
        return false;
    }
}

/**
Test if the type is an integer or floating-point type
*/
IRTypeObj.prototype.isNumberType = function ()
{
    return this.isIntType() || this.isFPType();
}

// TODO: boxed and pointer type sizes are actually platform-dependent
// Need code get appropriate size for the platform

// Size of a pointer on the current platform
PLATFORM_PTR_SIZE = 8;

// IR value type enumeration
IRType =
{
    // Type given when there is no output value
    VOID:       new IRTypeObj('void', 0),

    // Boxed value type
    BOXED:      new IRTypeObj('box', PLATFORM_PTR_SIZE),

    // Pointer to an object's start address
    OBJPTR:     new IRTypeObj('objptr', PLATFORM_PTR_SIZE),

    // Raw pointer to any memory address
    RAWPTR:     new IRTypeObj('rawptr', PLATFORM_PTR_SIZE),

    // Unboxed unsigned integer types
    UINT8:      new IRTypeObj('u8'  , 1),
    UINT16:     new IRTypeObj('u16' , 2),
    UINT32:     new IRTypeObj('u32' , 4),
    UINT64:     new IRTypeObj('u64' , 8),

    // Unboxed signed integer types
    INT8:       new IRTypeObj('i8'  , 1),
    INT16:      new IRTypeObj('i16' , 2),
    INT32:      new IRTypeObj('i32' , 4),
    INT64:      new IRTypeObj('i64' , 8),

    // Floating-point types
    FLOAT32:    new IRTypeObj('f32' , 4),
    FLOAT64:    new IRTypeObj('f64' , 8)
};

// Int type of width corresponding a pointer on this platform
IRType.PLATFORM_INT =
    PLATFORM_PTR_SIZE == 4?
    IRType.INT32:
    IRType.INT64
;

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
function ConstValue(value, type)
{
    /**
    Value of the constant
    @field
    */
    this.value = value;

    /**
    Type of the constant
    @field
    */
    this.type = type;
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
ConstValue.prototype.isInt = function ()
{
    return this.value - Math.floor(this.value) == 0;
}

/**
Test if a constant is the undefined constant
*/
ConstValue.prototype.isUndef = function ()
{
    return this.value === undefined;
}

/**
Map of values to maps of types to IR constants
*/
ConstValue.constMap = new HashMap();

/**
Get the unique constant instance for a given value
*/
ConstValue.getConst = function (value, type)
{
    // The default type is boxed
    if (type === undefined)
        type = IRType.BOXED;

    // If there is no type map for this value
    if (!ConstValue.constMap.hasItem(value))
    {
        // Create a new hash map to map types to constants
        var typeMap = new HashMap();
        ConstValue.constMap.addItem(value, typeMap);
    }
    else
    {
        var typeMap = ConstValue.constMap.getItem(value);
    }

    // If there is no constant for this type
    if (!typeMap.hasItem(type))
    {
        // Create a new constant with the specified type
        var constant = new ConstValue(value, type);
        typeMap.addItem(type, constant);
    }
    else
    {
        var constant = typeMap.getItem(type);
    }

    // Return the constant
    return constant;
};

/**
Map of names to IR constants
*/
ConstValue.nameMap = new HashMap();

/**
Register a named IR constant
*/
ConstValue.regNamedConst = function (name, val, type)
{
    assert (
        typeof name == 'string',
        'constant names must be strings'
    );

    assert (
        !ConstValue.nameMap.hasItem(name),
        'named constant already registered: ' + name
    );

    // Ensure that the name is valid
    // TODO: use JS regexes once supported
    for (var i = 0; i < name.length; ++i)
    {
        var ch = name.charCodeAt(i);
        assert (
            ch == 95 ||                 // _
            (ch >= 65 && ch <= 90) ||   // A-Z
            (ch >= 48 && ch <= 57),     // 0-9
            'invalid constant name'
        );
    }

    var constant = ConstValue.getConst(val, type);

    ConstValue.nameMap.addItem(name, constant);
}

/**
Lookup a named IR constant
*/
ConstValue.getNamedConst = function (name)
{
    if (!ConstValue.nameMap.hasItem(name))
        return undefined;

    return ConstValue.nameMap.getItem(name);        
}

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
    Potential branch target basic blocks
    @field
    */
    this.targets = [];

    /**
    Parent basic block
    @field
    */
    this.parentBlock = null;
}
IRInstr.prototype = new IRValue();

/**
Default output string formatting function
*/
IRInstr.defOutFormat = function (val)
{
    return val.type.name + ' ' + val.getValName();
}

/**
Default input string formatting function
*/
IRInstr.defInFormat = function (val)
{
    return val.getValName();
}

/**
Produce a string representation of this instruction
*/
IRInstr.prototype.toString = function (outFormatFn, inFormatFn)
{
    // If no formatting functions were specified, use the default ones
    if (!outFormatFn)
        outFormatFn = IRInstr.defOutFormat;
    if (!inFormatFn)
        inFormatFn = IRInstr.defInFormat;

    // Create a string for the output
    var output = "";

    // If this instruction has a non-void output print its output name
    if (this.type != IRType.VOID)
        output += outFormatFn(this) + ' = ';

    output += this.mnemonic;

    // For each use
    for (i = 0; i < this.uses.length; ++i)
    {
        var ins = this.uses[i];

        output += ' ';

        if (!(ins instanceof IRValue))
            output += '***invalid value***';
        else
            output += inFormatFn(ins);

        if (i != this.uses.length - 1)
            output += ",";
    }

    // For each branch target
    for (var i = 0; i < this.targets.length; ++i)
    {
        output += 
            (this.targetNames[i]? (' ' + this.targetNames[i]):'') + 
            ' ' + this.targets[i].getBlockName()
        ;
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
Returns a use iterator. Iterates through uses from left to right.
*/
IRInstr.prototype.getUseItr = function ()
{
    return new ArrayIterator(this.uses);
};

/**
Test if this instruction is a branch
*/
IRInstr.prototype.isBranch = function ()
{
    return (this.targets.length > 0);
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

    // Ensure that all values have the same type
    for (var i = 1; i < values.length; ++i)
    {
        assert (
            values[i].type === values[i-1].type,
            'all phi input values must have the same type'
        )
    }

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

    /**
    Phi node type, equal to the input values type
    @field
    */
    this.type = this.uses.length? this.uses[0].type:IRType.VOID;
}
PhiInstr.prototype = new IRInstr();

/**
Produce a string representation of the phi instruction
*/
PhiInstr.prototype.toString = function (outFormatFn, inFormatFn)
{
    // If no formatting functions were specified, use the default ones
    if (!outFormatFn)
        outFormatFn = IRInstr.defOutFormat;
    if (!inFormatFn)
        inFormatFn = IRInstr.defInFormat;

    var output = "";

    // If this instruction's type is not void, print its output name
    if (this.type != IRType.VOID)
        output += outFormatFn(this) + ' = ';

    output += this.mnemonic + ' ';

    for (i = 0; i < this.uses.length; ++i)
    {
        var ins = this.uses[i];
        var pred = this.preds[i];

        output += '[' + inFormatFn(ins) + ' ' + pred.getBlockName() + ']';

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

    // If there are already inputs
    if (this.uses.length)
    {
        assert (
            value.type === this.uses[0].type,
            'all phi inputs must have the same type'       
        );
    }
    else
    {
        // Set the phi node type
        this.type = value.type;
    }

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

/**
@class Function argument value instruction
@augments IRInstr
*/
function ArgValInstr(argName, argIndex)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'arg';

    // Set the output name as the argument name
    this.outName = argName;

    // Set the argument index
    this.argIndex = argIndex;
}
ArgValInstr.prototype = new IRInstr();

/**
Get a string representation of the argument instruction
*/
ArgValInstr.prototype.toString = function (outFormatFn, inFormatFn)
{
    // Get the default toString output for the instruction
    var output = IRInstr.prototype.toString.apply(this, outFormatFn, inFormatFn);

    // Add the argument index to the output
    output += ' ' + this.argIndex;

    return output;
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
Function to generate instruction constructors using closures
@param mnemonic mnemonic name of the instruction
@param initFunc initialization and validation function
@param branchNames list of branch target names
@param protoObj prototype object for the instruction
@param nameFunc name setting function
*/
function instrMaker(
    mnemonic,
    initFunc,
    branchNames,
    protoObj
)
{
    /**
    Parse instruction constructor arguments
    */
    function parseArgs(argArray, typeParams, inputVals, branchTargets)
    {
        var curIndex = 0;

        // Extract type parameters, if any
        for (; curIndex < argArray.length && argArray[curIndex] instanceof IRTypeObj; ++curIndex)
            typeParams.push(argArray[curIndex]);

        // Extract input values, if any
        for (; curIndex < argArray.length && argArray[curIndex] instanceof IRValue; ++curIndex)
            inputVals.push(argArray[curIndex]);

        // Extract branch targets, if any
        for (; curIndex < argArray.length && argArray[curIndex] instanceof BasicBlock; ++curIndex)
            branchTargets.push(argArray[curIndex]);

        assert (
            curIndex == argArray.length,
            'invalid arguments passed to ' + mnemonic + ' constructor'
        );
    }

    /**
    Set the instruction name based on its type
    */
    function setName(typeParams, inputVals)
    {
        // If type parameters are provided
        if (typeParams.length > 0)
        {
            // Add the type parameters to the instruction name
            this.mnemonic = mnemonic;
            for (var j = 0; j < typeParams.length; ++j)
                this.mnemonic += '_' + typeParams[j];
        }

        // Otherwise, no type parameters are provided
        else
        {
            // Verify if all types in the specification are the same
            var firstType = inputVals.length? inputVals[0].type:null;
            var allSame = true;
            for (var j = 0; j < inputVals.length; ++j)
            {
                if (inputVals[j].type !== firstType)            
                    allSame = false;
            }

            // Set the initial mnemonic name
            this.mnemonic = mnemonic;

            // If all input types are the same
            if (allSame)
            {
                // If there is a first type and it is not boxed
                if (firstType !== null && firstType !== IRType.BOXED)
                {
                    // Append the input type to the name
                    this.mnemonic += '_' + firstType.name;
                }
            }
            else
            {
                // Append all input types to the name
                for (var j = 0; j < inputVals.length; ++j)
                    this.mnemonic += '_' + inputVals[j].type;
            }
        }
    }

    /**
    Instruction constructor function instance, implemented as a closure
    */
    function InstrConstr(inputs)
    {
        // Parse the input arguments
        var typeParams = [];
        var inputVals = [];
        var branchTargets = [];
        if (inputs instanceof Array)
            parseArgs(inputs, typeParams, inputVals, branchTargets);
        else
            parseArgs(arguments, typeParams, inputVals, branchTargets);

        // Call the initialization and validation function
        try
        {
            this.initFunc(typeParams, inputVals, branchTargets);
        }

        // If an error occurs, rethrow it, including the instruction name
        catch (error)
        {
            var errorStr = 
                'Invalid arguments to "' + mnemonic + '" instruction: ' +
                error.toString()
            ;

            throw errorStr;
        }

        // If the mnemonic name is not set, call the name setting function
        if (this.mnemonic === '')
            setName.apply(this, [typeParams, inputVals]);

        // Store the type parameters of the instruction
        this.typeParams = typeParams;

        // Store the uses of the instruction
        this.uses = inputVals;

        // If this is a branch instruction
        if (branchNames)
        {
            // Store the branch targets
            this.targets = branchTargets;
        }
    }

    // If no prototype object was specified, create one
    if (!protoObj)
        protoObj = new IRInstr();

    // Set the prototype for the new instruction
    InstrConstr.prototype = protoObj;

    // Store the branch target names
    InstrConstr.prototype.targetNames = branchNames;

    // Store the initialization function
    if (initFunc)
        InstrConstr.prototype.initFunc = initFunc;


    /**
    Generic instruction shallow copy function
    */
    InstrConstr.prototype.copy = function ()
    {
        // Setup the copy arguments
        var copyArgs = [];
        copyArgs = copyArgs.concat(this.typeParams.slice(0));
        copyArgs = copyArgs.concat(this.uses.slice(0));
        if (this.targets) copyArgs = copyArgs.concat(this.targets.slice(0));

        // Return a new instruction with the same type parameters, uses and targets
        return this.baseCopy(new InstrConstr(copyArgs));
    };

    // Return the new constructor instance
    return InstrConstr;
}

/**
Function to validate the length of an input array
*/
instrMaker.validCount = function (name, array, minExpected, maxExpected)
{
    if (maxExpected === undefined)
        maxExpected = minExpected;

    var expectedStr;
    if (minExpected == maxExpected)
        expectedStr = String(minExpected);
    else if (maxExpected != Infinity)
        expectedStr = 'between ' + minExpected + ' and ' + maxExpected;
    else
        expectedStr = minExpected + ' or more'

    assert (
        array.length >= minExpected && array.length <= maxExpected,
        'got ' + array.length + ' ' +
        pluralize(name, array.length) + 
        ', expected ' + expectedStr
    );
}

/**
Function to validate the type paramers count of an instruction
*/
instrMaker.validNumParams = function (typeParams, minExpected, maxExpected)
{
    instrMaker.validCount('type parameter', typeParams, minExpected, maxExpected);
}

/**
Function to validate the argument count of an instruction
*/
instrMaker.validNumInputs = function (inputVals, minExpected, maxExpected)
{
    instrMaker.validCount('input value', inputVals, minExpected, maxExpected);
}

/**
Function to validate the branch targets of an instruction
*/
instrMaker.validNumBranches = function (branchTargets, minExpected, maxExpected)
{
    instrMaker.validCount('branch target', branchTargets, minExpected, maxExpected);
}

/**
Function to ensure that all values in an array are of boxed type
*/
instrMaker.allValsBoxed = function (inputVals)
{
    inputVals.forEach(
        function (val)
        {
            assert (
                val.type === IRType.BOXED,
                'all input values must be boxed'
            );
        }
    );
}

/**
Function to ensure that all values in an array are of boxed type
*/
instrMaker.validType = function (value, expectedType)
{
    assert (
        value.type === expectedType,
        'got ' + value.type + ' value, expected ' + expectedType
    );
}

/**
Function to generate generic untyped instruction constructors using closures
@param mnemonic mnemonic name for the instruction
@param numInputs number of input operands
@param protoObj prototype object instance, new IRInstr instance by default
*/
function untypedInstrMaker(mnemonic, numInputs, branchNames, voidOutput, protoObj)
{
    function initFunc(typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, numInputs);
        instrMaker.allValsBoxed(inputVals);

        assert (
            (branchTargets.length == 0 && !branchNames) ||
            (branchTargets.length == branchNames.length),
            'invalid number of branch targets specified'
        );

        this.type = voidOutput? IRType.VOID:IRType.BOXED;
    }

    return instrMaker(
        mnemonic,
        initFunc,
        branchNames,
        protoObj,
        undefined
    );
}

//=============================================================================
//
// High-Level instructions, these operate only on boxed values
//
//=============================================================================

/**
@class Logical negation instruction
@augments IRInstr
*/
var LogNotInstr = untypedInstrMaker(
    'not',
     1
);

/**
@class Type query instruction
@augments IRInstr
*/
var TypeOfInstr = untypedInstrMaker(
    'typeof',
     1
);

/**
@class Instance/class query instruction
@augments IRInstr
*/
var InstOfInstr = untypedInstrMaker(
    'instanceof',
     2
);

/**
@class Exception value catch
@augments IRInstr
*/
var CatchInstr = untypedInstrMaker(
    'catch',
     0
);

/**
@class Property deletion with value for field name
@augments IRInstr
*/
var DelPropValInstr = untypedInstrMaker(
    'del_prop_val',
     2
);

/**
@class Property test with value for field name
@augments IRInstr
*/
var HasPropValInstr = untypedInstrMaker(
    'has_prop_val',
     2
);

/**
@class Instruction to get an array containing the property names of an object
@augments IRInstr
*/
var GetPropNamesInstr = untypedInstrMaker(
    'get_prop_names',
     1
);

/**
@class Argument object creation
@augments IRInstr
*/
var MakeArgObjInstr = untypedInstrMaker(
    'make_arg_obj',
     1
);

/**
@class Mutable cell creation
@augments IRInstr
*/
var MakeCellInstr = untypedInstrMaker(
    'make_cell',
     0
);

/**
@class Get the value stored in a mutable cell
@augments IRInstr
*/
var GetCellInstr = untypedInstrMaker(
    'get_cell',
     1
);

/**
@class Set the value stored in a mutable cell
@augments IRInstr
*/
var PutCellInstr = untypedInstrMaker(
    'put_cell',
     2,
    undefined,
    true
);

/**
@class Closure creation with closure variable arguments
@augments IRInstr
*/
var MakeClosInstr = instrMaker(
    'make_clos',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.allValsBoxed(inputVals);
        instrMaker.validNumInputs(inputVals, 2, Infinity);
    }
);

/**
@class Get the global value stored in a closure
@augments IRInstr
*/
var GetGlobalInstr = untypedInstrMaker(
    'get_global',
     1
);

/**
@class Get the value stored in a closure variable
@augments IRInstr
*/
var GetClosInstr = untypedInstrMaker(
    'get_clos',
     2
);

/**
@class Set the value stored in a closure variable
@augments IRInstr
*/
var PutClosInstr = untypedInstrMaker(
   'put_clos',
    3,
    undefined,
    true
);

/**
@class Instruction to create a new, empty object
@augments IRInstr
*/
var NewObjectInstr = untypedInstrMaker(
    'new_object',
     1
);

/**
@class Instruction to create a new, empty array
@augments IRInstr
*/
var NewArrayInstr = untypedInstrMaker(
    'new_array',
     0
);

//=============================================================================
//
// Arithmetic operations without overflow handling
//
//=============================================================================

/**
@class Base class for arithmetic instructions
@augments IRInstr
*/
ArithInstr = function ()
{
}
ArithInstr.prototype = new IRInstr();

/**
Default initialization function for arithmetic instructions
*/
ArithInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2);

    assert (
        (inputVals[0].type === IRType.BOXED ||
         inputVals[0].type.isNumberType())
        &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types'
    );
    
    this.type = inputVals[0].type;
}

/**
@class Addition instruction
@augments ArithInstr
*/
var AddInstr = instrMaker(
    'add',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 2);

        assert (
            (inputVals[0].type === IRType.RAWPTR &&
             inputVals[1].type === IRType.PLATFORM_INT)
            ||
            (
                (inputVals[0].type === IRType.BOXED ||
                 inputVals[0].type.isNumberType())
                &&
                inputVals[1].type === inputVals[0].type
            ),
            'invalid input types'
        );
        
        this.type = inputVals[0].type;
    },
    undefined,
    new ArithInstr()
);

/**
@class Subtraction instruction
@augments ArithInstr
*/
var SubInstr = instrMaker(
    'sub',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 2);

        assert (
            (inputVals[0].type === IRType.RAWPTR &&
             inputVals[1].type === IRType.PLATFORM_INT)
            ||
            (inputVals[0].type === IRType.RAWPTR &&
             inputVals[1].type === IRType.RAWPTR)
            ||
            (
                (inputVals[0].type === IRType.BOXED ||
                 inputVals[0].type.isNumberType())
                &&
                inputVals[1].type === inputVals[0].type
            ),
            'invalid input types'
        );
        
        if (
            inputVals[0].type === IRType.RAWPTR && 
            inputVals[1].type === IRType.RAWPTR
        )
            this.type = IRType.PLATFORM_INT
        else
            this.type = inputVals[0].type;
    },
    undefined,
    new ArithInstr()
);

/**
@class Multiplication instruction
@augments ArithInstr
*/
var MulInstr = instrMaker(
    'mul',
    undefined,
    undefined,
    new ArithInstr()
);

/**
@class Division instruction
@augments ArithInstr
*/
var DivInstr = instrMaker(
    'div',
    undefined,
    undefined,
    new ArithInstr()
);

/**
@class Modulo instruction
@augments ArithInstr
*/
var ModInstr = instrMaker(
    'mod',
    undefined,
    undefined,
    new ArithInstr()
);

//=============================================================================
//
// Arithmetic operations with overflow handling
//
//=============================================================================

/**
@class Base class for arithmetic instructions with overflow handling
@augments IRInstr
*/
ArithOvfInstr = function ()
{
}
ArithOvfInstr.prototype = new IRInstr();

/**
Default initialization function for arithmetic instructions w/ overflow
*/
ArithOvfInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2);
    assert (
        inputVals[0].type === IRType.PLATFORM_INT &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types'
    );
    
    this.type = inputVals[0].type;
}

/**
@class Instruction to add integer values with overflow handling
@augments ArithOvfInstr
*/
var AddOvfInstr = instrMaker(
    'add_ovf',
    undefined,
    ['normal', 'overflow'],
    new ArithOvfInstr()
);

/**
@class Instruction to subtract integer values with overflow handling
@augments ArithOvfInstr
*/
var SubOvfInstr = instrMaker(
    'sub_ovf',
    undefined,
    ['normal', 'overflow'],
    new ArithOvfInstr()
);

/**
@class Instruction to multiply integer values with overflow handling
@augments ArithOvfInstr
*/
var MulOvfInstr = instrMaker(
    'mul_ovf',
    undefined,
    ['normal', 'overflow'],
    new ArithOvfInstr()
);

//=============================================================================
//
// Bitwise operations
//
//=============================================================================

/**
@class Base class for bitwise operation instructions
@augments IRInstr
*/
BitOpInstr = function ()
{
}
BitOpInstr.prototype = new IRInstr();

/**
Default initialization function for bitwise operation instructions
*/
BitOpInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2);

    assert (
        (inputVals[0].type === IRType.BOXED ||
         inputVals[0].type.isIntType())
        &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types'
    );
    
    this.type = inputVals[0].type;
}

/**
@class Bitwise NOT instruction
@augments BitOpInstr
*/
var NotInstr = instrMaker(
    'not',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1);

        assert (
            (inputVals[0].type === IRType.BOXED ||
            inputVals[0].type.isIntType()),
            'invalid input type'
        );
        
        this.type = inputVals[0].type;
    },
    undefined,
    new BitOpInstr()
);

/**
@class Bitwise AND instruction
@augments BitOpInstr
*/
var AndInstr = instrMaker(
    'and',
    undefined,
    undefined,
    new BitOpInstr()
);

/**
@class Bitwise OR instruction
@augments BitOpInstr
*/
var OrInstr = instrMaker(
    'or',
    undefined,
    undefined,
    new BitOpInstr()
);

/**
@class Bitwise XOR instruction
@augments BitOpInstr
*/
var XorInstr = instrMaker(
    'xor',
    undefined,
    undefined,
    new BitOpInstr()
);

/**
@class Left shift instruction
@augments BitOpInstr
*/
var LsftInstr = instrMaker(
    'lsft',
    undefined,
    undefined,
    new BitOpInstr()
);

/**
@class Right shift instruction
@augments BitOpInstr
*/
var RsftInstr = instrMaker(
    'rsft',
    undefined,
    undefined,
    new BitOpInstr()
);

/**
@class Unsigned right shift instruction
@augments BitOpInstr
*/
var UrsftInstr = instrMaker(
    'ursft',
    undefined,
    undefined,
    new BitOpInstr()
);

//=============================================================================
//
// Comparison instructions
//
//=============================================================================

/**
@class Base class for comparison instructions
@augments IRInstr
*/
CompInstr = function ()
{
}
CompInstr.prototype = new IRInstr();

/**
Default initialization function for comparison instructions
*/
CompInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2);

    assert (
        (inputVals[0].type === IRType.BOXED ||
         inputVals[0].type.isNumberType())
        &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types'
    );
    
    if (inputVals[0].type === IRType.BOXED)
        this.type = IRType.BOXED;
    else
        this.type = IRType.INT8;
}

/**
@class Less-than comparison instruction
@augments CompInstr
*/
var LtInstr = instrMaker(
    'lt',
    undefined,
    undefined,
    new CompInstr()
);

/**
@class Less-than-or-equal comparison instruction
@augments CompInstr
*/
var LteInstr = instrMaker(
    'lte',
    undefined,
    undefined,
    new CompInstr()
);

/**
@class Greater-than comparison instruction
@augments CompInstr
*/
var GtInstr = instrMaker(
    'gt',
    undefined,
    undefined,
    new CompInstr()
);

/**
@class Greater-than-or-equal comparison instruction
@augments CompInstr
*/
var GteInstr = instrMaker(
    'gte',
    undefined,
    undefined,
    new CompInstr()
);

/**
@class Equality comparison instruction
@augments CompInstr
*/
var EqInstr = instrMaker(
    'eq',
    undefined,
    undefined,
    new CompInstr()
);

/**
@class Inequality comparison instruction
@augments CompInstr
*/
var NeqInstr = instrMaker(
    'neq',
    undefined,
    undefined,
    new CompInstr()
);

/**
@class Strict-equality comparison instruction
@augments CompInstr
*/
var SeqInstr = untypedInstrMaker(
    'seq',
     2,
    undefined,
    false,
    new CompInstr()
);

/**
@class Strict-inequality comparison instruction
@augments CompInstr
*/
var NseqInstr = untypedInstrMaker(
    'nseq',
     2,
    undefined,
    false,
    new CompInstr()
);

//=============================================================================
//
// Branching instructions
//
//=============================================================================

/**
@class Unconditional jump instruction
@augments IRInstr
*/
var JumpInstr = untypedInstrMaker(
    'jump',
     0,
    [undefined],
    true
);

/**
@class Function return instruction
@augments IRInstr
*/
var RetInstr = untypedInstrMaker(
    'ret',
     1,
    undefined,
    true
);

/**
Ret instructions are always branch instructions
*/
RetInstr.prototype.isBranch = function ()
{
    return true;
}

/**
@class If branching instruction
@augments IRInstr
*/
var IfInstr = instrMaker(
    'if',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1);
        assert (
            inputVals[0].type === IRType.BOXED ||
            inputVals[0].type === IRType.INT8,
            'input must be boxed or int8'
        );
        instrMaker.validNumBranches(branchTargets, 2);
        
        this.type = IRType.VOID;
    },
    ['then', 'else']
);

//=============================================================================
//
// Exception-producing and call instructions
//
//=============================================================================

/**
@class Base class for exception-producing instructions
@augments IRInstr
*/
ExceptInstr = function ()
{
}
ExceptInstr.prototype = new IRInstr();

/**
Set the target block of the exception-producing instruction
*/
ExceptInstr.prototype.setThrowTarget = function (catchBlock)
{
    this.targets[0] = catchBlock;
}

/**
Get the target block of the exception-producing instruction
*/
ExceptInstr.prototype.getThrowTarget = function ()
{
    return this.targets[0]? this.targets[0]:null;
}

/**
@class Exception throw to exception handler. Handler may be left undefined for
interprocedural throw.
@augments ExceptInstr
*/
var ThrowInstr = instrMaker(
    'throw',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1);
        instrMaker.validType(inputVals[0], IRType.BOXED);
        instrMaker.validNumBranches(branchTargets, 0, 1);
        
        this.type = IRType.VOID;
    },
    ['to'],
    new ExceptInstr()
);

/**
Throw instructions are always branch instructions
*/
ThrowInstr.prototype.isBranch = function ()
{
    return true; 
}

/**
@class Base class for call instructions
@augments ExceptInstr
*/
CallInstr = function ()
{
}
CallInstr.prototype = new ExceptInstr();

/**
Set the continue block of the call instruction
*/
CallInstr.prototype.setContTarget = function (contBlock)
{
    this.targets[0] = contBlock;

    while (!this.targets[this.targets.length-1])
        this.targets.pop();
}

/**
Set the throw target block of the call instruction
*/
CallInstr.prototype.setThrowTarget = function (catchBlock)
{
    this.targets = catchBlock? [this.targets[0], catchBlock]:[this.targets[0]];

    while (!this.targets[this.targets.length-1])
        this.targets.pop();
}

/**
Get the throw target block of the call instruction
*/
CallInstr.prototype.getThrowTarget = function ()
{
    return (this.targets.length > 1)? this.targets[1]:null;
}

/**
@class Function call instruction
@augments CallInstr
*/
var CallFuncInstr = instrMaker(
    undefined,
    function (typeParams, inputVals, branchTargets)
    {
        this.mnemonic = 'call';

        instrMaker.validNumInputs(inputVals, 2, Infinity);
        instrMaker.validType(inputVals[0], IRType.BOXED);
        instrMaker.validType(inputVals[1], IRType.BOXED);
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.BOXED;
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
@class Constructor call with function object reference
@augments CallInstr
*/
var ConstructInstr = instrMaker(
    undefined,
    function (typeParams, inputVals, branchTargets)
    {
        this.mnemonic = 'construct';

        instrMaker.validNumInputs(inputVals, 2, Infinity);
        instrMaker.validType(inputVals[0], IRType.BOXED);
        instrMaker.validType(inputVals[1], IRType.BOXED);
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.BOXED;
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
@class Direct handler function call
@augments CallInstr
*/
var CallHandlerInstr = instrMaker(
    undefined,
    function (typeParams, inputVals, branchTargets)
    {
        this.mnemonic = 'call_hdlr';

        instrMaker.validNumInputs(inputVals, 1, Infinity);
        assert (
            inputVals[0] instanceof IRFunction,
            'expected handler function'
        );
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.BOXED;
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
@class Property set with value for field name
@augments CallInstr
*/
var PutPropValInstr = instrMaker(
    'put_prop_val',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 3);
        instrMaker.validType(inputVals[0], IRType.BOXED);
        instrMaker.validType(inputVals[1], IRType.BOXED);
        instrMaker.validType(inputVals[2], IRType.BOXED);
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.VOID;
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
@class Property get with value for field name
@augments CallInstr
*/
var GetPropValInstr = instrMaker(
    'get_prop_val',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 2);
        instrMaker.validType(inputVals[0], IRType.BOXED);
        instrMaker.validType(inputVals[1], IRType.BOXED);
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.BOXED;
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
@class Throw an error with the specified constructor and message
@augments CallInstr
*/
var ThrowErrorInstr = instrMaker(
    'throw_error',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 2);
        instrMaker.validType(inputVals[0], IRType.BOXED);
        instrMaker.validType(inputVals[1], IRType.BOXED);
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.BOXED;
    },
    ['continue', 'throw'],
    new CallInstr()
);

//=============================================================================
//
// Type conversion instructions
//
//=============================================================================

/**
@class Instruction to unbox a value
@augments IRInstr
*/
var UnboxInstr = instrMaker(
    'unbox',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(typeParams, 1);
        instrMaker.validNumInputs(inputVals, 1);
        assert (
            typeParams[0] === IRType.OBJPTR ||
            typeParams[0] === IRType.INT32  ||
            typeParams[0] === IRType.PLATFORM_INT,
            'type parameter should be object pointer or platform int'
        );
        instrMaker.validType(inputVals[0], IRType.BOXED);
        
        this.type = typeParams[0];
    }
);

/**
@class Instruction to box a value
@augments IRInstr
*/
var BoxInstr = instrMaker(
    'box',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(typeParams, 1);
        instrMaker.validNumInputs(inputVals, 1);
        assert (
            typeParams[0] === IRType.OBJPTR || typeParams[0] === IRType.PLATFORM_INT,
            'type parameter should be object pointer or platform int'
        );
        instrMaker.validType(inputVals[0], typeParams[0]);

        this.type = IRType.BOXED;
    }
);

/**
@class Instruction to perform a raw data extraction on a boxed value
@augments IRInstr
*/
var RawUnboxInstr = instrMaker(
    'raw_unbox',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(typeParams, 1);
        instrMaker.validNumInputs(inputVals, 1);
        assert (
            typeParams[0] === IRType.RAWPTR || typeParams[0] === IRType.PLATFORM_INT,
            'type parameter should be raw pointer or platform int'
        );
        instrMaker.validType(inputVals[0], IRType.BOXED);
        
        this.type = typeParams[0];
    }
);

/**
@class Instruction to evaluate the boolean value of a boxed value
@augments IRInstr
*/
var ToBoolInstr = instrMaker(
    'tobool',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1);
        instrMaker.validType(inputVals[0], IRType.BOXED);
        
        this.type = IRType.INT8;
    }
);

/**
@class Instruction to convert between different integer types
@augments IRInstr
*/
var ICastInstr = instrMaker(
    'icast',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(inputVals, 2);
        instrMaker.validNumInputs(inputVals, 1);
        assert (
            (typeParams[0].isIntType() || typeParams[0] === IRType.RAWPTR) &&
            (typeParams[1].isIntType() || typeParams[1] === IRType.RAWPTR),
            'type parameters must be integer or raw pointer'
        );
        instrMaker.validType(inputVals[0], typeParams[0]);
        
        this.type = typeParams[1];
    }
);

/**
@class Instruction to convert integer values to floating-point
@augments IRInstr
*/
var IToFPInstr = instrMaker(
    'itof',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(inputVals, 2);
        instrMaker.validNumInputs(inputVals, 1);
        assert (
            typeParams[0] === IRType.PLATFORM_INT &&
            typeParams[1] === IRType.FLOAT64,
            'invalid type parameters'
        );
        instrMaker.validType(inputVals[0], typeParams[0]);
        
        this.type = typeParams[1];
    }
);

/**
@class Instruction to convert floating-point values to integer
@augments IRInstr
*/
var FPToIInstr = instrMaker(
    'ftoi',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(inputVals, 2);
        instrMaker.validNumInputs(inputVals, 1);
        assert (
            typeParams[0] === IRType.FLOAT64 &&
            typeParams[1] === IRType.PLATFORM_INT,
            'invalid type parameters'
        );
        instrMaker.validType(inputVals[0], typeParams[0]);
        
        this.type = typeParams[1];
    }
);

//=============================================================================
//
// Memory access instructions
//
//=============================================================================

/**
@class Instruction to load a value from memory
@augments IRInstr
*/
var LoadInstr = instrMaker(
    'load',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(typeParams, 1);
        instrMaker.validNumInputs(inputVals, 2);
        assert (
            inputVals[0].type.isPtrType(),
            'the first input must be a pointer'
        );
        instrMaker.validType(inputVals[1], IRType.INT32);
        
        this.type = typeParams[0];
    }
);

/**
@class Instruction to store a value to memory
@augments IRInstr
*/
var StoreInstr = instrMaker(
    'load',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(typeParams, 1);
        instrMaker.validNumInputs(inputVals, 3);
        assert (
            inputVals[0].type.isPtrType(),
            'the first input must be a pointer'
        );
        instrMaker.validType(inputVals[1], IRType.INT32);
        instrMaker.validType(inputVals[2], typeParams[0]);
        
        this.type = IRType.VOID;
    }
);

//=============================================================================
//
// Low-Level IR instructions (LIR)
//
//=============================================================================

/**
@class Move a value between two registers or between a register and a memory 
       location. This kind of LIR instruction should only appear after 
       register allocation.
@augments IRInstr
*/
function MoveInstr(from, to)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = "move";

    /**
    Inputs to the move instruction
    @field
    */
    this.uses = [from, to];
}
MoveInstr.prototype = new IRInstr();

/**
Produce a string representation of the move instruction
*/
MoveInstr.prototype.toString = function ()
{
    var output = "";

    output += this.mnemonic + ' ';

    for (i = 0; i < this.uses.length; ++i)
    {
        output += this.uses[i];

        if (i != this.uses.length - 1)
        {
            output += ", ";
        }
    }

    return output;
};

