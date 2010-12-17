/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO:
// May want MIR iflt, ifgt, ifeq, etc.
// May want specialized test for mask? intel test
// - Takes mask, compares result to 0
// - ifmask <mask> <value>, tests if result is value

// TODO:
// May want low-level load without masking
// - type, offset, index, multiplier
// Keep both load/store with and without mask
// - RawLoad? w/
//      - No masking,
//      - base_ptr + offset + index * multiplier

// TODO: cast type in load instruction, load as?

// TODO: separate instruction initFunc from validFunc?
// instr.validate()

//=============================================================================
//
// IR Core
//
// Implementation of the foundational logic of the IR instructions.
//
//=============================================================================

/**
@class IR type representation object
*/
function IRType(name, size)
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

    /**
    Type size in bits
    @field
    */
    this.numBits = size * 8;

    // If this is an integer type
    if (name[0] == 'i' || name[0] == 'u')
    {
        // Compute the available range
        this.minVal = getIntMin(this.numBits, name[0] == 'u');
        this.maxVal = getIntMax(this.numBits, name[0] == 'u');
    }
}
IRType.prototype = {};

/**
Obtain a string representation of an IR type
*/
IRType.prototype.toString = function ()
{
    return this.name;
};

/**
Test if the type is a pointer type
*/
IRType.prototype.isPtr = function ()
{
    return this === IRType.rptr ||
           this === IRType.box;
};

/**
Test if the type is an integer type
*/
IRType.prototype.isInt = function ()
{
    return this.isUnsigned() ||
           this.isSigned();
};

/**
Test if the type is an unsigned integer type
*/
IRType.prototype.isUnsigned = function ()
{
    return this === IRType.u8  ||
           this === IRType.u16 ||
           this === IRType.u32 ||
           this === IRType.u64;
};

/**
Test if the type is a signed integer type
*/
IRType.prototype.isSigned = function ()
{
    return this === IRType.i8  ||
           this === IRType.i16 ||
           this === IRType.i32 ||
           this === IRType.i64;
};

/**
Test if the type is a floating-point type
*/
IRType.prototype.isFP = function ()
{
    return this === IRType.f64;
};

/**
Test if the type is an integer or floating-point type
*/
IRType.prototype.isNumber = function ()
{
    return this.isInt() ||
           this.isFP();
};

// TODO: boxed and pointer type sizes are actually platform-dependent
// Need code get appropriate size for the platform

// Size of a pointer on the current platform
var PLATFORM_PTR_SIZE = 4;

// Type given when there is no output value
IRType.none = new IRType('none', 0),

// Boxed value type
// Contains an immediate integer or an object pointer, and a tag
IRType.box  = new IRType('box' , PLATFORM_PTR_SIZE),

// Raw pointer to any memory address
IRType.rptr = new IRType('rptr', PLATFORM_PTR_SIZE),

// Boolean type
IRType.bool = new IRType('bool', PLATFORM_PTR_SIZE),

// Unboxed unsigned integer types
IRType.u8   = new IRType('u8'  , 1),
IRType.u16  = new IRType('u16' , 2),
IRType.u32  = new IRType('u32' , 4),

// Unboxed signed integer types
IRType.i8   = new IRType('i8'  , 1),
IRType.i16  = new IRType('i16' , 2),
IRType.i32  = new IRType('i32' , 4),

// Floating-point types
IRType.f64  = new IRType('f64' , 8);

// If we are on a 32-bit platform
if (PLATFORM_PTR_SIZE == 4)
{
    // Int type of width corresponding a pointer on this platform
    IRType.pint = IRType.i32;
}

// Otherwise, we are on a 64-bit platform
else
{
    IRType.u64  = new IRType('u64' , 8),
    IRType.i64  = new IRType('i64' , 8),

    // Int type of width corresponding a pointer on this platform
    IRType.pint = IRType.i64;
}

/**
@class Base class for all IR values
*/
function IRValue()
{
    /**
    Get a string representation of a value's name
    */
    this.getValName = function () { return 'value'; };

    /**
    Produce a string representation of this value
    */
    this.toString = this.getValName;

    /**
    By default, all IR values have the boxed type
    */
    this.type = IRType.box;
}

/**
@class Represents constant values in the IR
@augments IRValue
*/
function ConstValue(value, type)
{
    // Ensure that the specified value is valid
    assert (
        !(type.isInt() && 
        (typeof value != 'number' || Math.floor(value) != value)),
        'integer constants require integer values'
    );
    assert (
        !(type.isFP() && typeof value != 'number'),
        'floating-point constants require number values'
    );
    assert (
        !(typeof value == 'string' && type !== IRType.box),
        'string-valued constants must have box type'
    );
    assert (
        !(type === IRType.bool && value != 0 && value != 1),
        'boolean constants must be 0 or 1'
    );        

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
    else if (typeof this.value == 'number')
    {
        return this.type + ':' + String(this.value);
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
Test if a constant is a number
*/
ConstValue.prototype.isNumber = function ()
{
    return (typeof this.value == 'number');
};

/**
Test if a constant is an integer
*/
ConstValue.prototype.isInt = function ()
{
    return (this.isNumber() && this.value == Math.floor(this.value));
};

/**
Test if a constant is a boxed integer
*/
ConstValue.prototype.isBoxInt = function ()
{
    return (
        this.type === IRType.box &&
        this.isInt() && 
        this.value >= getIntMin(BOX_NUM_BITS_INT) && 
        this.value <= getIntMax(BOX_NUM_BITS_INT)
    );
};

/**
Test if a constant is a string
*/
ConstValue.prototype.isString = function ()
{
    return (typeof this.value == 'string');
};

/**
Test if a constant is the undefined constant
*/
ConstValue.prototype.isUndef = function ()
{
    return this.value === undefined;
};

/**
Get the tag bits associated with a constant
*/
ConstValue.prototype.getTagBits = function ()
{
    assert (
        this.type === IRType.box,
        'tag bits only applicable to boxed values'
    );

    if (this.value instanceof IRFunction)
    {
        return TAG_FUNCTION;
    }

    if (this.isBoxInt())
    {
        return TAG_INT;
    }

    if (this.isNumber())
    {
        return TAG_FLOAT;
    }

    if (typeof this.value == 'string')
    {
        return TAG_STRING;
    }

    if (this.value === true || 
        this.value === false ||
        this.value === null || 
        this.value === undefined)
    {
        return TAG_OTHER;
    }

    assert (
        false,
        'cannot get tag bits for: ' + this
    );
};

/**
Get the immediate value (bit pattern) of a constant
*/
ConstValue.prototype.getImmValue = function ()
{
    if (this.isBoxInt())
    {
        return (this.value << TAG_NUM_BITS_INT);
    }

    if (this.type.isInt() && this.type !== IRType.box)
    {
        return this.value;
    }

    if (this.type === IRType.rptr)
    {
        return this.value;
    }

    if (this.type === IRType.bool)
    {
        return this.value;
    }

    if (this.value === true)
    {
        return BIT_PATTERN_TRUE;
    }

    if (this.value === false)
    {
        return BIT_PATTERN_FALSE;
    }

    if (this.value === undefined)
    {
        return BIT_PATTERN_UNDEF;
    }

    if (this.value === null)
    {
        return BIT_PATTERN_NULL;
    }

    assert (
        false,
        'cannot get immediate bits for: ' + this
    );
};

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
        type = IRType.box;

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
IRInstr.defOutFormat = function (instr)
{
    return instr.type.name + ' ' + instr.getValName();
};

/**
Default input string formatting function
*/
IRInstr.defInFormat = function (instr, pos)
{
    output = "";
    ins = instr.uses[pos];

    if (!(ins instanceof IRValue))
        output += "***invalid value***";
    else
        output += ins.getValName();

    return output;
};

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
    if (this.type != IRType.none)
        output += outFormatFn(this) + ' = ';

    output += this.mnemonic;

    // For each use
    for (i = 0; i < this.uses.length; ++i)
    {
        output += " " + inFormatFn(this, i);

        if (i != this.uses.length - 1)
            output += ",";
    }

    // For each branch target
    for (var i = 0; i < this.targets.length; ++i)
    {
        output += 
            (this.targetNames[i]? (' ' + this.targetNames[i]):'') + 
            ' ' + this.targets[i].getBlockName();
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
    assert (
        dest instanceof IRValue,
        'invalid dest value'
    );

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
    assert (
        newDest instanceof IRValue,
        'invalid replacement dest value'
    );

    for (var i = 0; i < this.dests.length; ++i)
    {
        if (this.dests[i] === oldDest)
            this.dests[i] = newDest;
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
};

/**
Test if this instruction writes to memory
*/
IRInstr.prototype.writesMem = function ()
{
    return false;
};

/**
Test if this instruction reads from memory
*/
IRInstr.prototype.readsMem = function ()
{
    return false;
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

    // Ensure that all values have the same type
    for (var i = 1; i < values.length; ++i)
    {
        assert (
            values[i].type === values[i-1].type,
            'all phi input values must have the same type'
        );
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
    this.type = (this.uses.length > 0) ? this.uses[0].type : IRType.none;
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

    var phiInFormatFn = function (instr, pos)
    {
        var pred = instr.preds[pos];

        return '[' + inFormatFn(instr, pos) + ' ' + pred.getBlockName() + ']';
    };

    var output = "";

    // If this instruction's type is not void, print its output name
    if (this.type != IRType.none)
        output += outFormatFn(this) + ' = ';

    output += this.mnemonic + ' ';

    for (i = 0; i < this.uses.length; ++i)
    {
        output += phiInFormatFn(this, i);

        if (i != this.uses.length - 1)
            output += ", ";
    }

    return output;
};

/**
Replace a predecessor block by another, keeping the corresponding use
*/
PhiInstr.prototype.replPred = function (oldPred, newPred)
{
    //print('Replacing pred for: ' + this + ', ' + oldPred.getBlockName() + ', ' + newPred.getBlockName());

    for (var i = 0; i < this.preds.length; ++i)
    {
        if (this.preds[i] === oldPred)
        {
            this.preds[i] = newPred;
            return;
        }
    }

    assert (
        false,
        'cannot replace pred, invalid pred'
    );
};

/**
Remove a phi predecessor and the corresponding use
*/
PhiInstr.prototype.remPred = function (pred)
{
    //print('Removing pred for: ' + this + ', ' + pred.getBlockName());

    // For each predecessor of the phi node
    for (var k = 0; k < this.preds.length; ++k)
    {
        // If this is a reference to the block
        if (this.preds[k] === pred)
        {
            // Get the corresponding use
            var use = this.uses[k];

            // Remove this value
            this.preds.splice(k, 1);
            this.uses.splice(k, 1);

            // If the value is no longer used, remove the dest link
            if (!arraySetHas(this.uses, use) && use instanceof IRInstr)
                use.remDest(this);

            // Break out of this loop
            return;
        }
    }
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
};

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
function ArgValInstr(type, argName, argIndex)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'arg';

    // Set the output name as the argument name
    this.outName = argName;

    // Set the argument index
    this.argIndex = argIndex;

    // Set the argument type
    this.type = type; 
}
ArgValInstr.prototype = new IRInstr();

/**
Get a string representation of the argument instruction
*/
ArgValInstr.prototype.toString = function (outFormatFn, inFormatFn)
{
    // Get the default toString output for the instruction
    var output = IRInstr.prototype.toString.apply(this, [outFormatFn, inFormatFn]);

    // Add the argument index to the output
    output += ' ' + this.argIndex;

    return output;
};

/**
Make a shallow copy of the instruction
*/
ArgValInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new ArgValInstr(
            this.type,
            this.outName,
            this.argIndex
        )
    );
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
    assert (
        mnemonic !== undefined,
        'mnemonic name not specified for instruction'
    );

    /**
    Parse instruction constructor arguments
    */
    function parseArgs(argArray, typeParams, inputVals, branchTargets)
    {
        var curIndex = 0;

        // Extract type parameters, if any
        for (; curIndex < argArray.length && argArray[curIndex] instanceof IRType; ++curIndex)
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
            var firstType = (inputVals.length > 0) ? inputVals[0].type : null;
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
                if (firstType !== null && firstType !== IRType.box)
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
                error.toString();
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
        if (branchNames !== undefined)
        {
            // Store the branch targets
            this.targets = branchTargets;
        }
    }

    // If no prototype object was specified, create one
    if (protoObj === undefined)
        protoObj = new IRInstr();

    // Set the prototype for the new instruction
    InstrConstr.prototype = protoObj;

    // Store the branch target names
    InstrConstr.prototype.targetNames = branchNames;

    // Store the initialization function
    if (initFunc !== undefined)
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
    var expectedStr;
    if (minExpected == maxExpected)
        expectedStr = String(minExpected);
    else if (maxExpected !== undefined)
        expectedStr = 'between ' + minExpected + ' and ' + maxExpected;
    else
        expectedStr = minExpected + ' or more';

    assert (
        array.length >= minExpected && 
        (maxExpected === undefined || array.length <= maxExpected),
        'got ' + array.length + ' ' +
        pluralize(name, array.length) + 
        ', expected ' + expectedStr
    );
};

/**
Function to validate the type paramers count of an instruction
*/
instrMaker.validNumParams = function (typeParams, minExpected, maxExpected)
{
    instrMaker.validCount('type parameter', typeParams, minExpected, maxExpected);
};

/**
Function to validate the argument count of an instruction
*/
instrMaker.validNumInputs = function (inputVals, minExpected, maxExpected)
{
    instrMaker.validCount('input value', inputVals, minExpected, maxExpected);
};

/**
Function to validate the branch targets of an instruction
*/
instrMaker.validNumBranches = function (branchTargets, minExpected, maxExpected)
{
    instrMaker.validCount('branch target', branchTargets, minExpected, maxExpected);
};

/**
Function to ensure that all values in an array are of boxed type
*/
instrMaker.allValsBoxed = function (inputVals)
{
    inputVals.forEach(
        function (val)
        {
            assert (
                val.type === IRType.box,
                'all input values must be boxed'
            );
        }
    );
};

/**
Function to ensure that all values in an array are of boxed type
*/
instrMaker.validType = function (value, expectedType)
{
    assert (
        value.type === expectedType,
        'got ' + value.type + ' value, expected ' + expectedType
    );
};

//=============================================================================
//
// Arithmetic operations without overflow handling
//
//=============================================================================

/**
@class Base class for arithmetic instructions
@augments IRInstr
*/
var ArithInstr = function ()
{
};
ArithInstr.prototype = new IRInstr();

/**
Default initialization function for arithmetic instructions
*/
ArithInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        (inputVals[0].type === IRType.box ||
         inputVals[0].type.isNumber())
        &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types'
    );
    
    this.type = inputVals[0].type;
};

/**
@class Addition instruction
@augments ArithInstr
*/
var AddInstr = instrMaker(
    'add',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 2, 2);

        assert (
            (inputVals[0].type === IRType.rptr &&
             inputVals[1].type === IRType.pint)
            ||
            (
                (inputVals[0].type === IRType.box ||
                 inputVals[0].type.isNumber())
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
        instrMaker.validNumInputs(inputVals, 2, 2);

        assert (
            (inputVals[0].type === IRType.rptr &&
             inputVals[1].type === IRType.pint)
            ||
            (inputVals[0].type === IRType.rptr &&
             inputVals[1].type === IRType.rptr)
            ||
            (
                (inputVals[0].type === IRType.box ||
                 inputVals[0].type.isNumber())
                &&
                inputVals[1].type === inputVals[0].type
            ),
            'invalid input types'
        );
        
        if (
            inputVals[0].type === IRType.rptr && 
            inputVals[1].type === IRType.rptr
        )
            this.type = IRType.pint;
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
var ArithOvfInstr = function ()
{
};
ArithOvfInstr.prototype = new IRInstr();

/**
Default initialization function for arithmetic instructions w/ overflow
*/
ArithOvfInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);
    assert (
        (inputVals[0].type === IRType.pint &&
         inputVals[1].type === inputVals[0].type)
        ||
        (inputVals[0].type === IRType.box &&
         inputVals[1].type === inputVals[0].type),
        'invalid input types'
    );
    
    this.type = inputVals[0].type;
};

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

/**
@class Instruction to left-shift integer values with overflow handling
@augments ArithOvfInstr
*/
var LsftOvfInstr = instrMaker(
    'lsft_ovf',
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
var BitOpInstr = function ()
{
};
BitOpInstr.prototype = new IRInstr();

/**
Default initialization function for bitwise operation instructions
*/
BitOpInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        (
            (inputVals[0].type === IRType.box ||
            inputVals[0].type === IRType.rptr)
            &&
            (inputVals[1].type === IRType.box ||
            inputVals[1].type === IRType.pint)
        )
        ||
        (inputVals[0].type.isInt() &&
         inputVals[1].type === inputVals[0].type),
        'invalid input types'
    );
    
    this.type = inputVals[1].type;
};

/**
@class Bitwise NOT instruction
@augments BitOpInstr
*/
var NotInstr = instrMaker(
    'not',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1, 1);

        assert (
            (inputVals[0].type === IRType.box ||
            inputVals[0].type.isInt()),
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
var CompInstr = function ()
{
};
CompInstr.prototype = new IRInstr();

/**
Default initialization function for comparison instructions
*/
CompInstr.prototype.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        (inputVals[0].type === IRType.box ||
         inputVals[0].type.isNumber())
        &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types'
    );
    
    this.type = IRType.bool;
};

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
var LeInstr = instrMaker(
    'le',
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
var GeInstr = instrMaker(
    'ge',
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
var NeInstr = instrMaker(
    'ne',
    undefined,
    undefined,
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
var JumpInstr = instrMaker(
    'jump',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 0, 0);
        
        this.type = IRType.none;
    },
    [undefined]
);

/**
Jump instructions are always branch instructions
*/
JumpInstr.prototype.isBranch = function () { return true; };

/**
@class Function return instruction
@augments IRInstr
*/
var RetInstr = instrMaker(
    'ret',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1, 1);
        
        this.type = IRType.none;
    }
);

/**
Ret instructions are always branch instructions
*/
RetInstr.prototype.isBranch = function () { return true; };

/**
@class If branching instruction
@augments IRInstr
*/
var IfInstr = instrMaker(
    'if',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1, 1);
        assert (
            inputVals[0].type === IRType.box ||
            inputVals[0].type === IRType.bool,
            'input must be boxed or bool'
        );
        instrMaker.validNumBranches(branchTargets, 2, 2);
        
        this.type = IRType.none;
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
var ExceptInstr = function ()
{
};
ExceptInstr.prototype = new IRInstr();

/**
Set the target block of the exception-producing instruction
*/
ExceptInstr.prototype.setThrowTarget = function (catchBlock)
{
    this.targets[0] = catchBlock;
};

/**
Get the target block of the exception-producing instruction
*/
ExceptInstr.prototype.getThrowTarget = function ()
{
    return this.targets[0]? this.targets[0]:null;
};

/**
@class Exception throw to exception handler. Handler may be left undefined for
interprocedural throw.
@augments ExceptInstr
*/
var ThrowInstr = instrMaker(
    'throw',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1, 1);
        instrMaker.validType(inputVals[0], IRType.box);
        instrMaker.validNumBranches(branchTargets, 0, 1);
        
        this.type = IRType.none;
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
};

/**
@class Exception value catch
@augments IRInstr
*/
var CatchInstr = instrMaker(
    'catch',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 0, 0);
    }
);

/**
@class Base class for call instructions
@augments ExceptInstr
*/
var CallInstr = function ()
{
};
CallInstr.prototype = new ExceptInstr();

/**
By default, conservatively assume all calls read and write to/from memory
*/
CallInstr.prototype.writesMem = function () { return true; };
CallInstr.prototype.readsMem = function () { return true; };

/**
Set the continue block of the call instruction
*/
CallInstr.prototype.setContTarget = function (contBlock)
{
    this.targets[0] = contBlock;

    while (!this.targets[this.targets.length-1])
        this.targets.pop();
};

/**
Get the continuation target block of the call instruction
*/
CallInstr.prototype.getContTarget = function ()
{
    return (this.targets.length > 0) ? this.targets[0] : null;
};

/**
Set the throw target block of the call instruction
*/
CallInstr.prototype.setThrowTarget = function (catchBlock)
{
    this.targets = catchBlock? [this.targets[0], catchBlock]:[this.targets[0]];

    while (!this.targets[this.targets.length-1])
        this.targets.pop();
};

/**
Get the throw target block of the call instruction
*/
CallInstr.prototype.getThrowTarget = function ()
{
    return (this.targets.length > 1) ? this.targets[1] : null;
};

/**
@class Function call instruction
@augments CallInstr
*/
var CallFuncInstr = instrMaker(
    'call',
    function (typeParams, inputVals, branchTargets)
    {
        this.mnemonic = 'call';

        instrMaker.validNumInputs(inputVals, 2);
        instrMaker.validType(inputVals[0], IRType.box);
        instrMaker.validType(inputVals[1], IRType.box);
        instrMaker.validNumBranches(branchTargets, 0, 2);

        if (inputVals[0] instanceof IRFunction)
        {
            assert (
                inputVals.length  - 2 == inputVals[0].getNumArgs(),
                'direct calls do not support variable argument counts, got ' +
                (inputVals.length - 2) + ' arguments, expected ' + 
                inputVals[0].getNumArgs() + ' (' + inputVals[0].funcName + ')'
            );

            for (var i = 2; i < inputVals.length; ++i)
            {
                assert (
                    inputVals[i].type === inputVals[0].argTypes[i-2],
                    'argument type does not match (' + 
                    inputVals[0].argVars[i-2].toString() + ', ' +
                    inputVals[0].funcName + ')'
                );
            }

            this.type = inputVals[0].retType;
        }
        else
        {
            for (var i = 2; i < inputVals.length; ++i)
            {
                assert (
                    inputVals[0].type === IRType.box,
                    'indirect calls can only take boxed values as input'
                );
            }

            this.type = IRType.box;
        }
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
Test if a call instruction writes to memory
*/
CallFuncInstr.prototype.writesMem = function ()
{
    if (this.uses[0] instanceof IRFunction)
        return this.uses[0].writesMem;
    else
        return true;
};

/**
Test if a call instruction reads from memory
*/
CallFuncInstr.prototype.readsMem = function ()
{
    if (this.uses[0] instanceof IRFunction)
        return this.uses[0].readsMem;
    else
        return true;
};

/**
@class Constructor call with function object reference
@augments CallInstr
*/
var ConstructInstr = instrMaker(
    'construct',
    function (typeParams, inputVals, branchTargets)
    {
        this.mnemonic = 'construct';

        instrMaker.validNumInputs(inputVals, 2);
        instrMaker.validType(inputVals[0], IRType.box);
        instrMaker.validType(inputVals[1], IRType.box);
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.box;

        for (var i = 2; i < inputVals.length; ++i)
        {
            assert (
                inputVals[0].type === IRType.box,
                'constructor calls can only take boxed values as input'
            );
        }
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
@class Instruction to convert between different integer types
@augments IRInstr
*/
var ICastInstr = instrMaker(
    'icast',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(typeParams, 1, 1);
        instrMaker.validNumInputs(inputVals, 1, 1);
        assert (
            (inputVals[0].type.isInt() || 
             inputVals[0].type === IRType.box ||
             inputVals[0].type === IRType.rptr) 
            &&
            (typeParams[0].isInt() ||
             typeParams[0] === IRType.box ||
             typeParams[0] === IRType.rptr),
            'type parameters must be integer or raw pointer'
        );
        
        this.type = typeParams[0];
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
        instrMaker.validNumParams(inputVals, 1, 1);
        instrMaker.validNumInputs(inputVals, 1, 1);
        assert (
            inputVals[0].type === IRType.pint &&
            typeParams[0] === IRType.f64,
            'invalid type parameters'
        );
        
        this.type = typeParams[0];
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
        instrMaker.validNumParams(inputVals, 1, 1);
        instrMaker.validNumInputs(inputVals, 1, 1);
        assert (
            typeParams[0].type === IRType.f64 &&
            typeParams[0] === IRType.pint,
            'invalid type parameters'
        );
        
        this.type = typeParams[0];
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
        instrMaker.validNumParams(typeParams, 1, 1);
        instrMaker.validNumInputs(inputVals, 2, 2);
        assert (
            inputVals[0].type.isPtr(),
            'the first input must be a pointer'
        );
        instrMaker.validType(inputVals[1], IRType.pint);
        
        this.type = typeParams[0];
    }
);

/**
Load instructions always read from memory
*/
LoadInstr.prototype.readsMem = function () { return true; };

/**
@class Instruction to store a value to memory
@augments IRInstr
*/
var StoreInstr = instrMaker(
    'store',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumParams(typeParams, 1, 1);
        instrMaker.validNumInputs(inputVals, 3, 3);
        assert (
            inputVals[0].type.isPtr(),
            'the first input must be a pointer'
        );
        instrMaker.validType(inputVals[1], IRType.pint);
        instrMaker.validType(inputVals[2], typeParams[0]);
   
        this.type = IRType.none;
    }
);

/**
Store instructions always write to memory
*/
StoreInstr.prototype.writesMem = function () { return true; };

/**
@class Instruction to get a pointer to the current runtime context
@augments IRInstr
*/
var GetCtxInstr = instrMaker(
    'get_ctx',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 0, 0);
        
        this.type = IRType.rptr;
    }
);

/**
Get context instructions always read from memory
*/
GetCtxInstr.prototype.readsMem = function () { return true; };

/**
@class Instruction to set the runtime context pointer
@augments IRInstr
*/
var SetCtxInstr = instrMaker(
    'set_ctx',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 1, 1);
        instrMaker.validType(inputVals[0], IRType.rptr);
        
        this.type = IRType.none;
    }
);

/**
Set context instructions always write to memory
*/
SetCtxInstr.prototype.writesMem = function () { return true; };

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

