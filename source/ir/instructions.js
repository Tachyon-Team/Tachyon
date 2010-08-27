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
Map of values to IR constants
*/
ConstValue.constMap = new HashMap();

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

    output += this.mnemonic + (this.uses.length? ' ':'');

    for (i = 0; i < this.uses.length; ++i)
    {
        var ins = this.uses[i];

        if (!(ins instanceof IRValue))
            output += '***invalid value***';
        else
            output += inFormatFn(ins);

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
Returns a use iterator. Iterates through uses from left to right.
*/
IRInstr.prototype.getUseItr = function ()
{
    return new ArrayIterator(this.uses);
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
By default, branch instructions produce no output
@field
*/
BranchInstr.prototype.type = IRType.VOID;

/**
Branch target names, not defined by default
@field
*/
BranchInstr.prototype.targetNames = [];

/**
Default toString function for branch instructions
*/
BranchInstr.prototype.toString = function (outFormatFn, inFormatFn)
{
    // Get the default toString output for the instruction and its uses
    var output = IRInstr.prototype.toString.apply(this, outFormatFn, inFormatFn);

    // For each branch target
    for (var i = 0; i < this.targets.length; ++i)
    {
        output += 
            (this.targetNames[i]? (' ' + this.targetNames[i]):'') + 
            ' ' + this.targets[i].getBlockName()
        ;
    }

    return output;
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
Function to generate typed instruction constructors using closures
@param mnemonic mnemonic name of the instruction
@param typeParSpecs list of type parameter specifications
@param inTypeSpecs list of arrays of input value type
@param outTypeSpecs list of output value types
@param branchNames list of branch target names
@param protoObj prototype object for the instruction
*/
function TypedInstrMaker(
    mnemonic,
    typeParSpecs,
    inTypeSpecs,
    outTypeSpecs,
    branchNames,
    protoObj
)
{
    // If only one input type specification is present
    if (inTypeSpecs instanceof Array && !(inTypeSpecs[0] instanceof Array))
    {
        assert (
            typeParSpecs instanceof Array,
            'invalid type parameter specifications'
        );

        // Copy the input type for each type param specification
        var inType = inTypeSpecs;
        inTypeSpecs = [];
        for (var i = 0; i < typeParSpecs.length; ++i)
            inTypeSpecs.push(inType);
    }
    else
    {
        assert (
            !typeParSpecs || typeParSpecs.length == inTypeSpecs.length,
            'when type parameter specifications are provided, the number of ' +
            'type parameter and input type specifications must match'
        );
    }

    // If only one output type specification is present
    if (!(outTypeSpecs instanceof Array))
    {
        // Copy the output type for each input type specification
        var outType = outTypeSpecs;
        outTypeSpecs = [];
        for (var i = 0; i < inTypeSpecs.length; ++i)
            outTypeSpecs.push(outType);
    }
    else
    {
        assert (
            inTypeSpecs.length == outTypeSpecs.length,
            'the number of input and output type specifications must match'
        );
    }

    // Validate input type specifications
    for (var i = 0; i < inTypeSpecs.length; ++i)
    {
        var typeSpec = inTypeSpecs[i];
        for (var j = 0; j < typeSpec.length; ++j)
        {
            assert (
                typeSpec[j] instanceof IRTypeObj,
                'invalid type in input type specification for ' + mnemonic
            );
        }
    }

    // Validate output type specifications
    for (var i = 0; i < outTypeSpecs.length; ++i)
    {
        assert (
            outTypeSpecs[i] instanceof IRTypeObj,
            'invalid type in output type specification for ' + mnemonic
        );
    }

    // Create lists for the mnemonic instruction names
    var mnemonicNames = [];

    // If type parameter specifications are provided
    if (typeParSpecs)
    {
        // Ensure that the type parameter specifications are in an array
        assert (
            typeParSpecs instanceof Array,
            'invalid type parameter specifications array'
        );

        // Get the number of type parameters
        var numTypeParams = typeParSpecs[0].length;

        // For each type parameter specification
        for (var i = 0; i < typeParSpecs.length; ++i)
        {
            // Get this type parameter specification
            var typeSpec = typeParSpecs[i];

            // Ensure all specifications have the same length
            assert (
                typeSpec.length == numTypeParams,
                'invalid type parameter specification'
            );

            // Set the mnemonic name for this type parameter specification
            var mnemName = mnemonic;
            for (var j = 0; j < numTypeParams; ++j)
            {
                var type = typeSpec[j];

                assert (
                    type instanceof IRTypeObj,
                    'invalid type in type specification'
                );

                if (type !== IRType.BOXED || numTypeParams > 0)
                    mnemName += '_' + type.name;
            }

            // Add the mnemonic name to the list
            mnemonicNames.push(mnemName);
        }
    }

    // Otherwise, no type parameter specifications are provided
    else
    {
        // For each input type specification
        for (var i = 0; i < inTypeSpecs.length; ++i)
        {
            // Get this type parameter specification
            var typeSpec = inTypeSpecs[i];

            // Verify if all types in the specification are the same
            var firstType = typeSpec.length? typeSpec[0]:null;
            var allSame = true;
            for (var j = 0; j < typeSpec.length; ++j)
            {
                if (typeSpec[j] !== firstType)            
                    allSame = false;
            }

            // Set the mnemonic name for this type parameter specification
            var mnemName = mnemonic;

            // If all input types are the same
            if (allSame)
            {
                // If there is a first type and it is not boxed
                if (firstType !== null && firstType !== IRType.BOXED)
                {
                    // Append the input type to the name
                    mnemName += '_' + firstType.name;
                }
            }
            else
            {
                // Append all input types to the name
                for (var j = 0; j < typeSpec.length; ++j)
                    mnemName += '_' + typeSpec[j].name;
            }

            // Add the mnemonic name to the list
            mnemonicNames.push(mnemName);
        }
    }

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
    Find a type parameter or input type specification
    */
    function findSpec(typeSpec, specArray)
    {
        var specIndex = -1;

        // Find the type specification from the type values
        SPEC_LOOP:
        for (var i = 0; i < specArray.length; ++i)
        {
            var curSpec = specArray[i];

            // If the number of type values does not match the spec length, continue
            if (typeSpec.length != curSpec.length)
                continue SPEC_LOOP;

            // If any type value does not match, continue
            for (var j = 0; j < typeSpec.length; ++j)
            {
                if (typeSpec[j] !== curSpec[j])
                    continue SPEC_LOOP;
            }

            // The spec was found, break out of the loop
            specIndex = i;
            break;
        }

        assert (
            specIndex != -1,
            'invalid type specification passed to ' + mnemonic + ' constructor'
        );

        return specIndex;
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

        // If type parameters are to be used
        if (typeParSpecs)
        {
            // Find the type specifications based on the type parameters
            var specIndex = findSpec(typeParams, typeParSpecs);
   
            // Store the type parameters of the instruction
            this.typeParams = typeParams;
        }
        else
        {
            // Find the type specifications based on the input value types
            var specIndex = findSpec(
                inputVals.map(function (val) { return val.type; }), 
                inTypeSpecs
            );
        }

        // Set the mnemonic name of the instruction
        this.mnemonic = mnemonicNames[specIndex];

        // Set the output type for the instruction
        this.type = outTypeSpecs[specIndex];

        // Store the uses of the instruction
        this.uses = inputVals.slice(0);

        // If this is a branch instruction
        if (branchNames)
        {
            // Ensure that the right number of branch targets were specified
            assert (
                branchTargets.length == branchNames.length,
                'must specify ' + branchNames.length + ' branch targets to ' + mnemonic + ' constructor'
            );

            // Ensure that the branch targets are valid
            branchTargets.forEach(
                function (t)
                {
                    assert (
                        t instanceof BasicBlock,
                        'invalid branch target passed to ' + mnemonic + ' constructor'
                    );
                }
            );

            // Store the branch targets
            this.targets = branchTargets;
        }
    }

    // If no prototype object was specified, create one
    if (!protoObj)
    {
        if (branchNames)
            protoObj = new BranchInstr();
        else
            protoObj = new IRInstr();
    }

    // Set the constructor for the new instruction
    InstrConstr.prototype = protoObj;

    // Store the branch target names
    InstrConstr.prototype.targetNames = branchNames;

    /**
    Generic instruction shallow copy function
    */
    InstrConstr.prototype.copy = function ()
    {
        // Setup the copy arguments
        var copyArgs = [];
        if (this.typeParams) copyArgs = copyArgs.concat(this.typeParams.slice(0));
        copyArgs = copyArgs.concat(this.uses.slice(0));
        if (this.targets) copyArgs = copyArgs.concat(this.targets.slice(0));

        // Return a new instruction with the same type parameters, uses and targets
        return this.baseCopy(new InstrConstr(copyArgs));
    };

    // Return the new constructor instance
    return InstrConstr;
}









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
    protoObj,
    nameFunc
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

        // Call the name setting function
        this.nameFunc(typeParams, inputVals);

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
    {
        if (branchNames)
            protoObj = new BranchInstr();
        else
            protoObj = new IRInstr();
    }

    // Set the prototype for the new instruction
    InstrConstr.prototype = protoObj;

    // Store the branch target names
    InstrConstr.prototype.targetNames = branchNames;

    // Store the initialization function
    if (initFunc)
        InstrConstr.prototype.initFunc = initFunc;

    // Store the name setting function
    InstrConstr.prototype.nameFunc = nameFunc? nameFunc:setName;

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
Function to validate the type paramers count of an instruction
*/
instrMaker.validNumParams = function (typeParams, numExpected)
{
    assert (
        typeParams.length == numExpected,
        'got ' + typeParams.length + 
        pluralize(' type parameter', typeParams.length) + 
        ', expected ' + numExpected
    );
}

/**
Function to validate the argument count of an instruction
*/
instrMaker.validNumInputs = function (inputVals, minExpected, maxExpected)
{
    if (maxExpected === undefined)
        maxExpected = minExpected;

    var expectedStr;
    if (minExpected == maxExpected)
        expectedStr = String(minExpected);
    else if (maxExpected === Infinity)
        expectedStr = 'between ' + minExpected + ' and ' + maxExpected;
    else
        expectedStr = minExpected + ' or more'

    assert (
        inputVals.length >= minExpected && inputVals.length <= maxExpected,
        'got ' + inputVals.length + 
        pluralize(' argument', inputVals.length) + 
        ', expected ' + expectedStr
    );
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









// TODO: this instruction has no typed equivalend, like Neq, Nseq
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
@class Instruction to get an array containing the property names of an object
@augments IRInstr
*/
var JumpInstr = untypedInstrMaker(
    'jump',
     0,
    [undefined],
    true
);

/**
@class If conditional test instruction
@augments BranchInstr
*/
var IfInstr = untypedInstrMaker(
    'if',
    1,
    ['then', 'else'],
    true
);

/**
@class Function return instruction
@augments BranchInstr
*/
var RetInstr = untypedInstrMaker(
    'ret',
     1,
    undefined,
    true,
    new BranchInstr()
);

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
Branch target name for the throw instruction
@field
*/
ThrowInstr.prototype.targetNames = ['to'];

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
var CatchInstr = untypedInstrMaker(
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
@field
*/
CallRefInstr.prototype.type = IRType.BOXED;

/**
Branch target name for the call instruction
@field
*/
CallRefInstr.prototype.targetNames = ['continue', 'throw'];

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
Set the continue block of the call instruction
*/
CallRefInstr.prototype.setContTarget = function (contBlock)
{
    this.targets[0] = contBlock;
}

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
        new ConstructRefInstr(
            this.uses[0],
            this.uses.slice[1],
            this.targets[0],
            this.targets[1]
        )
    );
};

/**
@class Property set with value for field name
@augments CallRefInstr
*/
function PutPropValInstr(object, propName, propVal, contBlock, catchBlock)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'put_prop_val';

    /**
    Object, property name and propery value
    @field
    */
    this.uses = [object, propName, propVal];

    /**
    Catch block and continue block for the exception handler
    @field
    */
    this.targets = catchBlock? [contBlock, catchBlock]:[contBlock];
}
PutPropValInstr.prototype = new CallRefInstr();

/**
Put instructions produce no output value
@field
*/
PutPropValInstr.prototype.type = IRType.VOID;

/**
Make a shallow copy of the instruction
*/
PutPropValInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new PutPropValInstr(
            this.uses[0],
            this.uses[1],
            this.uses[2],
            this.targets[0],
            this.targets[1]
        )
    );
};

/**
@class Property get with value for field name
@augments CallRefInstr
*/
function GetPropValInstr(object, propName, contBlock, catchBlock)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'get_prop_val';

    /**
    Object and property name
    @field
    */
    this.uses = [object, propName];

    /**
    Catch block and continue block for the exception handler
    @field
    */
    this.targets = catchBlock? [contBlock, catchBlock]:[contBlock];
}
GetPropValInstr.prototype = new CallRefInstr();

/**
Make a shallow copy of the instruction
*/
GetPropValInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new GetPropValInstr(
            this.uses[0],
            this.uses[1],
            this.targets[0],
            this.targets[1]
        )
    );
};

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
     0
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

//====================================================
// Memory access instructions
//====================================================

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

//====================================================
// Type conversion instructions
//====================================================

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
            typeParams[0] === IRType.OBJPTR || typeParams[0] === IRType.PLATFORM_INT,
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

//====================================================
// Arithmetic operations w/o overflow handling
//====================================================

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

// TODO: replace regular add instruction
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

// TODO: replace regular sub instruction
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

// TODO: replace regular mul instruction
var MulInstr = instrMaker(
    'mul',
    undefined,
    undefined,
    new ArithInstr()
);

// TODO: replace regular div instruction
var DivInstr = instrMaker(
    'div',
    undefined,
    undefined,
    new ArithInstr()
);

// TODO: replace regular mod instruction
var ModInstr = instrMaker(
    'mod',
    undefined,
    undefined,
    new ArithInstr()
);

//====================================================
// Bitwise operations
//====================================================

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

// TODO: replace regular instruction
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

// TODO: replace regular instruction
var AndInstr = instrMaker(
    'and',
    undefined,
    undefined,
    new BitOpInstr()
);

// TODO: replace regular instruction
var OrInstr = instrMaker(
    'or',
    undefined,
    undefined,
    new BitOpInstr()
);

// TODO: replace regular instruction
var XorInstr = instrMaker(
    'xor',
    undefined,
    undefined,
    new BitOpInstr()
);

// TODO: replace regular instruction
var LsftInstr = instrMaker(
    'lsft',
    undefined,
    undefined,
    new BitOpInstr()
);

// TODO: replace regular instruction
var RsftInstr = instrMaker(
    'rsft',
    undefined,
    undefined,
    new BitOpInstr()
);

// TODO: replace regular instruction
var UrsftInstr = instrMaker(
    'ursft',
    undefined,
    undefined,
    new BitOpInstr()
);

//====================================================
// Comparison instructions
//====================================================

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

// TODO: replace regular instruction
var LtInstr = instrMaker(
    'lt',
    undefined,
    undefined,
    new CompInstr()
);

// TODO: replace regular instruction
var LteInstr = instrMaker(
    'lte',
    undefined,
    undefined,
    new CompInstr()
);

// TODO: replace regular instruction
var GtInstr = instrMaker(
    'gt',
    undefined,
    undefined,
    new CompInstr()
);

// TODO: replace regular instruction
var GteInstr = instrMaker(
    'gte',
    undefined,
    undefined,
    new CompInstr()
);

// TODO: replace regular instruction
var EqInstr = instrMaker(
    'eq',
    undefined,
    undefined,
    new CompInstr()
);

// TODO: replace regular instruction
var NeqInstr = instrMaker(
    'neq',
    undefined,
    undefined,
    new CompInstr()
);

/**
@class Strict-equality comparison instruction
@augments IRInstr
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
@augments IRInstr
*/
var NseqInstr = untypedInstrMaker(
    'nseq',
     2,
    undefined,
    false,
    new CompInstr()
);

//====================================================
// Branch instructions
//====================================================

/**
@class Specialized if instruction taking only booleans as input
@augments BranchInstr
*/
var IfBoolInstr = TypedInstrMaker(
    'if',
    undefined,
    [
        [IRType.INT8]
    ],
    IRType.VOID,
    ['then', 'else']
);

/**
@class Instruction to add integer values with overflow handling
@augments BranchInstr
*/
var IAddOvfInstr = TypedInstrMaker(
    'add_ovf',
    undefined,
    [
        [IRType.PLATFORM_INT, IRType.PLATFORM_INT]
    ],
    [
        IRType.PLATFORM_INT
    ],
    ['normal', 'overflow']
);

/**
@class Instruction to subtract integer values with overflow handling
@augments BranchInstr
*/
var ISubOvfInstr = TypedInstrMaker(
    'sub_ovf',
    undefined,
    [
        [IRType.PLATFORM_INT, IRType.PLATFORM_INT]
    ],
    [
        IRType.PLATFORM_INT
    ],
    ['normal', 'overflow']
);

/**
@class Instruction to multiply integer values with overflow handling
@augments BranchInstr
*/
var IMulOvfInstr = TypedInstrMaker(
    'mul_ovf',
    undefined,
    [
        [IRType.PLATFORM_INT, IRType.PLATFORM_INT]
    ],
    [
        IRType.PLATFORM_INT
    ],
    ['normal', 'overflow']
);

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
// Is LIR actually needed?

// TODO: LIR instructions, even closer to machine arch? Not SSA?

// TODO: MoveInstr? LIR only
// No SSA output

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

