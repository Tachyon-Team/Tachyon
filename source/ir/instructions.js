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

// IR value type enumeration
IRType =
{
    // TODO: boxed and pointer type sizes are actually platform-dependent
    // Need code get appropriate size for the platform

    VOID:       new IRTypeObj('void', 0),   // No output value
    BOXED:      new IRTypeObj('box' , 8),   // Boxed value type
    POINTER:    new IRTypeObj('ptr' , 8),   // Unboxed pointer
    UINT8:      new IRTypeObj('u8'  , 1),   // Unboxed uint8
    UINT16:     new IRTypeObj('u16' , 2),   // Unboxed uint16
    UINT32:     new IRTypeObj('u32' , 4),   // Unboxed uint32
    UINT64:     new IRTypeObj('u64' , 8),   // Unboxed uint64
    INT8:       new IRTypeObj('i8'  , 1),   // Unboxed int8
    INT16:      new IRTypeObj('i16' , 2),   // Unboxed int16
    INT32:      new IRTypeObj('i32' , 4),   // Unboxed int32
    INT64:      new IRTypeObj('i64' , 8),   // Unboxed int64
    FLOAT32:    new IRTypeObj('f32' , 4),   // Unboxed float32
    FLOAT64:    new IRTypeObj('f64' , 8)    // Unboxed float64
};

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
Produce a string representation of this instruction
*/
IRInstr.prototype.toString = function ()
{
    var output = "";

    // If this instruction's value is read, print its output name
    if (this.hasDests())
        output += this.type.name + ' ' + this.getValName() + ' = ';

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
Returns an operand iterator.  Iterates through operands from left to right.
*/
IRInstr.prototype.getOpndItr = function ()
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
BranchInstr.prototype.toString = function ()
{
    // Get the default toString output for the instruction and its uses
    var output = IRInstr.prototype.toString.apply(this);

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
PhiInstr.prototype.toString = function ()
{
    var output = "";

    // If this instruction's value is read, print its output name
    if (this.hasDests())
        output += this.type.name + ' ' + this.getValName() + ' = ';

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
            'when type parameter specifications are provided, the number of \
            type parameter and input type specifications must match'
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
                'invalid type in type specification'
            );
        }
    }

    // Validate output type specifications
    for (var i = 0; i < outTypeSpecs.length; ++i)
    {
        assert (
            outTypeSpecs[i] instanceof IRTypeObj,
            'invalid type in type specification'
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
    function parseArgs(argArray, typeParams, inputValues, branchTargets)
    {
        var curIndex = 0;

        // Extract type parameters, if any
        for (; curIndex < argArray.length && argArray[curIndex] instanceof IRTypeObj; ++curIndex)
            typeParams.push(argArray[curIndex]);

        // Extract input values, if any
        for (; curIndex < argArray.length && argArray[curIndex] instanceof IRValue; ++curIndex)
            inputValues.push(argArray[curIndex]);

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
        var inputValues = [];
        var branchTargets = [];
        if (inputs instanceof Array)
            parseArgs(inputs, typeParams, inputValues, branchTargets);
        else
            parseArgs(arguments, typeParams, inputValues, branchTargets);

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
                inputValues.map(function (val) { return val.type; }), 
                inTypeSpecs
            );
        }

        // Set the mnemonic name of the instruction
        this.mnemonic = mnemonicNames[specIndex];

        // Set the output type for the instruction
        this.type = outTypeSpecs[specIndex];

        // Store the uses of the instruction
        this.uses = inputValues.slice(0);

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
Function to generate generic untyped instruction constructors using closures
@param mnemonic mnemonic name for the instruction
@param numInputs number of input operands
@param protoObj prototype object instance, new IRInstr instance by default
*/
function UntypedInstrMaker(mnemonic, numInputs, branchNames, protoObj)
{
    var inTypes = [];
    for (var i = 0; i < numInputs; ++i)
        inTypes.push(IRType.BOXED);

    return TypedInstrMaker(
        mnemonic,
        undefined,
        [inTypes],
        IRType.BOXED,
        branchNames,
        protoObj
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
ArgValInstr.prototype.toString = function ()
{
    return this.type.name + ' ' + this.outName + ' = ' +
         this.mnemonic + ' ' + this.argIndex;
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
var AddInstr = UntypedInstrMaker(
    'add',
     2
);

/**
@class Arithmetic subtraction instruction
@augments IRInstr
*/
var SubInstr = UntypedInstrMaker(
    'sub',
     2
);

/**
@class Arithmetic multiply instruction
@augments IRInstr
*/
var MulInstr = UntypedInstrMaker(
    'mul',
     2
);

/**
@class Arithmetic divide instruction
@augments IRInstr
*/
var DivInstr = UntypedInstrMaker(
    'div',
     2
);

/**
@class Arithmetic modulo instruction
@augments IRInstr
*/
var ModInstr = UntypedInstrMaker(
    'mod',
     2
);

/**
@class Logical negation instruction
@augments IRInstr
*/
var LogNotInstr = UntypedInstrMaker(
    'not',
     1
);

/**
@class Bitwise NOT instruction
@augments IRInstr
*/
var BitNotInstr = UntypedInstrMaker(
    'not',
     1
);

/**
@class Bitwise AND instruction
@augments IRInstr
*/
var BitAndInstr = UntypedInstrMaker(
    'and',
     2
);

/**
@class Bitwise AND instruction
@augments IRInstr
*/
var BitOrInstr = UntypedInstrMaker(
    'and',
     2
);

/**
@class Bitwise XOR instruction
@augments IRInstr
*/
var BitXorInstr = UntypedInstrMaker(
    'xor',
     2
);

/**
@class Left shift instruction
@augments IRInstr
*/
var LsftInstr = UntypedInstrMaker(
    'lsft',
     2
);

/**
@class Right shift instruction
@augments IRInstr
*/
var RsftInstr = UntypedInstrMaker(
    'rsft',
     2
);

/**
@class Unsigned right shift instruction
@augments IRInstr
*/
var UrsftInstr = UntypedInstrMaker(
    'ursft',
     2
);

/**
@class Less-than comparison instruction
@augments IRInstr
*/
var LtInstr = UntypedInstrMaker(
    'lt',
     2
);

/**
@class Less-than-or-equal comparison instruction
@augments IRInstr
*/
var LteInstr = UntypedInstrMaker(
    'lte',
     2
);

/**
@class Greater-than comparison instruction
@augments IRInstr
*/
var GtInstr = UntypedInstrMaker(
    'gt',
     2
);

/**
@class Greater-than-or-equal comparison instruction
@augments IRInstr
*/
var GteInstr = UntypedInstrMaker(
    'gte',
     2
);

/**
@class Equality comparison instruction
@augments IRInstr
*/
var EqInstr = UntypedInstrMaker(
    'eq',
     2
);

/**
@class Inequality comparison instruction
@augments IRInstr
*/
var NeqInstr = UntypedInstrMaker(
    'neq',
     2
);

/**
@class Strict-equality comparison instruction
@augments IRInstr
*/
var SeqInstr = UntypedInstrMaker(
    'seq',
     2
);

/**
@class Strict-inequality comparison instruction
@augments IRInstr
*/
var NseqInstr = UntypedInstrMaker(
    'nseq',
     2
);

/**
@class Type query instruction
@augments IRInstr
*/
var TypeOfInstr = UntypedInstrMaker(
    'typeof',
     1
);

/**
@class Instance/class query instruction
@augments IRInstr
*/
var InstOfInstr = UntypedInstrMaker(
    'instanceof',
     2
);

/**
@class Property set with value for field name
@augments IRInstr
*/
var PutPropValInstr = UntypedInstrMaker(
    'put_prop_val',
     3
);

/**
@class Property get with value for field name
@augments IRInstr
*/
var GetPropValInstr = UntypedInstrMaker(
    'get_prop_val',
     2
);

/**
@class Property deletion with value for field name
@augments IRInstr
*/
var DelPropValInstr = UntypedInstrMaker(
    'del_prop_val',
     2
);

/**
@class Property test with value for field name
@augments IRInstr
*/
var HasPropValInstr = UntypedInstrMaker(
    'has_prop_val',
     2
);

/**
@class Instruction to get an array containing the property names of an object
@augments IRInstr
*/
var GetPropNamesInstr = UntypedInstrMaker(
    'get_prop_names',
     1
);

/**
@class Instruction to get an array containing the property names of an object
@augments IRInstr
*/
var JumpInstr = UntypedInstrMaker(
    'jump',
     0,
    [undefined]
);

/**
@class If conditional test instruction
@augments BranchInstr
*/
var IfInstr = UntypedInstrMaker(
    'if',
    1,
    ['then', 'else']
);

/**
@class Function return instruction
@augments BranchInstr
*/
var RetInstr = UntypedInstrMaker(
    'ret',
     1,
    undefined,
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
var CatchInstr = UntypedInstrMaker(
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
var MakeCellInstr = UntypedInstrMaker(
    'make_cell',
     0
);

/**
@class Get the value stored in a mutable cell
@augments IRInstr
*/
var GetCellInstr = UntypedInstrMaker(
    'get_cell',
     1
);

/**
@class Set the value stored in a mutable cell
@augments IRInstr
*/
var PutCellInstr = UntypedInstrMaker(
    'put_cell',
     2
);

/**
@class Closure creation with closure variable arguments
@augments IRInstr
*/
function MakeClosInstr(funcVal, globalObj, varVals)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'make_clos';

    /**
    Function value, global object, and closure variable values
    @field
    */
    this.uses = [funcVal, globalObj].concat(varVals);
}
MakeClosInstr.prototype = new IRInstr();

/**
Make a shallow copy of the instruction
*/
MakeClosInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new MakeClosInstr(
            this.uses[0],
            this.uses[1],
            this.uses.slice[2]
        )
    );
};

/**
@class Get the global value stored in a closure
@augments IRInstr
*/
var GetGlobalInstr = UntypedInstrMaker(
    'get_global',
     1
);

/**
@class Get the value stored in a closure variable
@augments IRInstr
*/
var GetClosInstr = UntypedInstrMaker(
    'get_clos',
     2
);

/**
@class Set the value stored in a closure variable
@augments IRInstr
*/
var PutClosInstr = UntypedInstrMaker(
    'put_clos',
     3
);

/**
@class Instruction to create a new, empty object
@augments IRInstr
*/
var NewObjectInstr = UntypedInstrMaker(
    'new_object',
     0
);

/**
@class Instruction to create a new, empty array
@augments IRInstr
*/
var NewArrayInstr = UntypedInstrMaker(
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
var LoadInstr = TypedInstrMaker(
    'load',
    [
        [IRType.BOXED],
        [IRType.POINTER],
        [IRType.INT8],
        [IRType.INT16],
        [IRType.INT32],
        [IRType.FLOAT64]
    ], 
    [IRType.POINTER], 
    [
        IRType.BOXED,
        IRType.POINTER,
        IRType.INT8,
        IRType.INT16,
        IRType.INT32,
        IRType.FLOAT64
    ]
);

/**
@class Instruction to store a value to memory
@augments IRInstr
*/
var StoreInstr = TypedInstrMaker(
    'store',
    [
        [IRType.BOXED],
        [IRType.POINTER],
        [IRType.INT8],
        [IRType.INT16],
        [IRType.INT32],
        [IRType.FLOAT64]
    ],
    [
        [IRType.POINTER, IRType.BOXED],
        [IRType.POINTER, IRType.POINTER],
        [IRType.POINTER, IRType.INT8],
        [IRType.POINTER, IRType.INT16],
        [IRType.POINTER, IRType.INT32],
        [IRType.POINTER, IRType.FLOAT64]
    ],
    IRType.VOID
);

//====================================================
// Type conversion instructions
//====================================================

/**
@class Instruction to unbox a value
@augments IRInstr
*/
var UnboxInstr = TypedInstrMaker(
    'unbox', 
    [
        [IRType.POINTER],
        [IRType.INT32]
    ],
    [IRType.BOXED], 
    [
        IRType.POINTER,
        IRType.INT32
    ]
);

/**
@class Instruction to box a value
@augments IRInstr
*/
var BoxInstr = TypedInstrMaker(
    'box', 
    [
        [IRType.POINTER],
        [IRType.INT32]
    ],
    [
        [IRType.POINTER],
        [IRType.INT32]
    ], 
    IRType.BOXED
);

/**
@class Instruction to evaluate the boolean value of a boxed value
@augments IRInstr
*/
var ToBoolInstr = TypedInstrMaker(
    'tobool',
    undefined,
    [
        [IRType.BOXED]
    ],
    IRType.INT8
);

/**
@class Instruction to convert between different integer types
@augments IRInstr
*/
var ICastInstr = TypedInstrMaker(
    'icast', 
    [
        [IRType.UINT16, IRType.INT16],
        [IRType.UINT32, IRType.INT32],
        [IRType.INT16, IRType.INT32],
        [IRType.INT32, IRType.INT16],
        [IRType.INT32, IRType.POINTER],
        [IRType.POINTER, IRType.INT32]
    ],
    [
        [IRType.UINT16],
        [IRType.UINT32],
        [IRType.INT16],
        [IRType.INT32],
        [IRType.INT32],
        [IRType.POINTER]
    ],
    [
        IRType.INT16,
        IRType.INT32,
        IRType.INT32,
        IRType.INT16,
        IRType.POINTER,
        IRType.INT32
    ]
);

/**
@class Instruction to convert integer values to floating-point
@augments IRInstr
*/
var IToFPInstr = TypedInstrMaker(
    'itof',
    [
        [IRType.INT32, IRType.FLOAT64],
    ],
    [IRType.INT32],
    IRType.FLOAT64
);

/**
@class Instruction to convert floating-point values to integer
@augments IRInstr
*/
var FPToIInstr = TypedInstrMaker(
    'ftoi',
    [
        [IRType.FLOAT64, IRType.INT32],
    ],
    [IRType.FLOAT64],
    IRType.INT32
);

//====================================================
// Arithmetic operations w/o overflow handling
//====================================================

/**
@class Instruction to add integer values without overflow handling
@augments IRInstr
*/
var IAddInstr = TypedInstrMaker(
    'add',
    undefined,
    [
        [IRType.POINTER, IRType.INT32],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.POINTER,
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to subtract integer values without overflow handling
@augments IRInstr
*/
var ISubInstr = TypedInstrMaker(
    'sub',
    undefined,
    [
        [IRType.POINTER, IRType.INT32],
        [IRType.POINTER, IRType.POINTER],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.POINTER,
        IRType.INT32,
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to multiply integer values without overflow handling
@augments IRInstr
*/
var IMulInstr = TypedInstrMaker(
    'mul',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to divide integer values
@augments IRInstr
*/
var IDivInstr = TypedInstrMaker(
    'div',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to compute the modulo of integer values
@augments IRInstr
*/
var IModInstr = TypedInstrMaker(
    'mod',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to add float64 values
@augments IRInstr
*/
var FAddInstr = TypedInstrMaker(
    'add',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64],
    ],
    [
        IRType.FLOAT64
    ]
);

/**
@class Instruction to subtract float64 values
@augments IRInstr
*/
var FSubInstr = TypedInstrMaker(
    'sub',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64],
    ],
    [
        IRType.FLOAT64
    ]

);

/**
@class Instruction to multiply float64 values
@augments IRInstr
*/
var FMulInstr = TypedInstrMaker(
    'mul',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64],
    ],
    [
        IRType.FLOAT64
    ]
);

/**
@class Instruction to divide float64 values
@augments IRInstr
*/
var FDivInstr = TypedInstrMaker(
    'div',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64],
    ],
    [
        IRType.FLOAT64
    ]
);

//====================================================
// Bitwise integer operations
//====================================================

/**
@class Instruction to compute the bitwise NOT of integer values
@augments IRInstr
*/
var INotInstr = TypedInstrMaker(
    'not',
    undefined,
    [
        [IRType.UINT16],
        [IRType.UINT32],
        [IRType.INT16],
        [IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to compute the bitwise AND of integer values
@augments IRInstr
*/
var IAndInstr = TypedInstrMaker(
    'and',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to compute the bitwise OR of integer values
@augments IRInstr
*/
var IOrInstr = TypedInstrMaker(
    'or',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to compute the bitwise XOR of integer values
@augments IRInstr
*/
var IXorInstr = TypedInstrMaker(
    'xor',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to compute the left shift of integer values
@augments IRInstr
*/
var ILsftInstr = TypedInstrMaker(
    'lsft',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to compute the right shift of integer values
@augments IRInstr
*/
var IRsftInstr = TypedInstrMaker(
    'rsft',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

/**
@class Instruction to compute the unsigned right shift of integer values
@augments IRInstr
*/
var IUrsftInstr = TypedInstrMaker(
    'ursft',
    undefined,
    [
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.UINT16,
        IRType.UINT32,
        IRType.INT16,
        IRType.INT32
    ]
);

//====================================================
// Comparison instructions
//====================================================

/**
@class Instruction to perform a less-than comparison on integer values
@augments IRInstr
*/
var ILtInstr = TypedInstrMaker(
    'ilt',
    undefined,
    [
        [IRType.POINTER, IRType.POINTER],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    IRType.INT8
);

/**
@class Instruction to perform a less-than-or-equal comparison on integer values
@augments IRInstr
*/
var ILteInstr = TypedInstrMaker(
    'ilte',
    undefined,
    [
        [IRType.POINTER, IRType.POINTER],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    IRType.INT8
);

/**
@class Instruction to perform a greater-than comparison on integer values
@augments IRInstr
*/
var IGtInstr = TypedInstrMaker(
    'igt',
    undefined,
    [
        [IRType.POINTER, IRType.POINTER],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    IRType.INT8
);

/**
@class Instruction to perform a greater-than-or-equal comparison on integer values
@augments IRInstr
*/
var IGteInstr = TypedInstrMaker(
    'igte',
    undefined,
    [
        [IRType.POINTER, IRType.POINTER],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    IRType.INT8
);

/**
@class Instruction to perform an equality comparison on integer values
@augments IRInstr
*/
var IEqInstr = TypedInstrMaker(
    'ieq',
    undefined,
    [
        [IRType.POINTER, IRType.POINTER],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    IRType.INT8
);

/**
@class Instruction to perform an inequality comparison on integer values
@augments IRInstr
*/
var INeqInstr = TypedInstrMaker(
    'ineq',
    undefined,
    [
        [IRType.POINTER, IRType.POINTER],
        [IRType.UINT16, IRType.UINT16],
        [IRType.UINT32, IRType.UINT32],
        [IRType.INT16, IRType.INT16],
        [IRType.INT32, IRType.INT32]
    ],
    IRType.INT8
);

/**
@class Instruction to perform a less-than comparison on floating-point values
@augments IRInstr
*/
var FLtInstr = TypedInstrMaker(
    'flt',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64]
    ],
    IRType.INT8
);

/**
@class Instruction to perform a less-than-or-equal comparison on floating-point values
@augments IRInstr
*/
var FLteInstr = TypedInstrMaker(
    'flte',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64]
    ],
    IRType.INT8
);

/**
@class Instruction to perform a greater-than comparison on floating-point values
@augments IRInstr
*/
var FGtInstr = TypedInstrMaker(
    'fgt',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64]
    ],
    IRType.INT8
);

/**
@class Instruction to perform a greater-than-or-equal comparison on floating-point values
@augments IRInstr
*/
var FGteInstr = TypedInstrMaker(
    'fgte',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64]
    ],
    IRType.INT8
);

/**
@class Instruction to perform an equality comparison on floating-point values
@augments IRInstr
*/
var FEqInstr = TypedInstrMaker(
    'feq',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64]
    ],
    IRType.INT8
);

/**
@class Instruction to perform an inequality comparison on floating-point values
@augments IRInstr
*/
var FNeqInstr = TypedInstrMaker(
    'fneq',
    undefined,
    [
        [IRType.FLOAT64, IRType.FLOAT64]
    ],
    IRType.INT8
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
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.INT32
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
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.INT32
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
        [IRType.INT32, IRType.INT32]
    ],
    [
        IRType.INT32
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

