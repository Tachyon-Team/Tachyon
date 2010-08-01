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
Function to generate typed instruction constructors using closures
@param mnemonic mnemonic name of the instruction
@param inTypeSpecs list of arrays of input value type
@param outTypeSpecs list of output value types
@param protoObj prototype object for the instruction
*/
function TypedInstrMaker(
    mnemonic,
    inTypeSpecs,
    outTypeSpecs,
    protoObj
)
{
    // Ensure that the input type specifications are valid
    assert (
        inTypeSpecs instanceof Array,
        'invalid input type specifications specified'
    );

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

    // Create lists for the mnemonic instruction names
    var mnemonicNames = [];

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
            var type = typeSpec[j];
    
            // Ensure that the type is valid
            assert (
                type instanceof IRTypeObj,
                'invalid type in type specification'
            );

            if (type !== firstType)            
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

    // For each output type specification
    for (var i = 0; i < outTypeSpecs.length; ++i)
    {
        var type = outTypeSpecs[i];

        // Ensure that the type is valid
        assert (
            type instanceof IRTypeObj,
            'invalid type in type specification'
        );
    }

    /**
    Instruction constructor function instance, implemented as a closure
    */
    function InstrConstr(inputs)
    {
        // Put the arguments into an array
        if (inputs instanceof Array)
        {
            var inputValues = inputs;
        }
        else
        {
            var inputValues = [];
            for (var i = 0; i < arguments.length; ++i)
                inputValues.push(arguments[i]);
        }

        // Find the type specification from the input value types
        var specIndex = -1;
        SPEC_LOOP:
        for (var i = 0; i < inTypeSpecs.length; ++i)
        {
            var inTypeSpec = inTypeSpecs[i];

            // If the number of input values does not match the spec length, continue
            if (inputValues.length != typeSpec.length)
                continue SPEC_LOOP;

            // For each input value
            for (var j = 0; j < inputValues.length; ++j)
            {
                var inputVal = inputValues[j];

                // Ensure that this input value is valid
                assert (
                    inputVal instanceof IRValue,
                    'argument ' + j + ' to "' + mnemonic + 
                    '" instruction constructor is not a valid IR value'
                );

                // If the input value type does not match that of the spec, continue
                if (inputVal.type !== inTypeSpec[j])
                    continue SPEC_LOOP;
            }

            // The spec was found, break otu of the loop
            specIndex = i;
            break;
        }
        assert (
            specIndex != -1,
            'input values to ' + mnemonic + ' constructor do not match any ' +
            'type specification'
        );

        // Set the mnemonic name of the instruction
        this.mnemonic = mnemonicNames[specIndex];

        // Set the output type for the instruction
        this.type = outTypeSpecs[specIndex];

        // Copy the uses of the instruction      
        this.uses = inputValues.slice(0);
    }
    
    // If no prototype object was specified, create an IRInstr instance
    if (!protoObj)
        protoObj = new IRInstr();

    // Set the constructor for the new instruction
    InstrConstr.prototype = protoObj;

    /**
    Generic instruction shallow copy function
    */
    InstrConstr.prototype.copy = function ()
    {
        // Return a new instruction with the same uses
        return this.baseCopy(new InstrConstr(this.uses.slice(0)));
    };

    // Return the new constructor instance
    return InstrConstr;
}

/**
Function to generate type-parameterized instruction constructors using closures
@param mnemonic mnemonic name of the instruction
@param typeSpecs array of valid type parameter tuples
@param inTypes array of input value types
@param outType output value type
@param protoObj prototype object for the instruction
*/
function TypeParamInstrMaker(
    mnemonic,
    typeSpecs,
    inTypes,
    outType,
    protoObj
)
{
    // If no type specifications were specified, add an empty one
    if (typeSpecs.length == 0)
        typeSpecs = [[]];

    // Get the number of type parameters
    var numTypeParams = typeSpecs[0].length;

    // Create lists for the input and output type specifications
    var inTypeSpecs = [];
    var outTypeSpecs = [];

    // Create lists for the mnemonic instruction names
    var mnemonicNames = [];

    // For each type parameter specification
    for (var i = 0; i < typeSpecs.length; ++i)
    {
        // Get this type parameter specification
        var typeSpec = typeSpecs[i];

        // Ensure all specifications have the same length
        assert (
            typeSpec.length == numTypeParams,
            'invalid type parameter specification'
        );

        // Build the input type array in function of the type parameters
        var inTypeSpec = [];
        for (var j = 0; j < inTypes.length; ++j)
        {
            var type = inTypes[j];

            // Ensure that type parameter indices are valid
            assert (
                typeof type != 'number' || type < numTypeParams,
                'invalid type parameter index in input types'
            );

            if (typeof type == 'number')
                type = typeSpec[type];

            inTypeSpec.push(type);
        }

        // Ensure that type parameter indices are valid
        assert (
           typeof outType != 'number' || outType < numTypeParams,
           'invalid type parameter index in output types'
        );

        // Set the output type in function of the type parameters
        var outTypeSpec = (typeof outType == 'number')? typeSpec[outType]:outType;

        // Add the input and output type specifications to the lists
        inTypeSpecs.push(inTypeSpec);
        outTypeSpecs.push(outTypeSpec);

        // Set the mnemonic name for this type parameter specification
        var mnemName = mnemonic;
        for (var j = 0; j < numTypeParams; ++j)
        {
            var type = typeSpec[j];
            if (type !== IRType.BOXED || numTypeParams > 0)
                mnemName += '_' + type.name;
        }

        // Add the mnemonic name to the list
        mnemonicNames.push(mnemName);
    }

    /**
    Instruction constructor function instance, implemented as a closure
    */
    function InstrConstr(inputs)
    {
        // Put the arguments into an array
        if (inputs instanceof Array)
        {
            var typeParams = inputs.slice(0, numTypeParams);
            var inputValues = inputs.slice(numTypeParams);
        }
        else
        {

            var typeParams = [];
            var inputValues = [];
            for (var i = 0; i < numTypeParams; ++i)
                typeParams.push(arguments[i]);
            for (var i = numTypeParams; i < arguments.length; ++i)
                inputValues.push(arguments[i]);
        }

        // If no type parameters were specified
        if (typeParams.length == 0)
        {
            // Build a type array from the input value types
            for (var i = 0; i < inputValues.length; ++i)
                typeParams.push(inputValues[i].type);
        }

        // Ensure that the argument count is valid
        assert (
            typeParams.length == numTypeParams ||
            inputValues.length == inTypes.length,
            'invalid arguments to "' + mnemonic + 
            '" instruction constructor'
        );

        // Find the type specification from the type parameters
        var specIndex = -1;
        SPEC_LOOP:
        for (var i = 0; i < typeSpecs.length; ++i)
        {
            var typeSpec = typeSpecs[i];

            for (var j = 0; j < typeParams.length; ++j)
                if (typeSpec[j] !== typeParams[j])
                    continue SPEC_LOOP;

            specIndex = i;
            break;
        }
        assert (
            specIndex != -1,
            'type parameters to ' + mnemonic + ' constructor do not match any ' +
            'type specification'
        );

        // Get the input types for the instruction
        var inTypes = inTypeSpecs[specIndex].slice(0);

        // Set the mnemonic name of the instruction
        this.mnemonic = mnemonicNames[specIndex];

        // Ensure that each input value is valid
        for (var i = 0; i < inputValues.length; ++i)
        {
            assert (
                inputValues[i] instanceof IRValue,
                'argument ' + i + ' to "' + this.mnemonic + 
                '" instruction constructor is not a valid IR value'
            );       

            assert (
                inputValues[i].type === inTypes[i],
                'argument ' + i + ' to "' + this.mnemonic + 
                '" instruction constructor does not have valid output type ' +
                '(' + inputValues[i].type.name + ')'
            );
        }

        // Set the output type for the instruction
        this.type = outTypeSpecs[specIndex];

        // Copy the uses of the instruction      
        this.uses = inputValues.slice(0);

        // Copy the type parameters of the instruction
        this.typeParams = typeParams.slice(0);
    }
    
    // If no prototype object was specified, create an IRInstr instance
    if (!protoObj)
        protoObj = new IRInstr();

    // Set the constructor for the new instruction
    InstrConstr.prototype = protoObj;

    /**
    Generic instruction shallow copy function
    */
    InstrConstr.prototype.copy = function ()
    {
        // Return a new instruction with the same uses
        return this.baseCopy(
            new InstrConstr(
                this.typeParams.slice(0).concat(this.uses.slice(0))
            )
        );
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
function GenericInstrMaker(mnemonic, numInputs, protoObj)
{
    var inTypes = [];
    for (var i = 0; i < numInputs; ++i)
        inTypes.push(IRType.BOXED);

    return TypedInstrMaker(
        mnemonic,
        [inTypes],
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
    return this.type.name + ' ' + this.outName + ' = ' + this.mnemonic;
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
            (this.targetNames.length? (' ' + this.targetNames[i]):'') + 
            ' ' + this.targets[i].getBlockName()
        ;
    }

    return output;
}

/**
@class Unconditional jump instruction
@augments BranchInstr
*/
function JumpInstr(targetBlock)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = 'jump'

    /**
    Target basic block
    @field
    */
    this.targets = [targetBlock];
}
JumpInstr.prototype = new BranchInstr();

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
    // Set the mnemonic name for this instruction
    this.mnemonic = 'if';

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
Branch target name for the if instruction
@field
*/
IfInstr.prototype.targetNames = ['then', 'else'];

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
var RetInstr = GenericInstrMaker(
    'ret',
     1,
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

//====================================================
// Memory access instructions
//====================================================

/**
@class Instruction to load a value from memory
@augments IRInstr
*/
var LoadInstr = TypeParamInstrMaker(
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
    0
);

/**
@class Instruction to store a value to memory
@augments IRInstr
*/
var StoreInstr = TypeParamInstrMaker(
    'store',
    [
        [IRType.BOXED],
        [IRType.POINTER],
        [IRType.INT8],
        [IRType.INT16],
        [IRType.INT32],
        [IRType.FLOAT64]
    ], 
    [IRType.POINTER, 0], 
    IRType.VOID
);

//====================================================
// Type conversion instructions
//====================================================

/**
@class Instruction to unbox a value
@augments IRInstr
*/
var UnboxInstr = TypeParamInstrMaker(
    'unbox', 
    [
        [IRType.POINTER],
        [IRType.INT32]
    ],
    [IRType.BOXED], 
    0
);

/**
@class Instruction to box a value
@augments IRInstr
*/
var BoxInstr = TypeParamInstrMaker(
    'box', 
    [
        [IRType.POINTER],
        [IRType.INT32]
    ],
    [0], 
    IRType.BOXED
);

/**
@class Instruction to convert between different integer types
@augments IRInstr
*/
var ICastInstr = TypeParamInstrMaker(
    'icast', 
    [
        [IRType.UINT16, IRType.INT16],
        [IRType.UINT32, IRType.INT32],
        [IRType.INT16, IRType.INT32],
        [IRType.INT32, IRType.INT16],
        [IRType.INT32, IRType.POINTER],
        [IRType.POINTER, IRType.INT32]
    ],
    [0],
    1
);

/**
@class Instruction to convert integer values to floating-point
@augments IRInstr
*/
var IToFPInstr = TypeParamInstrMaker(
    'itof', 
    [],
    [IRType.INT32],
    IRType.FLOAT64
);

/**
@class Instruction to convert floating-point values to integer
@augments IRInstr
*/
var FPToIInstr = TypeParamInstrMaker(
    'ftoi', 
    [],
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
    [
        [IRType.FLOAT64, IRType.FLOAT64]
    ],
    IRType.INT8
);

//====================================================
// Branch instructions
//====================================================

/**
Function to generate typed branch instruction constructors using closures
@param mnemonic mnemonic name of the instruction
@param inTypeSpecs list of arrays of input value type
@param outTypeSpecs list of output value types
@param branchNames list of branch target names
@param protoObj prototype object for the instruction
*/
function TypedBranchInstrMaker(
    mnemonic,
    inTypeSpecs,
    outTypeSpecs,
    branchNames,
    protoObj
)
{
    // Ensure that the input type specifications are valid
    assert (
        inTypeSpecs instanceof Array,
        'invalid input type specifications specified'
    );

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

    // Create lists for the mnemonic instruction names
    var mnemonicNames = [];

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
            var type = typeSpec[j];
    
            // Ensure that the type is valid
            assert (
                type instanceof IRTypeObj,
                'invalid type in input type specification'
            );

            if (type !== firstType)            
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

    // For each output type specification
    for (var i = 0; i < outTypeSpecs.length; ++i)
    {
        var type = outTypeSpecs[i];

        // Ensure that the type is valid
        assert (
            type instanceof IRTypeObj,
            'invalid type in output type specification'
        );
    }

    // Get the number of branches
    var numBranches = branchNames.length;

    /**
    Instruction constructor function instance, implemented as a closure
    */
    function InstrConstr(inputs)
    {
        // Put the arguments into an array
        if (inputs instanceof Array)
        {
            var inputValues = inputs.slice(0, inputs.length - numBranches);
            var targets = inputs.slice(inputs.length - numBranches, inputs.length);
        }
        else
        {
            var inputValues = [];
            for (var i = 0; i < arguments.length - numBranches; ++i)
                inputValues.push(arguments[i]);
            var targets = [];
            for (var i = arguments.length - numBranches; i < arguments.length; ++i)
                targets.push(arguments[i]);
        }

        // Ensure that the right number of branch targets were specified
        assert (
            targets.length == numBranches,
            'must specify ' + numBranches + ' branch targets to ' + mnemonic + ' constructor'
        );

        // Ensure that the branch targets are valid
        targets.forEach(
            function (t)
            {
                assert (
                    t instanceof BasicBlock,
                    'invalid branch target passed to ' + mnemonic + ' constructor'
                );
            }
        );

        // Find the type specification from the input value types
        var specIndex = -1;
        SPEC_LOOP:
        for (var i = 0; i < inTypeSpecs.length; ++i)
        {
            var inTypeSpec = inTypeSpecs[i];

            // If the number of input values does not match the spec length, continue
            if (inputValues.length != typeSpec.length)
                continue SPEC_LOOP;

            // For each input value
            for (var j = 0; j < inputValues.length; ++j)
            {
                var inputVal = inputValues[j];

                // Ensure that this input value is valid
                assert (
                    inputVal instanceof IRValue,
                    'argument ' + j + ' to "' + mnemonic + 
                    '" instruction constructor is not a valid IR value'
                );

                // If the input value type does not match that of the spec, continue
                if (inputVal.type !== inTypeSpec[j])
                    continue SPEC_LOOP;
            }

            // The spec was found, break otu of the loop
            specIndex = i;
            break;
        }
        assert (
            specIndex != -1,
            'input values to ' + mnemonic + ' constructor do not match any ' +
            'type specification'
        );

        // Set the mnemonic name of the instruction
        this.mnemonic = mnemonicNames[specIndex];

        // Set the output type for the instruction
        this.type = outTypeSpecs[specIndex];

        // Store the uses of the instruction      
        this.uses = inputValues.slice(0);

        // Store the branch targets
        this.targets = targets;
    }

    // If no prototype object was specified, create a BranchInstr instance
    if (!protoObj)
        protoObj = new BranchInstr();

    // Set the constructor for the new instruction
    InstrConstr.prototype = protoObj;

    // Store the branch target names
    InstrConstr.prototype.targetNames = branchNames;

    /**
    Generic instruction shallow copy function
    */
    InstrConstr.prototype.copy = function ()
    {
        // Return a new instruction with the same uses and targets
        return this.baseCopy(
            new InstrConstr(
                this.uses.slice(0).concat(this.targets.slice(0))
            )
        );
    };

    // Return the new constructor instance
    return InstrConstr;
}

/**
@class Specialized if instruction taking only booleans as input
@augments BranchInstr
*/
var IfBoolInstr = TypedBranchInstrMaker(
    'if',
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
var IAddOvfInstr = TypedBranchInstrMaker(
    'add_ovf',
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
var ISubOvfInstr = TypedBranchInstrMaker(
    'sub_ovf',
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
var IMulOvfInstr = TypedBranchInstrMaker(
    'mul_ovf',
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










