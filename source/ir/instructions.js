/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Class hierarchy for Intermediate Representation (IR) instructions

@author
Maxime Chevalier-Boisvert
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
    var output = "";
    var ins = instr.uses[pos];

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
    if (outFormatFn === undefined)
        outFormatFn = IRInstr.defOutFormat;
    if (inFormatFn === undefined)
        inFormatFn = IRInstr.defInFormat;

    // Create a string for the output
    var output = "";

    // If this instruction has a non-void output print its output name
    if (this.type !== IRType.none)
        output += outFormatFn(this) + ' = ';

    output += this.mnemonic;

    // For each use
    for (i = 0; i < this.uses.length; ++i)
    {
        output += " " + inFormatFn(this, i);

        if (i !== this.uses.length - 1)
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
    if (this.outName !== "") // FIXME
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
    assert (
        oldUse.type === newUse.type,
        'cannot replace use by one of a different type'
    );

    for (var i = 0; i < this.uses.length; ++i)
    {
        if (this.uses[i] === oldUse)
        {
            this.uses[i] = newUse;
        }
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

    if (this.dests.length === 0)
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
        values.length === preds.length,
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
    if (outFormatFn === undefined)
        outFormatFn = IRInstr.defOutFormat;
    if (inFormatFn === undefined)
        inFormatFn = IRInstr.defInFormat;

    var phiInFormatFn = function (instr, pos)
    {
        var pred = instr.preds[pos];

        return '[' + inFormatFn(instr, pos) + ' ' + pred.getBlockName() + ']';
    };

    var output = "";

    // If this instruction's type is not void, print its output name
    if (this.type !== IRType.none)
        output += outFormatFn(this) + ' = ';

    output += this.mnemonic + ' ';

    for (var i = 0; i < this.uses.length; ++i)
    {
        output += phiInFormatFn(this, i);

        if (i !== this.uses.length - 1)
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
    if (this.uses.length !== 0)
    {
        assert (
            value.type === this.type,
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
Get the predecessor block for a given value 
*/
PhiInstr.prototype.getPredecessor = function (value)
{
    for (var i = 0; i < this.uses.length; ++i)
    {
        if (this.uses[i] === value)
            return this.preds[i];
    }

    assert (
        false,
        'cannot get predecessor for value, invalid value'
    );        
};

/**
Make a shallow copy of the instruction
*/
PhiInstr.prototype.copy = function ()
{
    /*
    print('Copying: ' + this);

    for (var i = 0; i < this.uses.length; ++i)
    {
        print(this.uses[i].type);
    }
    */

    return this.baseCopy(new PhiInstr(this.uses.slice(0), this.preds.slice(0)));
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

        if (curIndex !== argArray.length)
            error('invalid arguments passed to ' + mnemonic + ' constructor');
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
        catch (errorVal)
        {
            rethrowError(
                errorVal,
                'Invalid arguments to "' + mnemonic + '" instruction'
            );
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
    if (minExpected === maxExpected)
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
ArithInstr.initFunc = function (typeParams, inputVals, branchTargets)
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
Initialization function for arithmetic instructions which can interfere
with tag bits, and thus should not operate on tagged values.
*/
ArithInstr.initFuncUntag = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        inputVals[0].type.isNumber() &&
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
            (inputVals[0].type === IRType.rptr &&
             inputVals[1].type === IRType.puint)
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
             inputVals[1].type === IRType.puint)
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
        
        if (inputVals[0].type === IRType.rptr && 
            inputVals[1].type === IRType.rptr)
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
    ArithInstr.initFuncUntag,
    undefined,
    new ArithInstr()
);

/**
@class Division instruction
@augments ArithInstr
*/
var DivInstr = instrMaker(
    'div',
    ArithInstr.initFuncUntag,
    undefined,
    new ArithInstr()
);

/**
@class Modulo instruction
@augments ArithInstr
*/
var ModInstr = instrMaker(
    'mod',
    ArithInstr.initFunc,
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
ArithOvfInstr.initFunc = function (typeParams, inputVals, branchTargets)
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
Initialization function for arithmetic instructions w/ overflow which can
interfere with tag bits, and thus should only operate on integer values.
*/
ArithOvfInstr.initFuncUntag = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);
    assert (
        (inputVals[0].type === IRType.pint &&
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
    ArithOvfInstr.initFunc,
    ['normal', 'overflow'],
    new ArithOvfInstr()
);

/**
@class Instruction to subtract integer values with overflow handling
@augments ArithOvfInstr
*/
var SubOvfInstr = instrMaker(
    'sub_ovf',
    ArithOvfInstr.initFunc,
    ['normal', 'overflow'],
    new ArithOvfInstr()
);

/**
@class Instruction to multiply integer values with overflow handling
@augments ArithOvfInstr
*/
var MulOvfInstr = instrMaker(
    'mul_ovf',
    ArithOvfInstr.initFuncUntag,
    ['normal', 'overflow'],
    new ArithOvfInstr()
);

/**
@class Instruction to left-shift integer values with overflow handling
@augments ArithOvfInstr
*/
var LsftOvfInstr = instrMaker(
    'lsft_ovf',
    ArithOvfInstr.initFuncUntag,
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
BitOpInstr.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        (
            inputVals[0].type.isPtr()
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
Initialization function for bitwise operation instructions that can only
operate on integer values.
*/
BitOpInstr.initFuncUntag = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        (inputVals[0].type.isInt() &&
         inputVals[1].type === inputVals[0].type),
        'invalid input types'
    );
    
    this.type = inputVals[0].type;
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
    BitOpInstr.initFunc,
    undefined,
    new BitOpInstr()
);

/**
@class Bitwise OR instruction
@augments BitOpInstr
*/
var OrInstr = instrMaker(
    'or',
    BitOpInstr.initFunc,
    undefined,
    new BitOpInstr()
);

/**
@class Bitwise XOR instruction
@augments BitOpInstr
*/
var XorInstr = instrMaker(
    'xor',
    BitOpInstr.initFunc,
    undefined,
    new BitOpInstr()
);

/**
@class Left shift instruction
@augments BitOpInstr
*/
var LsftInstr = instrMaker(
    'lsft',
    BitOpInstr.initFuncUntag,
    undefined,
    new BitOpInstr()
);

/**
@class Right shift instruction
@augments BitOpInstr
*/
var RsftInstr = instrMaker(
    'rsft',
    BitOpInstr.initFuncUntag,
    undefined,
    new BitOpInstr()
);

/**
@class Unsigned right shift instruction
@augments BitOpInstr
*/
var UrsftInstr = instrMaker(
    'ursft',
    BitOpInstr.initFuncUntag,
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
CompInstr.initFunc = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        (inputVals[0].type === IRType.box  ||
         inputVals[0].type === IRType.rptr ||
         inputVals[0].type.isNumber())
        &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types (' + inputVals[0].type + 
        ', ' + inputVals[1].type + ')'
    );
    
    this.type = IRType.bool;
};

/**
Initialization function for equality and inequality comparison instructions
*/
CompInstr.initFuncEq = function (typeParams, inputVals, branchTargets)
{
    instrMaker.validNumInputs(inputVals, 2, 2);

    assert (
        (inputVals[0].type === IRType.box   ||
         inputVals[0].type === IRType.rptr  ||
         inputVals[0].type.isNumber()       ||
         inputVals[0].type === IRType.bool)
        &&
        inputVals[1].type === inputVals[0].type,
        'invalid input types (' + inputVals[0].type + 
        ', ' + inputVals[1].type + ')'
    );
    
    this.type = IRType.bool;
};

/**
@class Less-than comparison instruction
@augments CompInstr
*/
var LtInstr = instrMaker(
    'lt',
    CompInstr.initFunc,
    undefined,
    new CompInstr()
);

/**
@class Less-than-or-equal comparison instruction
@augments CompInstr
*/
var LeInstr = instrMaker(
    'le',
    CompInstr.initFunc,
    undefined,
    new CompInstr()
);

/**
@class Greater-than comparison instruction
@augments CompInstr
*/
var GtInstr = instrMaker(
    'gt',
    CompInstr.initFunc,
    undefined,
    new CompInstr()
);

/**
@class Greater-than-or-equal comparison instruction
@augments CompInstr
*/
var GeInstr = instrMaker(
    'ge',
    CompInstr.initFunc,
    undefined,
    new CompInstr()
);

/**
@class Equality comparison instruction
@augments CompInstr
*/
var EqInstr = instrMaker(
    'eq',
    CompInstr.initFuncEq,
    undefined,
    new CompInstr()
);

/**
@class Inequality comparison instruction
@augments CompInstr
*/
var NeInstr = instrMaker(
    'ne',
    CompInstr.initFuncEq,
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
        // Can either return nothing or one value
        instrMaker.validNumInputs(inputVals, 0, 1);
        
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

/**
@class If branching instruction
@augments IRInstr
*/
function IfTestInstr(input1, input2, test, trueTarget, falseTarget)
{
    assert (
        input1.type === input2.type &&
        input1.type !== IRType.box,
        'input types must match'
    );

    assert (
        test === IfTestInstr.test.EQ ||
        test === IfTestInstr.test.LT ||
        test === IfTestInstr.test.LE,
        'invalid test condition'
    );

    assert (
        trueTarget instanceof BasicBlock &&
        falseTarget instanceof BasicBlock,
        'invalid branch targets'
    );

    this.mnemonic = 'if';

    this.uses = [input1, input2];

    this.test = test;

    this.targets = [trueTarget, falseTarget];

    this.type = IRType.none;
}
IfTestInstr.prototype = new IRInstr();

/**
Possible tests for the if instruction
*/
IfTestInstr.test = {
    EQ  : 1,
    LT  : 2,
    LE  : 3
};

/**
Branch target names
*/
IfTestInstr.prototype.targetNames = ['then', 'else'];

/**
Produce a string representation of the if instruction
*/
IfTestInstr.prototype.toString = function (outFormatFn, inFormatFn)
{
    // If no formatting functions were specified, use the default ones
    if (outFormatFn === undefined)
        outFormatFn = IRInstr.defOutFormat;
    if (inFormatFn === undefined)
        inFormatFn = IRInstr.defInFormat;

    // Create a string for the output
    var output = this.mnemonic;

    // Print the comparison
    output += ' ' + inFormatFn(this, 0) + ' ';
    switch (this.test)
    {
        case IfTestInstr.test.EQ: output += '==='; break;
        case IfTestInstr.test.LT: output += '<';   break;
        case IfTestInstr.test.LE: output += '<=';  break;
    }
    output += ' ' + inFormatFn(this, 1) + ' ';

    // Print the branch targets
    output += this.targetNames[0] + ' ';
    output += this.targets[0].getBlockName() + ' ';
    output += this.targetNames[1] + ' ';
    output += this.targets[1].getBlockName();

    return output;
}

/**
Make a shallow copy of the instruction
*/
IfTestInstr.prototype.copy = function ()
{
    return this.baseCopy(
        new IfTestInstr(
            this.uses[0],
            this.uses[1],
            this.test,
            this.targets[0],
            this.targets[1]
        )
    );
};

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
    return this.targets[0] ? this.targets[0] : null;
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
ThrowInstr.prototype.isBranch = function () { return true; };

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

        // 3 base arguments required for any call
        const NUM_BASE_ARGS = 3;

        // Ensure that we have all the base arguments
        instrMaker.validNumInputs(inputVals, NUM_BASE_ARGS);
        
        // Get references to the base arguments
        var funcPtr = inputVals[0];
        var funcObj = inputVals[1];
        var thisVal = inputVals[2];

        // Ensure that the base argument types are valid
        instrMaker.validType(funcPtr, IRType.rptr);
        instrMaker.validType(funcObj, IRType.box);
        instrMaker.validType(thisVal, IRType.box);

        // 0 to 2 branch targets
        instrMaker.validNumBranches(branchTargets, 0, 2);

        // Get a reference to the function object value
        var funcObj = inputVals[1];

        // If this is a static vall
        if (funcPtr instanceof IRFunction)
        {
            var numArgs = inputVals.length - NUM_BASE_ARGS;

            assert (
                numArgs === funcPtr.getNumArgs(),
                'direct calls do not support variable argument counts, got ' +
                numArgs + ' arguments, expected ' + funcPtr.getNumArgs() +
                ' (' + funcPtr.funcName + ')'
            );

            for (var i = NUM_BASE_ARGS; i < inputVals.length; ++i)
            {
                 assert (
                    inputVals[i].type === funcPtr.argTypes[i-NUM_BASE_ARGS],
                    'argument type does not match (' + 
                    funcPtr.argVars[i-NUM_BASE_ARGS].toString() + ' = ' +
                    inputVals[i].type + ') for ' + funcPtr.funcName
                );
            }

            this.type = funcPtr.retType;
        }
        else
        {
            for (var i = NUM_BASE_ARGS; i < inputVals.length; ++i)
            {
                assert (
                    inputVals[i].type === IRType.box,
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
Get the function callee, if known
*/
CallFuncInstr.prototype.getCallee = function ()
{
    if (this.uses[0] instanceof IRFunction)
        return this.uses[0];
    else
        return undefined;
}

/**
Get the this argument
*/
CallFuncInstr.prototype.getThisArg = function ()
{
    return this.uses[2];
}

/**
Get an argument by number
*/
CallFuncInstr.prototype.getArg = function (argIdx)
{
    assert (
        argIdx < this.uses.length + 3,
        'invalid argument index'
    );

    return this.uses[3 + argIdx];
}

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

        // 3 base arguments required
        const NUM_BASE_ARGS = 3;

        instrMaker.validNumInputs(inputVals, NUM_BASE_ARGS);
        instrMaker.validType(inputVals[0], IRType.rptr);
        instrMaker.validType(inputVals[1], IRType.box);
        instrMaker.validType(inputVals[2], IRType.box);
        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.box;

        for (var i = NUM_BASE_ARGS; i < inputVals.length; ++i)
        {
            assert (
                inputVals[i].type === IRType.box,
                'constructor calls can only take boxed values as input'
            );
        }
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
@class Constructor call with function object reference
@augments CallApplyInstr
*/
var CallApplyInstr = instrMaker(
    'call_apply',
    function (typeParams, inputVals, branchTargets)
    {
        this.mnemonic = 'call_apply';

        // 5 arguments are required
        instrMaker.validNumInputs(inputVals, 5, 5);

        instrMaker.validType(inputVals[0], IRType.rptr);
        instrMaker.validType(inputVals[1], IRType.box);
        instrMaker.validType(inputVals[2], IRType.box);
        instrMaker.validType(inputVals[3], IRType.ref);
        instrMaker.validType(inputVals[4], IRType.pint);

        instrMaker.validNumBranches(branchTargets, 0, 2);
        
        this.type = IRType.box;
    },
    ['continue', 'throw'],
    new CallInstr()
);

/**
@class FFI function call instruction
@augments CallInstr
*/
var CallFFIInstr = instrMaker(
    'call_ffi',
    function (typeParams, inputVals, branchTargets)
    {
        this.mnemonic = 'call_ffi';

        instrMaker.validNumInputs(inputVals, 1);
        instrMaker.validType(inputVals[0], IRType.box);
        instrMaker.validNumBranches(branchTargets, 0, 0);

        assert (
            inputVals[0] instanceof CFunction,
            'FFI calls can only be made to static functions'
        );

        assert (
            inputVals.length - 1 === inputVals[0].argTypes.length,
            'incorrect number of arguments to FFI function'
        );

        for (var i = 1; i < inputVals.length; ++i)
        {
            assert (
                inputVals[i].type === inputVals[0].argTypes[i-1].cIRType,
                'argument type does not match (arg' + (i-1) + ' ' +
                inputVals[i].type + ', ' +
                inputVals[0].funcName + ')'
            );
        }

        this.type = inputVals[0].retType.cIRType;
    },
    new CallInstr()
);

/**
By default, conservatively assume all calls read and write to/from memory
*/
CallFFIInstr.prototype.writesMem = function () { return true; };
CallFFIInstr.prototype.readsMem = function () { return true; };

//=============================================================================
//
// Argument access instructions
//
//=============================================================================

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
@class Get the number of arguments passed to a function. This is used
       to build the arguments object.
@augments IRInstr
*/
var GetNumArgsInstr = instrMaker(
    'get_num_args',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 0, 0);
        
        this.type = IRType.pint;
    }
);

/**
@class Get the arguments table containing all arguments passed to a
       function. This is used to build the arguments object.
@augments IRInstr
*/
var GetArgTableInstr = instrMaker(
    'get_arg_table',
    function (typeParams, inputVals, branchTargets)
    {
        instrMaker.validNumInputs(inputVals, 0, 0);
        
        // The table is an array table object
        this.type = IRType.ref;
    }
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
             inputVals[0].type === IRType.bool ||
             inputVals[0].type === IRType.box ||
             inputVals[0].type === IRType.ref ||
             inputVals[0].type === IRType.rptr) 
            &&
            (typeParams[0].isInt() ||
             typeParams[0] === IRType.bool ||
             typeParams[0] === IRType.box ||
             typeParams[0] === IRType.ref ||
             typeParams[0] === IRType.rptr),
            'type parameters must be integer, boolean, boxed, reference or raw pointer'
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
        
        this.type = IRType.ref;
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
        instrMaker.validType(inputVals[0], IRType.ref);
        
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
function MoveInstr(from, to, interval)
{
    // Set the mnemonic name for this instruction
    this.mnemonic = "move";

    /**
    Inputs to the move instruction
    @field
    */
    this.uses = [from, to];

    /**
    Field used for register allocation
    */
    this.regAlloc = Object.create(this.regAlloc);

    /**
    Store the interval that has introduced this move
    */
    this.regAlloc.interval = (interval === undefined) ? null : interval;
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

        if (i !== this.uses.length - 1)
        {
            output += ", ";
        }
    }

    if (this.regAlloc.interval !== null && 
        this.regAlloc.interval.instr !== null)
    {
        output += " -- SSA Temp: " + this.regAlloc.interval.instr.getValName();
    }

    return output;
};

//=============================================================================
//
// High-level IR (HIR) instructions
//
//=============================================================================

/**
@class Base class for HIR instructions
@augments CallInstr
*/
var HIRInstr = function ()
{
};
HIRInstr.prototype = new CallInstr();

// TODO: PROBLEM: some HIR instructions may throw exceptions. Would need to
// descend from ExceptInstr?
//
// May want to alter design to eliminate ExceptInstr
// Could have a throws() function defined on all instructions
// by default, checks for second branch target
// Also define setThrowTarget for all?
//
// Other possibility is to get rid of HIRInstr, just check if lower
// function is defined on the instruction
// May be more logical. HIRInstr screws with current hierarchy

/**
Create an HIR instruction constructor
*/
function hirInstrMaker(instrName, numInputs, mayThrow, proto)
{
    var InstrCtor = instrMaker(
        instrName,
        function (typeParams, inputVals, branchTargets)
        {
            instrMaker.validNumInputs(inputVals, numInputs, numInputs);
            instrMaker.allValsBoxed(inputVals);

            if (mayThrow)
                instrMaker.validNumBranches(branchTargets, 0, 2);
            
            this.type = IRType.box;
        },
        mayThrow? ['continue', 'throw']:undefined,
        proto? proto:new HIRInstr()
    );

    return InstrCtor;
};

/**
@class Property read instruction
@augments HIRInstr
*/
var GetPropInstr = hirInstrMaker(
    'get_prop',
    2,
    true
);

/**
@class Property write instruction
@augments HIRInstr
*/
var PutPropInstr = hirInstrMaker(
    'put_prop',
    3,
    true
);

/**
@class Base class for HIR arithmetic instructions
@augments HIRInstr
*/
var HIRArithInstr = function ()
{
};
HIRArithInstr.prototype = new HIRInstr();

/**
@class Property write instruction
@augments HIRInstr
*/
var HIRAddInstr = hirInstrMaker(
    'hir_add',
    2,
    false,
    new HIRArithInstr()
);




//
// TODO: fill in other HIR instructions
//

//
// TODO: lowering function for HIR instructions, in lowering.js
//

// TODO: HIR load/store?









