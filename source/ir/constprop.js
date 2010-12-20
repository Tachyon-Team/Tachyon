/**
@fileOverview
Implementation of constant propagation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

//=============================================================================
//
// Implementation of Sparse Conditional Constant Propagation (SCCP)
//
//=============================================================================

// Constant for values not yet known
var TOP = 'TOP';

// Constant for value known to be non-constant
var BOT = 'BOT';

/**
Perform sparse conditional constant propagation on a CFG
*/
function constProp(cfg)
{
    // List of CFG blocks to be processed
    var cfgWorkList = [cfg.entry];

    // List of SSA edges to be processed
    var ssaWorkList = [];

    // Reachable blocks, indexed by block id
    var reachable = [];

    // Instruction values, indexed by instr id
    var instrVals = [];

    // Initialize all instruction values to top
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
        instrVals[itr.get().instrId] = TOP;

    // Until a fixed point is reached
    while (cfgWorkList.length > 0 || ssaWorkList.length > 0)
    {
        // Until the CFG work list is processed
        while (cfgWorkList.length > 0)
        {
            // Remove a block from the CFG work list
            var b = cfgWorkList.pop();

            // Test if this is the first visit to this block
            var firstVisit = !reachable[b.blockId];

            // Mark b as reachable
            reachable[b.blockId] = true;

            // For each instruction in b
            for (var i = 0; i < b.instrs.length; ++i)
            {
                var instr = b.instrs[i];

                // If this is not a phi node and this is not the first visit,
                // do not revisit non-phi instructions
                if (!(instr instanceof PhiInstr) && !firstVisit)
                    break;

                // Evaluate the instruction
                instrVals[instr.instrId] = evalInstr(instr);

                // For each dest of the instruction
                for (var j = 0; j < instr.dests.length; ++j)
                {
                    var dest = instr.dests[j];

                    // If the block of the destination is reachable
                    if (reachable[dest.parentBlock.blockId])
                    {
                        // Add the dest to the SSA work list
                        ssaWorkList.push(dest);
                    }
                }
            }
        }

        // Until the SSA work list is processed
        while (ssaWorkList.length > 0)
        {
            // Remove an edge from the SSA work list
            var v = ssaWorkList.pop();

            // Evaluate the value of the edge dest
            var t = evalInstr(v);

            //print(t);

            // If the instruction value has changed
            if (t !== instrVals[v.instrId])
            {
                // Update the value for this instruction
                instrVals[v.instrId] = t;
                
                // For each dest of v
                for (var i = 0; i < v.dests.length; ++i)
                {
                    var dest = v.dests[i];

                    // If the block of the destination is reachable
                    if (reachable[dest.parentBlock.blockId])
                    {
                        // Add the dest to the SSA work list
                        ssaWorkList.push(dest);
                    }
                }
            }
        }
    }

    // Get the value of a constant use or instruction
    function getValue(val)
    {
        if (val instanceof ConstValue)
            return val;
        else
            return instrVals[val.instrId];
    }

    // Test if a basic block is reachable
    function isReachable(block)
    {
        return (reachable[block.blockId] === true);
    }

    // Evaluate an SSA instruction
    function evalInstr(instr)
    {
        //print('evaluating: ' + instr);

        // If there is a const prop function for this instruction, use it
        if (instr.constEval)
        {
            var val = instr.constEval(getValue, isReachable, cfgWorkList);

            if (val instanceof IRInstr && 
                instrVals[val.instrId] instanceof ConstValue)
                return instrVals[val.instrId];

            //print('result: ' + val);

            return val;
        }

        // Otherwise, if this instruction is a generic branch
        else if (instr.isBranch())
        {
            // Put all branches on the CFG work list
            for (var i = 0; i < instr.targets.length; ++i)
            {
                if (instr.targets[i])
                    cfgWorkList.push(instr.targets[i]);
            }
        }

        // By default, return the non-constant value
        return BOT;
    }
    
    var numConsts = 0;
    var numBranches = 0;

    // For each block in the CFG
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];

        //print('processing: ' + block.getBlockName());

        // If this block is not reachable, skip it
        if (!isReachable(block))
        {
            //print('unreachable: ' + block.getBlockName());
            continue;
        }

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Get the value for this instruction
            var val = getValue(instr);

            // If this is an if instruction
            if (instr instanceof IfInstr)
            {
                // If only one if branch is reachable, replace the if by a jump
                if (val instanceof ConstValue && val.value === true)
                {
                    block.replInstrAtIndex(j, new JumpInstr(instr.targets[0]));
                    ++numBranches;
                }
                else if (val instanceof ConstValue && val.value === false)
                {
                    block.replInstrAtIndex(j, new JumpInstr(instr.targets[1]));
                    ++numBranches;
                }
            }

            // If this is an arithmetic instruction with overflow 
            // and we have a replacement value
            else if (instr instanceof ArithOvfInstr && val instanceof IRValue)
            {
                //print(instr + ' ==> ' + val);
                //print(instr.parentBlock.parentCFG.ownerFunc.funcName);

                // Remap the dests to the replacement instruction
                while (instr.dests.length > 0)
                {
                    var dest = instr.dests[0];

                    dest.replUse(instr, val);

                    if (val instanceof IRInstr)
                        val.addDest(dest);

                    instr.remDest(dest);
                }

                // Replace the instruction by a jump to the normal branch
                block.replInstrAtIndex(j, new JumpInstr(instr.targets[0]));
                ++numBranches;
            }

            // If there is a constant value for this instruction
            else if (val instanceof ConstValue)
            {
                //print(instr + ' ==> ' + val);

                // Replace the instruction by the constant value
                block.replInstrAtIndex(j, val);
                --j;
            }

            // If there is a replacement instruction value for this instruction
            else if (val instanceof IRValue && val !== instr)
            {
                //print(instr + ' ==> ' + val);

                // Remap the dests to the replacement instruction
                for (var k = 0; k < instr.dests.length; ++k)
                {
                    var dest = instr.dests[k];

                    print('DEST: ' + dest);

                    dest.replUse(instr, val);
                    val.addDest(dest);
                }

                // Remove the instruction
                block.remInstrAtIndex(j, val);
                --j;
            }
        }
    }

    //print('Constants found: ' + numConsts);
    //print('Branches found: ' + numBranches);

    // Remove basic blocks which are now unreachable from the CFG
    cfg.remDeadBlocks();
}

//=============================================================================
//
// Constant propagation functions for IR instructions
//
//=============================================================================

PhiInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    var curVal;

    // For each incoming value
    for (var i = 0; i < this.uses.length; ++i)
    {
        var useVal = getValue(this.uses[i]);
        var pred = this.preds[i];

        // If this predecessor is not reachable, ignore its value
        if (!isReachable(pred))
            continue;

        // If any use is still top, the current value is unknown
        if (useVal === TOP)
            return TOP;

        // If not all uses have the same value, return the non-constant value
        if (useVal != curVal && curVal != undefined)
            return BOT;

        curVal = useVal;
    }

    // All uses have the same constant value
    return curVal;
};

ArithInstr.genConstEval = function (opFunc, genFunc)
{
    function constEval(getValue, isReachable, cfgWorkList)
    {
        var v0 = getValue(this.uses[0]);
        var v1 = getValue(this.uses[1]);

        if (v0 === TOP || v1 === TOP)
            return TOP;

        if (v0 instanceof ConstValue && v1 instanceof ConstValue)
        {
            var result = opFunc(v0.value, v1.value, v0.type);

            // If there was no overflow, return the result
            if (result >= v0.type.minVal && result <= v0.type.maxVal)
            {
                return ConstValue.getConst(
                    result,
                    v0.type
                );
            }
        }
        else if (genFunc)
        {
            return genFunc(v0, v1);
        }

        // By default, return the unknown value
        return BOT;
    }

    return constEval;
};

AddInstr.prototype.constEval = ArithInstr.genConstEval(
    function (v0, v1)
    {
        return v0 + v1;
    },
    function (u0, u1)
    {
        if (u0 instanceof ConstValue && u0.value == 0)
            return u1;

        if (u1 instanceof ConstValue && u1.value == 0)
            return u0;

        return BOT;
    }
);

SubInstr.prototype.constEval = ArithInstr.genConstEval(
    function (v0, v1)
    {
        return v0 - v1;
    },
    function (u0, u1)
    {
        if (u1 instanceof ConstValue && u1.value == 0)
            return u0;

        return BOT;
    }
);

MulInstr.prototype.constEval = ArithInstr.genConstEval(
    function (v0, v1)
    {
        return v0 * v1;
    },
    function (u0, u1)
    {
        if (u0 instanceof ConstValue && u0.value == 1)
            return u1;

        if (u1 instanceof ConstValue && u1.value == 1)
            return u0;

        if (((u0 instanceof ConstValue && u0.value == 0) || 
             (u1 instanceof ConstValue && u1.value == 0)) &&
            u0.type === u1.type)
        {
            return ConstValue.getConst(
                0,
                u0.type
            );
        }

        return BOT;
    }
);

DivInstr.prototype.constEval = ArithInstr.genConstEval(
    function (v0, v1, type)
    {
        var res = v0 / v1;

        if (type.isInt())
        {
            if (res > 0)
                return Math.floor(res);
            else
                return Math.ceil(res);
        }

        return res;
    },
    function (u0, u1)
    {
        if (u1 instanceof ConstValue && u1.value == 1)
            return u0;

        return BOT;
    }
);

ModInstr.prototype.constEval = ArithInstr.genConstEval(
    function (v0, v1, type)
    {
        if (type.isInt() && v0 < 0 || v1 < 0)
            return NaN;

        return v0 % v1;
    }
);

BitOpInstr.genConstEval = function (opFunc, genFunc)
{
    function constEval(getValue, isReachable, cfgWorkList)
    {
        var v0 = getValue(this.uses[0]);
        var v1 = getValue(this.uses[1]);

        if (v0 === TOP || v1 === TOP)
            return TOP;

        var val0 = (v0.type === IRType.box)? (v0.value << TAG_NUM_BITS_INT):v0.value;
        var val1 = (v1.type === IRType.box)? (v1.value << TAG_NUM_BITS_INT):v1.value;

        if (v0 instanceof ConstValue && v1 instanceof ConstValue &&
            typeof v0.value == 'number' && typeof v1.value == 'number' &&
            val0 >= IRType.i32.minVal && val0 <= IRType.i32.maxVal &&
            val1 >= IRType.i32.minVal && val1 <= IRType.i32.maxVal)
        {
            var result = opFunc(val0, val1);

            // If the result is within the range of the output type, return it
            if (result >= this.type.minVal && result <= this.type.maxVal)
            {
                return ConstValue.getConst(
                    result,
                    this.type
                );
            }
        }

        else if (genFunc)
        {
            return genFunc(v0, v1, this.type);
        }

        // By default, return the unknown value
        return BOT;
    }

    return constEval;
};

AndInstr.prototype.constEval = BitOpInstr.genConstEval(
    function (v0, v1)
    {
        return v0 & v1;
    },
    function (u0, u1, type)
    {
        if ((u0 instanceof ConstValue && u0.value == 0) ||
            (u1 instanceof ConstValue && u1.value == 0))
        {
            return ConstValue.getConst(
                0,
                type
            );
        }

        if (u0 instanceof ConstValue &&
            u1 instanceof ConstValue &&
            u0.type === IRType.box &&
            !u0.isBoxInt() &&
            u1.type === IRType.pint && 
            u1.value === TAG_REF_MASK)
        {
            return ConstValue.getConst(
                u0.getTagBits(),
                IRType.pint
            );
        }

        if (u0 instanceof ConstValue &&
            u1 instanceof ConstValue &&
            u0.type === IRType.box &&
            !u1.isBoxInt() &&
            u0.type === IRType.pint && 
            u0.value === TAG_REF_MASK)
        {
            return ConstValue.getConst(
                u1.getTagBits(),
                IRType.pint
            );
        }

        if (u0 instanceof ConstValue &&
            u1 instanceof ConstValue &&
            u0.type === IRType.box &&
            u1.type === IRType.pint && 
            u1.value === TAG_INT_MASK)
        {
            return ConstValue.getConst(
                u0.getTagBits() & TAG_INT_MASK,
                IRType.pint
            );
        }

        if (u0 instanceof ConstValue &&
            u1 instanceof ConstValue &&
            u1.type === IRType.box &&
            u0.type === IRType.pint && 
            u0.value === TAG_INT_MASK)
        {
            return ConstValue.getConst(
                u1.getTagBits() & TAG_INT_MASK,
                IRType.pint
            );
        }

        return BOT;
    }
);

OrInstr.prototype.constEval = BitOpInstr.genConstEval(
    function (v0, v1)
    {
        return v0 | v1;
    },
    function (u0, u1, type)
    {
        if (u0 instanceof ConstValue && u0.value == 0)
            return u1;

        if (u1 instanceof ConstValue && u1.value == 0)
            return u0;

        return BOT;
    }
);

XorInstr.prototype.constEval = BitOpInstr.genConstEval(
    function (v0, v1)
    {
        return v0 ^ v1;
    }
);

LsftInstr.prototype.constEval = BitOpInstr.genConstEval(
    function (v0, v1)
    {
        return v0 << v1;
    }
);

RsftInstr.prototype.constEval = BitOpInstr.genConstEval(
    function (v0, v1)
    {
        return v0 >> v1;
    }
);

UrsftInstr.prototype.constEval = BitOpInstr.genConstEval(
    function (v0, v1)
    {
        return v0 >>> v1;
    }
);

ICastInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    var v0 = getValue(this.uses[0]);

    if (v0 === TOP)
        return TOP;

    if (v0 instanceof ConstValue)
    {
        var result;

        if (v0.type === this.type)
        {
            result = v0.value;
        }
        else if (v0.type === IRType.box && this.type.isInt())
        {
            var castVal = v0.value << TAG_NUM_BITS_INT;
            
            if (castVal >= this.type.minVal && castVal <= this.type.maxVal)
                result = castVal;
        }
        else if (v0.type.isInt() && this.type.isInt())
        {
            if (v0.value >= this.type.minVal && v0.value <= this.type.maxVal)
                result = v0.value;
        }

        if (result !== undefined)
        {
            return ConstValue.getConst(
                result,
                this.type
            );
        }
    }

    else if (v0.type === this.type)
    {
        return v0;
    }

    return BOT;
};

CompInstr.genConstEval = function (opFunc)
{
    function constEval(getValue, isReachable, cfgWorkList)
    {
        var v0 = getValue(this.uses[0]);
        var v1 = getValue(this.uses[1]);

        if (v0 === TOP || v1 === TOP)
            return TOP;

        if (v0 instanceof ConstValue && v1 instanceof ConstValue)
        {
            var test = opFunc(v0.value, v1.value);

            if (test != undefined)
            {
                return ConstValue.getConst(
                    (this.type === IRType.box)? test:(test? 1:0),
                    this.type
                );
            }
        }

        // By default, return the unknown value
        return BOT;
    }

    return constEval;
};

LtInstr.prototype.constEval = CompInstr.genConstEval(
    function (v0, v1)
    {
        if ((typeof v0 != 'number' && typeof v0 != 'string') ||
            (typeof v1 != 'number' && typeof v1 != 'string'))
            return undefined;

        return v0 < v1;
    }
);

LeInstr.prototype.constEval = CompInstr.genConstEval(
    function (v0, v1)
    {
        if ((typeof v0 != 'number' && typeof v0 != 'string') ||
            (typeof v1 != 'number' && typeof v1 != 'string'))
            return undefined;

        return v0 <= v1;
    }
);

GtInstr.prototype.constEval = CompInstr.genConstEval(
    function (v0, v1)
    {
        if ((typeof v0 != 'number' && typeof v0 != 'string') ||
            (typeof v1 != 'number' && typeof v1 != 'string'))
            return undefined;

        return v0 > v1;
    }
);

GeInstr.prototype.constEval = CompInstr.genConstEval(
    function (v0, v1)
    {
        if ((typeof v0 != 'number' && typeof v0 != 'string') ||
            (typeof v1 != 'number' && typeof v1 != 'string'))
            return undefined;

        return v0 >= v1;
    }
);

EqInstr.prototype.constEval = CompInstr.genConstEval(
    function (v0, v1)
    {
        return v0 == v1;
    }
);

NeInstr.prototype.constEval = CompInstr.genConstEval(
    function (v0, v1)
    {
        return v0 != v1;
    }
);

ArithOvfInstr.genConstEval = function (opFunc, genFunc)
{
    function constEval(getValue, isReachable, cfgWorkList)
    {
        var v0 = getValue(this.uses[0]);
        var v1 = getValue(this.uses[1]);

        if (v0 === TOP || v1 === TOP)
        {
            return TOP;
        }

        if (v0 instanceof ConstValue && v1 instanceof ConstValue)
        {
            var result = opFunc(v0.value, v1.value, v0.type);

            // If there was no overflow
            if (result >= -MAX_FIXNUM && result <= MAX_FIXNUM)
            {
                // Add the normal (non-overflow) branch to the work list
                cfgWorkList.push(this.targets[0]);

                // Return the result
                return ConstValue.getConst(
                    result,
                    v0.type
                );
            }
        }

        else if (genFunc)
        {
            var result = genFunc(v0, v1);

            if (result !== BOT)
            {
                // Add the normal (non-overflow) branch to the work list
                cfgWorkList.push(this.targets[0]);

                return result;
            }
        }

        // By default, both branches are reachable (an overflow could occur)
        cfgWorkList.push(this.targets[0]);
        cfgWorkList.push(this.targets[1]);

        // By default, return the unknown value
        return BOT;
    }

    return constEval;
};
/*
AddOvfInstr.prototype.constEval = ArithInstr.genConstEval(
    function (v0, v1)
    {
        return v0 + v1;
    },
    function (u0, u1)
    {
        if (u0 instanceof ConstValue && u0.value == 0)
            return u1;

        if (u1 instanceof ConstValue && u1.value == 0)
            return u0;

        return BOT;
    }
);
*/
SubOvfInstr.prototype.constEval = ArithOvfInstr.genConstEval(
    function (v0, v1)
    {
        return v0 - v1;
    },
    function (u0, u1)
    {
        if (u1 instanceof ConstValue && u1.value == 0)
            return u0;

        return BOT;
    }
);

function constEvalBool(val)
{
    // If the test is a constant
    if (val instanceof ConstValue)
    {
        // If the test evaluates to true
        if (
            val.value === true ||
            (val.isNumber() && val.value != 0) ||
            (val.isString() && val.value != '')
        )
        {
            return ConstValue.getConst(true);
        }

        // If the test evaluates to false
        else if (
            val.value === false ||
            val.value === null ||
            val.value === undefined ||
            val.value == 0 ||
            val.value == ''
        )
        {
            return ConstValue.getConst(false);
        }
    }

    // Return the non-constant value
    return BOT;
}

CallFuncInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    // If this is a call to boxToBool
    if (this.uses[0] instanceof IRFunction && 
        this.uses[0].funcName == 'boxToBool')
    {
        // Evaluate the boolean value
        var boolVal = constEvalBool(this.uses[this.uses.length-1]);

        if (boolVal instanceof ConstValue)
        {
            return ConstValue.getConst(
                boolVal.value? 1:0,
                this.type
            );
        }
    }

    // Add all branch targets to the CFG work list
    for (var i = 0; i < this.targets.length; ++i)
        if (this.targets[i])
            cfgWorkList.push(this.targets[i]);

    return BOT;
};

IfInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    // Evaluate the test value
    var test = constEvalBool(getValue(this.uses[0]));

    // If the test is a constant
    if (test.value === true)
    {
        // Add the true branch to the work list
        cfgWorkList.push(this.targets[0]);
    }

    // If the test evaluates to false
    else if (test.value === false)
    {
        // Add the false branch to the work list
        cfgWorkList.push(this.targets[1]);
    }

    // If test is non-constant, both branches are reachable
    else if (test === BOT)
    {
        cfgWorkList.push(this.targets[0]);
        cfgWorkList.push(this.targets[1]);
    }

    // Return the test value
    return test;
};
