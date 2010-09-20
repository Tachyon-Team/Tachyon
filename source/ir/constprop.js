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

                // Evaluate the instruction for the first time
                instrVals[instr.instrId] = evalInstr(instr);

                // For each dest of the instruction
                for (var j = 0; j < instr.dests; ++j)
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
        // If there is a const prop function for this instruction, use it
        if (instr.constEval)
        {
            return instr.constEval(getValue, isReachable, cfgWorkList);
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
    
    //
    // TODO: integrate some peephole patterns into evalInstr?
    //

    var numConsts = 0;
    var numBranches = 0;

    // For each block in the CFG
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];

        // For each instruction in the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // Get the value for this instruction
            var val = getValue(instr);

            // If there is a constant value for this instruction
            if (val instanceof ConstValue)
            {
                //print(instr);
                ++numConsts;

                // Replace the instruction by the constant value
                block.replInstrAtIndex(val, j);
                --j;
            }

            // If this is an if instruction
            else if (instr instanceof IfInstr)
            {
                if (val === true || val === false)
                    ++numBranches;

                // If test evaluates to true or false, replace the if by a jump
                if (val === true)
                    block.replInstrAtIndex(new JumpInstr(instr.targets[0]), j);
                else if (val === false)
                    block.replInstrAtIndex(new JumpInstr(instr.targets[1]), j);
            }
        }
    }

    print('Constants found: ' + numConsts);
    print('Branches found: ' + numBranches);
}

//=============================================================================
//
// Constant propagation functions for IR instructions
//
//=============================================================================

PhiInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    //
    // TODO: evaluate phi functions
    // Take reachability of preds into account
    //



    
    return BOT;
}


// TODO: function for arithmetic op implementation?
// Generate func using closure?
// Problem: division, modulo, integer behavior

AddInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    var v0 = getValue(this.uses[0]);
    var v1 = getValue(this.uses[1]);

    if (v0 === TOP || v1 === TOP)
        return TOP;

    // TODO: handle overflow on integer values...
    // - Max and min range?
    // Can simply drop the result if overflow occurs

    if (v0 instanceof ConstValue && v1 instanceof ConstValue)
    {
        return ConstValue.getConst(
            v0.value + v1.value,
            v0.type
        );
    }

    return BOT;
}

SubInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    var v0 = getValue(this.uses[0]);
    var v1 = getValue(this.uses[1]);

    if (v0 === TOP || v1 === TOP)
        return TOP;

    // TODO: handle overflow on integer values...
    // - Max and min range?
    // Can simply drop the result if overflow occurs

    if (v0 instanceof ConstValue && v1 instanceof ConstValue)
    {
        return ConstValue.getConst(
            v0.value - v1.value,
            v0.type
        );
    }

    return BOT;
}

MulInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    var v0 = getValue(this.uses[0]);
    var v1 = getValue(this.uses[1]);

    if (v0 === TOP || v1 === TOP)
        return TOP;

    // TODO: handle overflow on integer values...
    // - Max and min range?
    // Can simply drop the result if overflow occurs

    if (v0 instanceof ConstValue && v1 instanceof ConstValue)
    {
        return ConstValue.getConst(
            v0.value * v1.value,
            v0.type
        );
    }

    return BOT;
}



AndInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    var v0 = getValue(this.uses[0]);
    var v1 = getValue(this.uses[1]);

    if (v0 === TOP || v1 === TOP)
        return TOP;

    // TODO: 64 bit values, need to compute in two parts...

    if (
        v0 instanceof ConstValue && v1 instanceof ConstValue &&
        v0.type === IRType.box && v1.isInt()
    )
    {
        return ConstValue.getConst(
            v0.value << 2 & v1.value,
            v1.type
        );
    }

    return BOT;
}




/*
// TODO: implement
  
        // ICast instruction
        else if (instr instanceof ICastInstr)
        {
            var v0 = getValue(instr.uses[0]);

            if (v0 === TOP)
                return TOP;

            // TODO

            if (v0 instanceof ConstValue && instr.type)
            {
                return ConstValue.getConst(
                    op1.value * op2.value,
                    op1.type
                );
            }
        }
*/



EqInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    var v0 = getValue(this.uses[0]);
    var v1 = getValue(this.uses[1]);

    if (v0 === TOP || v1 === TOP)
        return TOP;

    if (
        v0 instanceof ConstValue && v1 instanceof ConstValue &&
        typeof v0.value === typeof v1.value
    )
    {
        var test = v0.value === v1.value;

        return ConstValue.getConst(
            (this.type === IRType.box)? test:(test? 1:0),
            this.type
        );
    }

    return BOT;
}



IfInstr.prototype.constEval = function (getValue, isReachable, cfgWorkList)
{
    // TODO: extract boolean eval for boxtobool...

    var test = getValue(this.uses[0]);

    // If the test is a constant
    if (test instanceof ConstValue)
    {
        // If the test evaluates to true
        if (
            test === true ||
            (test.isNumber() && test.value != 0)
        )
        {
            // Add the true branch to the work list
            cfgWorkList.push(this.targets[0]);
            return true;
        }

        // If the test evaluates to false
        else if (
            test.value === false ||
            test.value === null ||
            test.value === undefined ||
            test.value == 0
        )
        {
            // Add the false branch to the work list
            cfgWorkList.push(this.targets[1]);
            return false;
        }
    }

    // If test is non-constant, both branches are reachable
    if (test !== TOP)
    {
        cfgWorkList.push(this.targets[0]);
        cfgWorkList.push(this.targets[1]);
    }
}
