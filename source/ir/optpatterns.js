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
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert
*/

/**
Apply all peephole optimization patterns to a CFG
*/
function applyPatternsCFG(cfg, params, maxItrs, printInfo)
{
    assert (
        params instanceof CompParams,
        'compilation parameters expected'
    );

    // Apply all block patterns
    var numItrs = applyPatternsListCFG(
        blockPatterns,
        cfg,
        maxItrs,
        printInfo,
        false,
        params
    );

    return numItrs;
}

/**
Apply peephole optimization patterns to a CFG
*/
function applyPatternsListCFG(blockPatterns, cfg, maxItrs, printInfo, validate, params)
{
    if (printInfo === true)
    {
        print('Processing CFG of function "' + cfg.ownerFunc.funcName + '"');
        //print(cfg);
    }

    // Flag to indicate that changes occurred in the CFG
    var changed = true;

    // Repeat until no more changes occur or the max iteration count is reached
    for (var itrCount = 0; 
         changed && (maxItrs === undefined || itrCount < maxItrs);
         ++itrCount
    )
    {
        //print('*** itr ' + itrCount);

        // No changes in this iteration yet
        changed = false;

        // For each block in the CFG
        for (var i = 0; i < cfg.blocks.length; ++i)
        {
            var block = cfg.blocks[i];

            // Apply block-level patterns to the block
            var result = applyPatternsBlock(blockPatterns, cfg, block, printInfo, params);

            // If any changes occurred, set the changed flag
            if (result)
                changed = true;

            // If the validation flag is on
            if (validate)
            {
                // Remove any dead blocks
                cfg.remDeadBlocks();

                // Validate the CFG
                cfg.validate();
            }
        }

        // Remove dead blocks from the CFG
        cfg.remDeadBlocks();
    }

    // Return the iteration count
    return itrCount;
}

/**
@class Optimization pattern applicable to a basic block or instruction
*/
function optPattern(
    name,
    matchFunc,
    applyFunc
)
{
    /**
    Optimization pattern name
    @field
    */
    this.name = name;

    /**
    Element match test method
    @function
    */
    this.match = matchFunc;

    /**
    Pattetn application method
    @function
    */
    this.apply = applyFunc;
}

/**
List of block-level optimization patterns
*/
var blockPatterns = [];

/**
List of instruction-level optimization patterns
*/
var instrPatterns = [];

/**
Merge blocks with only one destination
*/
blockPatterns.predSuccMerge = new optPattern(
    'predecessor-successor merge',
    function match(cfg, block, params)
    {
        // If this block has only one successor, which has only one predecessor
        // and the block is not terminated by an exception-producing instruction
        return (
            block.succs.length === 1 && 
            block.succs[0].preds.length === 1 &&
            block !== block.succs[0] &&
            !(block.getLastInstr() instanceof ExceptInstr && 
              block.getLastInstr().getThrowTarget())
        );
    },
    function apply(cfg, block, printInfo, params)
    {
        //print('merging block with one succ: ' + block.getBlockName());
        //print('successor: ' + block.succs[0].getBlockName());

        var succ = block.succs[0];

        // For each instruction of the successor
        for (var iIndex = 0; iIndex < succ.instrs.length; ++iIndex)
        {
            var instr = succ.instrs[iIndex];
            
            // If the instruction is a phi node
            if (!(instr instanceof PhiInstr))
                break;
 
            // Ensure that this phi node has only one predecessor
            if (DEBUG === true && !(instr.preds.length === 1))
            {
                error(
                    'phi node in merged block should have one pred:\n' +
                    instr
                );
            }

            var phiIn = instr.uses[0];

            // Remove the edge from the use to the phi node
            if (phiIn instanceof IRInstr)
                phiIn.remDest(instr);

            // For each dest of the phi node
            for (var k = 0; k < instr.dests.length; ++k)
            {
                // Replace all uses of the phi by uses of its input
                instr.dests[k].replUse(instr, phiIn);
                if (phiIn instanceof IRInstr)
                    phiIn.addDest(instr.dests[k]);
            }
        }

        // Remove the final branch instruction from the predecessor
        block.remInstrAtIndex(block.instrs.length - 1);

        // For each non-phi instruction of the successor
        for (; iIndex < succ.instrs.length; ++iIndex)
        {
            var instr = succ.instrs[iIndex];

            // Add the instruction to the predecessor
            block.addInstr(instr);
        }

        //print('Number of succ succs: ' + succ.succs.length);

        // For each successor of the successor
        for (var j = 0; j < succ.succs.length; ++j)
        {
            var succSucc = succ.succs[j];

            // For each instruction of the successor's successor
            for (var k = 0; k < succSucc.instrs.length; ++k)
            {
                var instr = succSucc.instrs[k];
                
                // If the instruction is not a phi node, stop
                if (!(instr instanceof PhiInstr))
                    break;

                // Change references to the successor to references
                // to the predecessor instead
                instr.replPred(succ, block);
            }
        }

        // Remove the successor block from the CFG
        cfg.remBlock(succ, false);

        // The CFG was changed
        return true;
    }
);
blockPatterns.push(blockPatterns.predSuccMerge);

/**
Jump over blocks with no instructions and a single successor
*/
blockPatterns.emptyBypass = new optPattern(
    'empty block bypassing',
    function match(cfg, block, params)
    {
        // If this block has only one successor, no instructions but a branch, 
        // and the block is not terminated by an exception-producing instruction
        return (
            block.succs.length === 1 && 
            block.succs[0] !== block &&
            block.instrs.length === 1 &&
            !(block.getLastInstr() instanceof ExceptInstr && 
              block.getLastInstr().getThrowTarget())
        );
    },
    function apply(cfg, block, printInfo, params)
    {
        var succ = block.succs[0];
    
        //print('block w/ no instrs, 1 succ: ' + block.getBlockName());

        // Copy the predecessor list for this block
        var preds = block.preds.slice(0);

        // Flag to indicate the CFG was changed
        var changed = false;

        // For each predecessor
        PRED_LOOP:
        for (var j = 0; j < preds.length; ++j)
        {
            var pred = preds[j];

            //print('got pred: ' + pred.getBlockName());

            // For each instruction of the successor
            for (var k = 0; k < succ.instrs.length; ++k)
            {
                var instr = succ.instrs[k];

                // If this instruction is not a phi node, stop
                if (!(instr instanceof PhiInstr))
                    break;                        

                // If the phi node already has this predecessor, skip the predecessor
                if (arraySetHas(instr.preds, pred))
                    continue PRED_LOOP;
            }

            //print('simplifying no instructions, single successor: ' + 
            //block.getBlockName() + ', succ: ' + succ.getBlockName());

            // Remove the predecessor from our predecessor list
            block.remPred(pred);

            // Get the predecessor's branch instruction
            var branchInstr = pred.getLastInstr();

            // Replace the predecessor's branch target by the successor
            for (var k = 0; k < branchInstr.targets.length; ++k)
                if (branchInstr.targets[k] === block)
                    branchInstr.targets[k] = succ;

            // Replace the predecessor's successor block by the successor
            pred.remSucc(block);
            pred.addSucc(succ);

            // Add the predecessor as a predecessor of the successor
            succ.addPred(pred);

            // For each instruction of the successor
            for (var k = 0; k < succ.instrs.length; ++k)
            {
                var instr = succ.instrs[k];

                // If this instruction is not a phi node, stop
                if (!(instr instanceof PhiInstr))
                    break;

                assert (
                    arraySetHas(succ.preds, block),
                    'successor does not have us as a predecessor'
                );

                // Add an incoming value for the predecessor
                var inVal = instr.getIncoming(block);
                instr.addIncoming(inVal, pred);
            }

            // Set the changed flag
            changed = true;
        }

        return changed;
    }
);
blockPatterns.push(blockPatterns.emptyBypass);

/**
Eliminate blocks of the form:
t = phi x1,x2,x3
(if t === true/false then b1 else b2) or (jump b1)
*/
blockPatterns.ifPhiElim = new optPattern(
    'if-phi elimination',
    function match(cfg, block, params)
    {
        // Get references to the first and last instructions
        var firstInstr = block.instrs[0];
        var lastInstr = block.getLastInstr();

        // If the first instruction is not a phi node, or the block
        // doesn't have 2 instructions, no match
        if (!(firstInstr instanceof PhiInstr) ||
            block.instrs.length !== 2)
            return false;

        // For each dest of the phi node
        for (var i = 0; i < firstInstr.dests.length; ++i)
        {
            var dest = firstInstr.dests[i];

            // If the use is in this block, continue
            if (dest.parentBlock === block)
                continue;

            // If the dest is not a phi instruction in a successor, no match
            if (!(dest instanceof PhiInstr) || 
                !arraySetHas(dest.parentBlock.preds, block))
                return false;
        }

        // If the last instruction is a jump, match
        if (lastInstr instanceof JumpInstr)
            return true;

        // If the last instruction is a phi with a boolean test, match
        if (lastInstr instanceof IfInstr &&
            lastInstr.uses[0] === firstInstr &&
            lastInstr.uses[1] instanceof IRConst &&
            typeof lastInstr.uses[1].value === 'boolean' &&
            (lastInstr.testOp !== 'EQ' || lastInstr.testOp !== 'NE'))
            return true;

        // No match
        return false;
    },
    function apply(cfg, block, printInfo, params)
    {
        var phiInstr = block.instrs[0];
        var phiPreds = phiInstr.preds.slice(0);
        var phiUses = phiInstr.uses.slice(0);

        var branchInstr = block.getLastInstr();

        // Determine the target for a phi predecessor
        function getTarget(use)
        {
            // If our branch is a jump, return its target
            if (branchInstr instanceof JumpInstr)
                return branchInstr.targets[0];

            // If we could not determine the truth value, branch unknown
            if (!(use instanceof IRConst))
                return false;

            // Perform the if comparison
            if (branchInstr.testOp == 'EQ')
                var compVal = (use.value === branchInstr.uses[1].value);
            else
                var compVal = (use.value !== branchInstr.uses[1].value);

            // Evaluate which target the predecessor should jump to
            return branchInstr.targets[
                compVal? 0:1
            ];
        }

        // Determine if we can branch from a predecessor to a target
        function canJump(pred, target)
        {
            // If the predecessor already jumps to the target, can't jump
            if (arraySetHas(pred.succs, target) === true)
                return false

            // For each phi node in the target
            for (var j = 0; j < target.instrs.length; ++j)
            {
                var instr = target.instrs[j];

                if (!(instr instanceof PhiInstr))
                    break;

                // Get the incoming phi value corresponding to this block
                var inc = instr.getIncoming(block);

                // If the incoming value is our phi node, skip it
                if (inc === phiInstr)
                    continue;

                // If the incoming value is a constant, skip it
                if (inc instanceof IRConst)
                    continue;
                
                // If the incoming value is anything else, can't jump
                return false;
            }
        }

        // Adjust the incoming phi values in a target block
        function adjustPhis(pred, use, target)
        {
            // Add incoming phi values for the predecessor block
            // matching that of the current block
            for (var j = 0; j < target.instrs.length; ++j)
            {
                var instr = target.instrs[j];

                if (!(instr instanceof PhiInstr))
                    break;

                // Get the incoming phi value corresponding to this block
                var inc = instr.getIncoming(block);

                // If the incoming value is the phi node from this block,
                // use the value coming from our predecessor instead
                if (inc === phiInstr)
                    inc = use;

                // Add the incoming value to the target phi
                instr.addIncoming(inc, pred);
            }
        }

        // Flag to indicate the CFG was changed
        var changed = false;

        // For each phi predecessor
        for (var i = 0; i < phiPreds.length; ++i)
        {
            var pred = phiPreds[i];
            var use = phiUses[i];

            // Get a reference to the predecessor's branch instruction
            var predBranch = pred.getLastInstr();

            // Attempt to determine the target for this predecessor
            var target = getTarget(use);

            // If we could determine a target
            if (target !== false)
            {
                // If we can't directly jump to this target, skip this predecessor
                if (canJump(pred, target) === false)
                    continue;

                // The predecessor will no longer jump to this block
                pred.remSucc(block);

                // Remove the phi and block predecessor
                block.remPred(pred);
                phiInstr.remPred(pred);

                // Make the predecessor jump to the right target
                for (var j = 0; j < predBranch.targets.length; ++j)
                {
                    if (predBranch.targets[j] === block)
                        predBranch.targets[j] = target;

                    pred.addSucc(predBranch.targets[j]);
                }

                // Add the predecessor to the if target block
                target.addPred(pred);

                // Adjust incoming phi values in the target
                adjustPhis(pred, use, target);

                // Set the changed flag
                changed = true;
            }

            // Otherwise, if the predecessor branch is a jump
            else if (predBranch instanceof JumpInstr)
            {
                //print('eliminating phi node pred for ' + phiInstr + ' in ' + block.getBlockName());

                // Replace the jump by an if instruction
                pred.replBranch(
                    new IfInstr(
                        [use, branchInstr.uses[1]],
                        branchInstr.testOp,
                        branchInstr.targets[0],
                        branchInstr.targets[1]
                    )
                );

                // For each if target. add an incoming phi value for the
                // predecessor block matching that of the current block
                for (var k = 0; k < branchInstr.targets.length; ++k)
                {
                    var target = branchInstr.targets[k];

                    // Adjust incoming phi values in the target
                    adjustPhis(pred, use, target);
                }

                // Set the changed flag
                changed = true;
            }
        }

        // Return whether a change was made or not
        return changed;
    }
);
blockPatterns.push(blockPatterns.ifPhiElim);

/**
Eliminate conditional branches performing the same test as 
previous conditionals
*/
blockPatterns.ifIfElim = new optPattern(
    'if-if elimination',
    function match(cfg, block, params)
    {
        // If this block doesn't have a single predecessor, no match
        if (block.preds.length !== 1)
            return false;

        // Get the branch instruction for this block and its predecessor        
        var thisBranch = block.getLastInstr();
        var predBranch = block.preds[0].getLastInstr();

        // If both blocks do not terminate in if instructions performing
        // the same type of test operation, no match
        if (!(thisBranch instanceof IfInstr) ||
            !(predBranch instanceof IfInstr) ||
            thisBranch.testOp !== predBranch.testOp)
            return false;

        // If any of the uses are not the same, no match
        for (var i = 0; i < thisBranch.uses.length; ++i)
            if (thisBranch.uses[i] !== predBranch.uses[i])
                return false;

        // The if instructions are identical
        return true;
    },
    function apply(cfg, block, printInfo, params)
    {
        var blockIf = block.getLastInstr();
        var predIf = block.preds[0].getLastInstr();

        // Find whether this block is the predecessor's true or false target
        var target = predIf.targets.indexOf(block);

        // Replace our conditional by a direct jump to the correct target
        block.replBranch(new JumpInstr(blockIf.targets[target]));

        // A change was made to the CFG
        return true;
    }
);
blockPatterns.push(blockPatterns.ifIfElim);

/**
Eliminate if statements with two identical targets.
*/
blockPatterns.ifElim = new optPattern(
    'if elimination',
    function match(cfg, block, params)
    {
        // Get the last instruction for this block
        var branch = block.getLastInstr();        

        // Test if this is an if statement with two identical targets
        return (
            branch instanceof IfInstr &&
            branch.targets[0] === branch.targets[1]
        );
    },
    function apply(cfg, block, printInfo, params)
    {
        // Get the last instruction for this block
        var branch = block.getLastInstr();

        // Replace our conditional by a direct jump to the target
        block.replBranch(new JumpInstr(branch.targets[0]));

        // A change was made to the CFG
        return true;
    }
);
blockPatterns.push(blockPatterns.ifElim);

/**
Instruction-level optimization patterns
*/
blockPatterns.instrPatterns = new optPattern(
    'instruction-level patterns',
    function match()
    {
        // Always match
        return true;
    },
    function apply(cfg, block, printInfo, params)
    {
        // Flag to indicate the CFG was changed
        var changed = false;

        // For each instruction of the block
        for (var i = 0; i < block.instrs.length; ++i)
        {
            var instr = block.instrs[i];

            // Apply instruction-level patterns to this instruction
            var result = applyPatternsInstr(cfg, block, instr, i, params);

            if (result)
                changed = true;
        }

        return changed;
    }
);
blockPatterns.push(blockPatterns.instrPatterns);

/**
Apply block-level optimization patterns to a block
*/
function applyPatternsBlock(blockPatterns, cfg, block, printInfo, params)
{
    // For each block level pattern
    for (var i = 0; i < blockPatterns.length; ++i)
    {
        var pattern = blockPatterns[i];

        // If the pattern matches this block
        if (pattern.match(cfg, block, params))
        {
            if (printInfo)
            {
                print(
                    'Applying ' + pattern.name + ' to ' + 
                    block.getBlockName()
                );
            }

            // Try applying the pattern to the block
            var res = pattern.apply(cfg, block, printInfo, params);

           // If changes were made, the CFG was changed
            if (res)
                return true;
        }
    }

    // The CFG was not changed
    return false;
}

/**
Apply instruction-level optimization patterns to an instruction
*/
function applyPatternsInstr(cfg, block, instr, index, params)
{
    // Test if a value is a specific constant
    function isConst(val, cst)
    {
        return (
            val instanceof IRConst &&
            val.value === cst
        );
    }

    // Test if a value is a power of 2 constant
    function isPow2(val)
    {
        return (
            val instanceof IRConst &&
            (val.isBoxInt(params) || val.isInt()) &&
            isPowerOf2(val.value)
        );
    }

    // Replace an arithmetic instruction
    function replArith(replInstr, replInstrOvf, u0, u1)
    {
        if (instr instanceof ArithInstr)
        {
            block.replInstrAtIndex(
                index,
                new replInstr(
                    u0,
                    u1
                )
            );
        }
        else
        {
            assert (
                instr instanceof ArithOvfInstr,
                'expected arithmetic instruction w/ overflow'
            );

            block.replBranch(
                new replInstrOvf(
                    u0,
                    u1,
                    instr.targets[0],
                    instr.targets[1]
                )
            );
        }
    }

    // Replace an instruction by a value
    function replByVal(value)
    {
        // Replace the instruction by a jump to the normal branch
        block.replInstrAtIndex(
            index, 
            (instr instanceof ArithOvfInstr || 
             (instr instanceof CallInstr && instr.getThrowTarget()))?
            new JumpInstr(instr.targets[0]):undefined,
            value
        );
    }

    // If the instruction's value is not used and the instruction
    // has no side effects and is not a branch
    if (
        instr.dests.length === 0 &&
        instr.writesMem() === false &&
        instr.isBranch() === false
    )
    {
        //print('Removing dead: ' + instr);

        // Remove the instruction
        block.remInstrAtIndex(index);

        // A change was made
        return true;
    }
     
    // If the instruction is a phi node
    if (instr instanceof PhiInstr)
    {
        // Delete all phi-assignments of the form:
        // Vi <- phi(...Vi...Vi...)
        //
        // If a phi-assignment has the form:
        // Vi <- phi(...Vi...Vj...Vi...Vj...)
        // 0 or more Vi and 1 or more Vj
        //
        // Then delete the assignment and rename
        // all occurences of Vi to Vj

        var numVi = 0;
        var numVj = 0;
        var Vj = null;

        // Count the kinds of uses of the phi node
        for (var j = 0; j < instr.uses.length; ++j)
        {
            var use = instr.uses[j];

            if (use === instr)
            {
                numVi++;
            }
            else if (use === Vj || Vj === null)
            {
                numVj++;
                Vj = use;
            }
        }

        // If this phi node has the form:
        // Vi <- phi(...Vi...Vi...)
        if (numVi === instr.uses.length)
        {
            //print('Removing phi: ' + instr);

            // Remove the phi node
            block.remInstr(instr);

            // A change was made
            return true;
        }
        
        // If this phi-assignment has the form:
        // Vi <- phi(...Vi...Vj...Vi...Vj...)
        // 0 or more Vi and 1 or more Vj
        else if (numVi + numVj === instr.uses.length)        
        {
            //print('Renaming phi: ' + instr);

            // Rename all occurences of Vi to Vj
            for (var k = 0; k < instr.dests.length; ++k)
            {
                var dest = instr.dests[k];
                dest.replUse(instr, Vj);
                if (Vj instanceof IRInstr)
                    Vj.addDest(dest);
            }
            if (Vj instanceof IRInstr)
                Vj.remDest(instr);

            // Remove the phi node
            block.remInstr(instr);

            // A change was made
            return true;
        }
    }

    //
    // Redundant boxToBool elimination
    //

    // If this is a call to boxToBool
    else if (instr instanceof CallFuncInstr &&
             instr.getCallee() === params.staticEnv.getBinding('boxToBool'))
    {
        // Max number of phi nodes to visit
        const MAX_VISIT = 8;

        // Phi nodes visited
        var visited = [];

        //var thelog = ''

        // Function to test that an IR value must be boolean
        function isBoolean(val)
        {
            //thelog += val + '\n';

            // If the value is a boolean constant, it is boolean
            if (val instanceof IRConst &&
                typeof val.value === 'boolean')
                return true;
            
            // If this is a comparison instruction, it is boolean
            if (val instanceof JSCompInstr)
                return true;

            // If this is a boxToBool call, it is boolean
            if (val instanceof CallFuncInstr &&
                val.getCallee() === params.staticEnv.getBinding('boxToBool'))
                return true;

            // If this is a phi instruction
            if (val instanceof PhiInstr)
            {
                // If we already visited this node, it is boolean
                if (arraySetHas(visited, val) === true)
                    return true;

                // If too many phi nodes were already visited, stop
                if (visited.length >= MAX_VISIT)
                    return false;

                // Mark the phi node as visited
                arraySetAdd(visited, val);

                // If any phi use is not boolean, the result is not boolean
                for (var i = 0; i < val.uses.length; ++i)
                    if (isBoolean(val.uses[i]) === false)
                        return false;

                // The phi node is boolean
                return true;
            }

            // Not a boolean value
            return false;
        }

        // Get the boxToBool argument
        var arg = instr.getArg(0);

        // If the argument to boxToBool can only be boolean
        if (isBoolean(arg))
        {
            // Replace uses of the boxToBool by its argument
            replByVal(arg);

            // A change was made
            return true;
        }
    }

    //
    // Strength reduction patterns
    //

    /*
    // If this is an addition instruction
    else if (instr instanceof AddInstr || instr instanceof AddOvfInstr)
    {
        // If the left operand is 0
        if (isConst(instr.uses[0], 0))
        {
            // Replace the instruction by its right operand
            replByVal(instr.uses[1]);

            // A change was made
            return true;
        }

        // If the right operand is 0
        else if (isConst(instr.uses[1], 0))
        {
            // Replace the instruction by its left operand
            replByVal(instr.uses[0]);

            // A change was made
            return true;
        }
    }

    // If this is a subtraction instruction
    else if (instr instanceof SubInstr || instr instanceof SubOvfInstr)
    {
        // If the right operand is 0        
        if (isConst(instr.uses[1], 0))
        {
            // Replace the instruction by its left operand
            replByVal(instr.uses[0]);

            // A change was made
            return true;
        }
    }
    */

    // If this is a multiplication
    else if (instr instanceof MulInstr || instr instanceof MulOvfInstr)
    {
        /*
        // If the right or left operand is 0
        if (isConst(instr.uses[0], 0) || isConst(instr.uses[1], 0))
        {
            // Replace the instruction by 0
            replByVal(IRConst.getConst(0, instr.type));

            // A change was made
            return true;
        }

        // If the left operand is 1
        else if (isConst(instr.uses[0], 1))
        {
            // Replace the instruction by its right operand
            replByVal(instr.uses[1]);

            // A change was made
            return true;
        }

        // If the right operand is 1
        else if (isConst(instr.uses[1], 1))
        {
            // Replace the instruction by its left operand
            replByVal(instr.uses[0]);

            // A change was made
            return true;
        }
        */

        // If the left operand is a power of 2
        if (isPow2(instr.uses[0]))
        {
            // Replace the multiplication by a left shift
            replArith(
                LsftInstr,
                LsftOvfInstr,
                instr.uses[1],
                IRConst.getConst(
                    highestBit(instr.uses[0].getImmValue(params)),
                    (instr.uses[1].type === IRType.box)? IRType.pint:instr.uses[1].type
                )
            );

            // A change was made
            return true;
        }

        // If the right operand is a power of 2
        else if (isPow2(instr.uses[1]))
        {
            // Replace the multiplication by a left shift
            replArith(
                LsftInstr,
                LsftOvfInstr,
                instr.uses[0],
                IRConst.getConst(
                    highestBit(instr.uses[1].getImmValue(params)),
                    (instr.uses[0].type === IRType.box)? IRType.pint:instr.uses[0].type
                )
            );

            // A change was made
            return true;
        }
    }

    // If this is a division instruction
    else if (instr instanceof DivInstr)
    {
        /*
        // If this is a division by 1
        if (isConst(instr.uses[1], 1))
        {
            // Replace the instruction by its left operand
            replByVal(instr.uses[0]);

            // A change was made
            return true;
        }
        */

        // If this is a division by a power of 2
        if (isPow2(instr.uses[1]) === true)
        {
            // Replace the division by a right shift
            replArith(
                instr.type.isUnsigned()? UrsftInstr:RsftInstr,
                undefined,
                instr.uses[0],
                IRConst.getConst(
                    highestBit(instr.uses[1].getImmValue(params)),
                    (instr.uses[0].type === IRType.box)? IRType.pint:instr.uses[0].type
                )
            );

            // A change was made
            return true;
        }
    }

    /* TODO: handle negative dividend case
    // If this is a modulo operation
    else if (instr instanceof ModInstr)
    {
        // If the result is not boxed
        if (instr.type !== IRType.box)
        {
            // Replace the modulo by a bitwise AND instruction
            replArith(
                AndInstr,
                undefined,
                instr.uses[0],
                IRConst.getConst(
                    num_sub(instr.uses[1].value, 1),
                    instr.uses[0].type
                )
            );

            // A change was made
            return true;
        }
    }
    */

    /*
    // If this is a logical OR instruction
    else if (instr instanceof OrInstr)
    {
        // If the left operand is 0
        if (isConst(instr.uses[0], 0))
        {
            // Replace the instruction by its right operand
            replByVal(instr.uses[1]);

            // A change was made
            return true;
        }

        // If the right operand is 0
        else if (isConst(instr.uses[1], 0))
        {
            // Replace the instruction by its left operand
            replByVal(instr.uses[0]);

            // A change was made
            return true;
        }
    }

    // If this is a logical AND instruction
    else if (instr instanceof AndInstr)
    {
        var TAG_INT_MASK = params.staticEnv.getBinding('TAG_INT_MASK').value;
        var TAG_REF_MASK = params.staticEnv.getBinding('TAG_REF_MASK').value;

        // If the right or left operand is 0
        if (isConst(instr.uses[0], 0) || isConst(instr.uses[1], 0))
        {
            // Replace the instruction by 0
            replByVal(IRConst.getConst(0, instr.type));

            // A change was made
            return true;
        }

        // If this is a reference tag bit extraction
        if (instr.uses[0] instanceof IRConst &&
            instr.uses[1] instanceof IRConst &&
            instr.uses[0].type === IRType.box &&
            !instr.uses[0].isBoxInt(params) &&
            instr.uses[1].type === IRType.pint && 
            instr.uses[1].value === TAG_REF_MASK)
        {
            replByVal(
                IRConst.getConst(
                    instr.uses[0].getTagBits(params),
                    IRType.pint
                )
            );
        }

        // If this is a reference tag bit extraction
        if (
            instr.uses[0] instanceof IRConst &&
            instr.uses[1] instanceof IRConst &&
            instr.uses[1].type === IRType.box &&
            !instr.uses[1].isBoxInt(params) &&
            instr.uses[0].type === IRType.pint && 
            instr.uses[0].value === TAG_REF_MASK)
        {
            replByVal(
                IRConst.getConst(
                    instr.uses[1].getTagBits(params),
                    IRType.pint
                )
            );
        }

        // If this is an integer tag bit extraction
        if (
            instr.uses[0] instanceof IRConst &&
            instr.uses[1] instanceof IRConst &&
            instr.uses[0].type === IRType.box &&
            instr.uses[1].type === IRType.pint && 
            instr.uses[1].value === TAG_INT_MASK)
        {
            replByVal(
                IRConst.getConst(
                    instr.uses[0].getTagBits(params) & TAG_INT_MASK,
                    IRType.pint
                )
            );
        }

        // If this is an integer tag bit extraction
        if (
            instr.uses[0] instanceof IRConst &&
            instr.uses[1] instanceof IRConst &&
            instr.uses[1].type === IRType.box &&
            instr.uses[0].type === IRType.pint && 
            instr.uses[0].value === TAG_INT_MASK)
        {
            replByVal(
                IRConst.getConst(
                    instr.uses[1].getTagBits(params) & TAG_INT_MASK,
                    IRType.pint
                )
            );
        }
    }

    // If this is an integer cast instruction
    else if (instr instanceof ICastInstr)
    {
        // If the input and output types are the same
        if (instr.uses[0].type === instr.type)
        {
            // Replace the instruction by its input
            replByVal(instr.uses[0]);

            // A change was made
            return true;
        }
    }
    */

    // No changes were made
    return false;
}

