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

    // Remove dead blocks from the CFG
    cfg.remDeadBlocks();

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
            assert (
                instr.preds.length === 1, 
                'phi node in merged block should have one pred:\n' +
                instr
            );

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
        cfg.remBlock(succ);

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

            //print('simplifying no instructions, single successor: ' + block.getBlockName() + ', succ: ' + succ.getBlockName());

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
[t = call <boxToBool> t]
if t b1 b2
*/
blockPatterns.ifPhiElim = new optPattern(
    'if-phi elimination',
    function match(cfg, block, params)
    {
        // If this block contains only an if instruction using the
        // value of an immediately preceding phi
        return (
            block.instrs[0] instanceof PhiInstr &&
            block.instrs[0].dests.length === 1 &&
            (
                (block.instrs.length === 2 &&
                 block.instrs[1] instanceof IfInstr &&
                 block.instrs[1].uses[0] === block.instrs[0])
                ||
                (block.instrs.length === 3 &&
                 block.instrs[1] instanceof CallFuncInstr &&
                 block.instrs[1].getCallee() === params.staticEnv.getBinding('boxToBool') &&
                 block.instrs[1].getArg(0) === block.instrs[0] &&
                 block.instrs[2] instanceof IfInstr &&
                 block.instrs[2].uses[0] === block.instrs[1])
            )
        );
    },
    function apply(cfg, block, printInfo, params)
    {
        var phiInstr = block.instrs[0];
        var phiPreds = phiInstr.preds.slice(0);
        var phiUses = phiInstr.uses.slice(0);
        var boxToBool = false;
        var ifInstr;

        if (block.instrs.length === 2)
        {
            ifInstr = block.instrs[1];
        }
        else
        {
            boxToBool = block.instrs[1];
            ifInstr = block.instrs[2];
        }

        var trueTarget = ifInstr.targets[0];
        var falseTarget = ifInstr.targets[1];

        // Flag to indicate the CFG was changed
        var changed = false;

        // For each phi predecessor
        for (var j = 0; j < phiPreds.length; ++j)
        {
            var pred = phiPreds[j];
            var use = phiUses[j];

            //print(block.getBlockName());
            //print('pred: ' + pred.getBlockName());

            var predBranch = pred.getLastInstr();

            // Attempt to evaluate the truth value of the use
            var truthVal = constEvalBool(use);

            // If a truth value could be evaluated
            if (truthVal !== BOT)
            {
                // We know which target the predecessor should jump to
                var ifTarget = ifInstr.targets[truthVal.value? 0:1];

                // If the predecessor doesn't already jump to the target                      
                if (!arraySetHas(pred.succs, ifTarget))
                {
                    pred.remSucc(block);

                    // Make the predecessor jump to the right target
                    for (var k = 0; k < predBranch.targets.length; ++k)
                    {
                        if (predBranch.targets[k] === block)
                            predBranch.targets[k] = ifTarget;

                        pred.addSucc(predBranch.targets[k]);
                    }

                    // Add the predecessor to the if target block
                    ifTarget.addPred(pred);

                    // Remove the phi and block predecessor
                    block.remPred(pred);
                    phiInstr.remPred(pred);
                    
                    // Add incoming phi values for the predecessor block
                    // matching that of the current block
                    for (var l = 0; l < ifTarget.instrs.length; ++l)
                    {
                        var instr = ifTarget.instrs[l];

                        if (!(instr instanceof PhiInstr))
                            break;

                        var inc = instr.getIncoming(block);
                        instr.addIncoming(inc, pred);
                    }

                    // Set the changed flag
                    changed = true;
                }
            }

            // Otherwise, if the predecessor branch is a jump and there is
            // no call to boxToBool
            else if (predBranch instanceof JumpInstr && !boxToBool)
            {
                //print('eliminating phi node pred for ' + phiInstr + ' in ' + block.getBlockName());

                // Replace the jump by an if instruction
                pred.replInstrAtIndex(
                    pred.instrs.length - 1,
                    new IfInstr(
                        use,
                        ifInstr.targets[0],
                        ifInstr.targets[1]
                    )
                );

                // For each if target. add an incoming phi value for the
                // predecessor block matching that of the current block
                for (var k = 0; k < ifInstr.targets.length; ++k)
                {
                    var target = ifInstr.targets[k];

                    for (var l = 0; l < target.instrs.length; ++l)
                    {
                        var instr = target.instrs[l];

                        if (!(instr instanceof PhiInstr))
                            break;

                        var inc = instr.getIncoming(block);
                        instr.addIncoming(inc, pred);
                    }
                }

                // Set the changed flag
                changed = true;
            }
        }

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
        // If this block ends with an if instruction testing the same value
        // as its only predecessor's if instruction
        return (
            block.getLastInstr() instanceof IfInstr &&
            block.preds.length === 1 &&
            block.preds[0].getLastInstr() instanceof IfInstr &&
            block.getLastInstr().uses[0] === block.preds[0].getLastInstr().uses[0] &&
            block.preds[0].getLastInstr().targets[0] !== block.preds[0].getLastInstr().targets[1]
        );
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
Aggregate phi nodes used only by other phi nodes
*/
blockPatterns.phiMerge = new optPattern(
    'phi node aggregation',
    function match(cfg, block, params)
    {
        // If this block contains only a phi node used only by another phi node
        return (
            block.instrs.length === 2 &&
            block.instrs[0] instanceof PhiInstr &&
            block.instrs[0].dests.length === 1 &&
            block.instrs[0].dests[0] instanceof PhiInstr &&
            block.instrs[1] instanceof JumpInstr &&
            block.succs[0].instrs[0] === block.instrs[0].dests[0]
        );
    },
    function apply(cfg, block, printInfo, params)
    {
        var origPhi = block.instrs[0];
        var destPhi = origPhi.dests[0];
        var destBlock = block.succs[0];

        if (printInfo)
        {
            print('dest phi: ' + destPhi);
            print('dest block: ' + destBlock.getBlockName());
        }

        // Flag to indicate the CFG was changed
        var changed = false;

        // For each incoming value of the origin phi
        for (var i = 0; i < origPhi.uses.length; ++i)
        {
            var use = origPhi.uses[i];
            var pred = origPhi.preds[i];

            // If the destination block has this predecessor, skip it
            if (arraySetHas(destBlock.preds, pred))
                continue;

            // Remove the phi and block predecessor-successor links
            origPhi.remPred(pred);
            block.remPred(pred);
            pred.remSucc(block);

            // Make the pred branch to the dest block
            var branch = pred.getLastInstr();
            for (var j = 0; j < branch.targets.length; ++j)
                if (branch.targets[j] === block)
                    branch.targets[j] = destBlock;

            // Add the block predecessor-successor links
            destBlock.addPred(pred);
            pred.addSucc(destBlock);

            if (printInfo)
                print('adding pred: ' + pred.getBlockName());

            // Add a new incoming value to the dest phi
            destPhi.addIncoming(use, pred);

            // For each other phi node in the block, give it an incoming
            // value for this new predecessor
            for (var j = 0; j < destBlock.instrs.length; ++j)
            {
                var instr = destBlock.instrs[j];

                if (!(instr instanceof PhiInstr))
                    break;

                if (instr === destPhi)
                    continue;

                var inc = instr.getIncoming(block);
                instr.addIncoming(inc, pred);
            }

            // Move back one phi predecessor index
            --i;

            // Set the changed flag
            changed = true;
        }

        return changed;
    }
);
blockPatterns.push(blockPatterns.phiMerge);

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
    // Strength reduction patterns
    //

    // Test if a value is a specific constant
    function isConst(val, cst)
    {
        return (
            val instanceof ConstValue &&
            val.value === cst
        );
    }

    // Test if a value is a power of 2 constant
    function isPow2(val)
    {
        return (
            val instanceof ConstValue &&
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
            (instr instanceof ArithOvfInstr)?
            new JumpInstr(instr.targets[0]):undefined,
            value
        );
    }

    /*
    // If this is an addition instruction
    if (instr instanceof AddInstr || instr instanceof AddOvfInstr)
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
    if (instr instanceof SubInstr || instr instanceof SubOvfInstr)
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
    if (instr instanceof MulInstr || instr instanceof MulOvfInstr)
    {
        /*
        // If the right or left operand is 0
        if (isConst(instr.uses[0], 0) || isConst(instr.uses[1], 0))
        {
            // Replace the instruction by 0
            replByVal(ConstValue.getConst(0, instr.type));

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
                ConstValue.getConst(
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
                ConstValue.getConst(
                    highestBit(instr.uses[1].getImmValue(params)),
                    (instr.uses[0].type === IRType.box)? IRType.pint:instr.uses[0].type
                )
            );

            // A change was made
            return true;
        }
    }

    // If this is a division instruction
    if (instr instanceof DivInstr)
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
        if (isPow2(instr.uses[1]))
        {
            // Replace the division by a right shift
            replArith(
                instr.type.isUnsigned()? UrsftInstr:RsftInstr,
                undefined,
                instr.uses[0],
                ConstValue.getConst(
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
    if (instr instanceof ModInstr)
    {
        // If the result is not boxed
        if (instr.type !== IRType.box)
        {
            // Replace the modulo by a bitwise AND instruction
            replArith(
                AndInstr,
                undefined,
                instr.uses[0],
                ConstValue.getConst(
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
    if (instr instanceof OrInstr)
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
    if (instr instanceof AndInstr)
    {
        var TAG_INT_MASK = params.staticEnv.getBinding('TAG_INT_MASK').value;
        var TAG_REF_MASK = params.staticEnv.getBinding('TAG_REF_MASK').value;

        // If the right or left operand is 0
        if (isConst(instr.uses[0], 0) || isConst(instr.uses[1], 0))
        {
            // Replace the instruction by 0
            replByVal(ConstValue.getConst(0, instr.type));

            // A change was made
            return true;
        }

        // If this is a reference tag bit extraction
        if (instr.uses[0] instanceof ConstValue &&
            instr.uses[1] instanceof ConstValue &&
            instr.uses[0].type === IRType.box &&
            !instr.uses[0].isBoxInt(params) &&
            instr.uses[1].type === IRType.pint && 
            instr.uses[1].value === TAG_REF_MASK)
        {
            replByVal(
                ConstValue.getConst(
                    instr.uses[0].getTagBits(params),
                    IRType.pint
                )
            );
        }

        // If this is a reference tag bit extraction
        if (
            instr.uses[0] instanceof ConstValue &&
            instr.uses[1] instanceof ConstValue &&
            instr.uses[1].type === IRType.box &&
            !instr.uses[1].isBoxInt(params) &&
            instr.uses[0].type === IRType.pint && 
            instr.uses[0].value === TAG_REF_MASK)
        {
            replByVal(
                ConstValue.getConst(
                    instr.uses[1].getTagBits(params),
                    IRType.pint
                )
            );
        }

        // If this is an integer tag bit extraction
        if (
            instr.uses[0] instanceof ConstValue &&
            instr.uses[1] instanceof ConstValue &&
            instr.uses[0].type === IRType.box &&
            instr.uses[1].type === IRType.pint && 
            instr.uses[1].value === TAG_INT_MASK)
        {
            replByVal(
                ConstValue.getConst(
                    instr.uses[0].getTagBits(params) & TAG_INT_MASK,
                    IRType.pint
                )
            );
        }

        // If this is an integer tag bit extraction
        if (
            instr.uses[0] instanceof ConstValue &&
            instr.uses[1] instanceof ConstValue &&
            instr.uses[1].type === IRType.box &&
            instr.uses[0].type === IRType.pint && 
            instr.uses[0].value === TAG_INT_MASK)
        {
            replByVal(
                ConstValue.getConst(
                    instr.uses[1].getTagBits(params) & TAG_INT_MASK,
                    IRType.pint
                )
            );
        }
    }

    // If this is an integer cast instruction
    if (instr instanceof ICastInstr)
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

