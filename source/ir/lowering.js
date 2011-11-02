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

//=============================================================================
//
// HIR function lowering code
//
//=============================================================================

/**
Perform IR lowering on a function and its subfunctions
*/
function lowerIRFunc(irFunc, params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    if (params.printHIR === true)
        print(irFunc.toString());

    // For each function in the IR
    var funcList = irFunc.getChildrenList();
    for (var i = 0; i < funcList.length; ++i)
    {
        var func = funcList[i];

        //print('calling lowerIRCFG for "' + func.funcName + '"');

        // Perform lowering on the function's CFG
        lowerIRCFG(func.virginCFG, params);

        //print('back from lowerIRCFG for "' + func.funcName + '"');
    }

    //print('lowerIRFunc done for "' + irFunc.funcName + '"');

    if (params.printLIR === true)
        print(irFunc.toString());
}

/**
Perform IR lowering on a control-flow graph
*/
function lowerIRCFG(cfg, params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    measurePerformance(
        "inlining/transform",
        function ()
        {
            // For each instruction in the CFG
            for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
            {
                var instr = itr.get();

                // If this is a load or a store instruction on a boxed value
                if ((instr instanceof LoadInstr || instr instanceof StoreInstr) && 
                    instr.uses[0].type === IRType.box)
                {
                    // Create an unboxing operation
                    var unboxVal = new CallFuncInstr(
                        [
                            params.staticEnv.getBinding('unboxRef'),
                            IRConst.getConst(undefined),
                            IRConst.getConst(undefined),
                            instr.uses[0]
                        ]
                    );

                    var instrConstr = (instr instanceof LoadInstr)? LoadInstr:StoreInstr;

                    // Replace the load/store instruction
                    cfg.replInstr(
                        itr, 
                        new instrConstr(
                            [instr.typeParams[0], unboxVal].concat(instr.uses.slice(1))
                        )
                    );

                    // Add the unbox instruction before the load
                    cfg.addInstr(itr, unboxVal);

                    var instr = itr.get();
                }

                // If this is an HIR instruction
                if (instr instanceof HIRInstr)
                {
                    //print('lowering HIR instruction: ' + instr);

                    // Call the lowering function to get the
                    // primitive to be called
                    var primFunc = instr.lower(params);

                    // Create the primitive call
                    var callInstr = new CallFuncInstr(
                        [
                            primFunc,
                            IRConst.getConst(undefined),
                            IRConst.getConst(undefined),
                        ].concat(instr.uses).concat(instr.targets)
                    );

                    // Replace the HIR instruction by the primitive call
                    cfg.replInstr(itr, callInstr);

                    var instr = itr.get();
                }

                // If this is a function call to a known function
                if (instr instanceof CallFuncInstr &&
                    instr.getCallee() instanceof IRFunction)
                {
                    var calleeFunc = instr.getCallee();

                    // If the callee is marked inline and is inlinable
                    if (calleeFunc.inline && isInlinable(calleeFunc))
                    {
                        /*
                        print(
                            'inlining: ' + calleeFunc.funcName + ' in ' + 
                            cfg.ownerFunc.funcName
                        );
                        */

                        // Inline the call
                        inlineCall(instr, calleeFunc);
                    }
                }
            }
        }
    );

    measurePerformance(
        "opt patterns pass 1",
        function ()
        {
            //print('*** applying patterns ***');

            // Apply peephole optimization patterns to the CFG
            applyPatternsCFG(cfg, params);

            //print('*** validating after patterns ***');

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    //print(cfg.ownerFunc);

    measurePerformance(
        "const prop",
        function ()
        {
            //print('*** const prop ***');

            // Perform constant propagation on the CFG
            constProp(cfg, params);

            //print(cfg.ownerFunc);

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    measurePerformance(
        "opt patterns pass 2",
        function ()
        {
            // Apply peephole optimization patterns to the CFG
            applyPatternsCFG(cfg, params);

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    measurePerformance(
        "comm elim",
        function ()
        {
            //print('*** comm elim ***');

            // Perform common subexpression elimination on the CFG
            commElim(cfg);

            //print('*** done ***');

            // Validate the CFG
            if (DEBUG)
                cfg.validate();
        }
    );

    measurePerformance(
        "r/w analysis",
        function ()
        {
            // Assume that the function does not read or write from/to memory
            cfg.ownerFunc.writesMem = false;
            cfg.ownerFunc.readsMem = false;

            // For each instructon in the CFG
            for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
            {
                var instr = itr.get();

                // If any instruction reads or writes from memory, annotate the
                // function as reading or writing memory
                if (instr.writesMem())
                {
                    //print('******' + cfg.ownerFunc.funcName + ' writes mem: ' + instr);
                    cfg.ownerFunc.writesMem = true;
                }
                if (instr.readsMem())
                {
                    //print('******' + cfg.ownerFunc.funcName + ' reads mem: ' + instr);
                    cfg.ownerFunc.readsMem = true;
                }
            }

            //if (!cfg.ownerFunc.writesMem)
            //    print('############ DOES NOT WRITE MEM: ' + cfg.ownerFunc.funcName);
        }
    );

    //print('*** lowering done ***');
}

//=============================================================================
//
// HIR instruction lowering functions
//
//=============================================================================

/**
Genering lowering function generator for HIR instructions.
Directly inlines a primitive function.
*/
function genLowerFunc(primName)
{
    return function (compParams)
    {
        // Return the primitive to be inlined
        return compParams.staticEnv.getBinding(primName);
    }
}

// For now, these HIR instructions are directly replaced by a primitive call
JSAddInstr.prototype.lower = genLowerFunc('add');
JSSubInstr.prototype.lower = genLowerFunc('sub');
JSMulInstr.prototype.lower = genLowerFunc('mul');
JSDivInstr.prototype.lower = genLowerFunc('div');
JSModInstr.prototype.lower = genLowerFunc('mod');
JSNotInstr.prototype.lower = genLowerFunc('not');
JSAndInstr.prototype.lower = genLowerFunc('and');
JSOrInstr.prototype.lower = genLowerFunc('or');
JSXorInstr.prototype.lower = genLowerFunc('xor');
JSLsftInstr.prototype.lower = genLowerFunc('lsft');
JSRsftInstr.prototype.lower = genLowerFunc('rsft');
JSUrsftInstr.prototype.lower = genLowerFunc('ursft');
JSLtInstr.prototype.lower = genLowerFunc('lt');
JSLeInstr.prototype.lower = genLowerFunc('le');
JSGtInstr.prototype.lower = genLowerFunc('gt');
JSGeInstr.prototype.lower = genLowerFunc('ge');
JSSeInstr.prototype.lower = genLowerFunc('se');
JSNsInstr.prototype.lower = genLowerFunc('ns');
JSEqInstr.prototype.lower = genLowerFunc('eq');
JSNeInstr.prototype.lower = genLowerFunc('ne');
GetPropInstr.prototype.lower = genLowerFunc('getPropVal');
PutPropInstr.prototype.lower = genLowerFunc('putPropVal');
DelPropInstr.prototype.lower = genLowerFunc('delPropVal');
TypeOfInstr.prototype.lower = genLowerFunc('typeOf');
InstOfInstr.prototype.lower = genLowerFunc('instanceOf');
InInstr.prototype.lower = genLowerFunc('inOp');

// TODO
// TODO: lowering for call, constructor?
// TODO
// Need to dynamically generate code







