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
Generate a specialized primitive function based on specialization parameters.
*/
function genSpecPrim(instrClass, genFunc, specParams, compParams)
{
    assert (
        typeof genFunc === 'function',
        'expected generation function'
    );

    assert (
        specParams instanceof Array,
        'expected specialization parameters'
    );

    assert (
        compParams instanceof CompParams,
        'expected compilation parameters'
    );

    // Set the instruction class in the specialization parameters
    specParams.instrClass = instrClass;

    // If the runtime is not yet initialized
    if (compParams.initState < initState.FULL_RUNTIME)
    {
        // Mark the primitive as being an inline version
        specParams.inline = true;
    }

    //print('looking for func');

    // Try to find a specialized function with the said parameters
    var specFunc = compParams.specPrims.get(specParams);

    // If there is a cache hit for the parameters, return the match
    if (specFunc instanceof IRFunction)
        return specFunc;

    // Generate the source code for the specialized primitives
    var sourceStr = genFunc.apply(null, specParams);

    // Compile the source string to an IR function
    var ast = parse_src_str(sourceStr, compParams);
    var ir = unitToIR(ast, compParams);
    lowerIRFunc(ir, compParams);

    // Get the compiled primitive function
    var specFunc = ir.childFuncs[0];

    // If the runtime is not yet initialized
    if (compParams.initState < initState.FULL_RUNTIME)
    {
        // Set the inline flag on the IR function
        specFunc.inline = true;
    }
    else
    {    
        // Compile and link the IR function
        compileIR(specFunc, compParams);
        linkIR(specFunc, compParams);
    }

    // Cache the compiled primitive
    compParams.specPrims.set(specParams, specFunc);

    // Return the new function
    return specFunc;
}

/**
Hash function for the memoization of specialized primitives
*/
function specHashFunc(p)
{
    if (typeof p === 'object')
    {
        var hashCode = 0;

        for (k in p)
            hashCode += specHashFunc(p[k]);

        return hashCode;
    }

    else if (typeof p === 'function')
    {
        return 1;
    }

    return defHashFunc(p);
}

/**
Equality function for the memoization of specialized primitives
*/
function specEqualFunc(p1, p2)
{
    if (p1 === p2)
    {
        return true;
    }

    else if (typeof p1 === 'object' && typeof p2 === 'object')
    {
        if (Object.getPrototypeOf(p1) !== Object.getPrototypeOf(p2))
            return false;

        var keys = {};
        for (k in p1) keys[k] = true;
        for (k in p2) keys[k] = true;

        for (k in keys)
        {
            if (specEqualFunc(p1[k], p2[k]) === false)
                return false;
        }

        return true;
    }

    return false;
}

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
GlobalObjInstr.prototype.lower = genLowerFunc('getGlobalObj');
GetGlobalInstr.prototype.lower = genLowerFunc('getGlobal');
BlankObjInstr.prototype.lower = genLowerFunc('blankObject');
GetPropInstr.prototype.lower = genLowerFunc('getPropVal');
PutPropInstr.prototype.lower = genLowerFunc('putPropVal');
DelPropInstr.prototype.lower = genLowerFunc('delPropVal');
HasPropInstr.prototype.lower = genLowerFunc('hasProp');
TypeOfInstr.prototype.lower = genLowerFunc('typeOf');
InstOfInstr.prototype.lower = genLowerFunc('instanceOf');
InInstr.prototype.lower = genLowerFunc('inOp');

// JavaScript function call
JSCallInstr.prototype.lower = function (params)
{
    // Get the number of function arguments
    var numArgs = this.uses.length - 2;

    // Generator function for the constructor call
    function genFunc(numArgs)
    {
        //print('GENERATOR FUNCTION CALLED, numArgs = ' + numArgs);

        // Concatenate the argument names
        var args = '';
        for (var i = 0; i < numArgs; ++i)
            args += ',a' + i;

        var callStr = '                                 \
        function jsCall<numArgs>(func, thisVal <args>)  \
        {                                               \
            "tachyon:static";                           \
            "tachyon:noglobal";                         \
                                                        \
            var ctx = iir.get_ctx();                    \
                                                        \
            if (boxIsFunc(func) === false)              \
            {                                           \
                throw makeError(                        \
                    get_ctx_typeerror(ctx),             \
                    "callee is not a function"          \
                );                                      \
            }                                           \
                                                        \
            /* Do the function call */                  \
            var retVal = iir.call(                      \
                get_clos_funcptr(func),                 \
                func,                                   \
                thisVal                                 \
                <args>                                  \
            );                                          \
                                                        \
            return retVal;                              \
        }';

        sourceStr = callStr;
        sourceStr = sourceStr.replace('<numArgs>', numArgs);
        sourceStr = sourceStr.replace('<args>', args);
        sourceStr = sourceStr.replace('<args>', args);

        //print(sourceStr);

        return sourceStr;
    }

    // Generate the primitive function
    return genSpecPrim(JSCallInstr, genFunc, [numArgs], config.hostParams);
}

// JavaScript new operator
JSNewInstr.prototype.lower = function (params)
{
    //print('JS NEW LOWERING');

    // Get the number of constructor arguments
    var numArgs = this.uses.length - 1;

    // Generator function for the constructor call
    function genFunc(numArgs)
    {
        //print('GENERATOR FUNCTION CALLED, numArgs = ' + numArgs);

        // Concatenate the argument names
        var args = '';
        for (var i = 0; i < numArgs; ++i)
            args += ',a' + i;

        var ctorStr = '                                 \
        function jsNew<numArgs>(ctor <args>)            \
        {                                               \
            "tachyon:static";                           \
            "tachyon:noglobal";                         \
                                                        \
            var ctx = iir.get_ctx();                    \
                                                        \
            var funcProto = ctor.prototype;             \
                                                        \
            var protoVal =                              \
                (boxIsObjExt(funcProto) === true)?      \
                funcProto:                              \
                get_ctx_objproto(ctx);                  \
                                                        \
            var newObj = newObject(protoVal);           \
                                                        \
            if (boxIsFunc(ctor) === false)              \
            {                                           \
                throw makeError(                        \
                    get_ctx_typeerror(ctx),             \
                    "constructor is not a function"     \
                );                                      \
            }                                           \
                                                        \
            /* Do the constructor call */               \
            var retVal = iir.call(                      \
                get_clos_funcptr(ctor),                 \
                ctor,                                   \
                newObj                                  \
                <args>                                  \
            );                                          \
                                                        \
            var objVal =                                \
                (boxIsObjExt(retVal) === true)?         \
                retVal:                                 \
                newObj;                                 \
                                                        \
            /* Return the newly created object */       \
            return objVal;                              \
        }';

        sourceStr = ctorStr;
        sourceStr = sourceStr.replace('<numArgs>', numArgs);
        sourceStr = sourceStr.replace('<args>', args);
        sourceStr = sourceStr.replace('<args>', args);

        //print(sourceStr);

        return sourceStr;
    }

    // Generate the primitive function
    return genSpecPrim(JSNewInstr, genFunc, [numArgs], config.hostParams);
}

