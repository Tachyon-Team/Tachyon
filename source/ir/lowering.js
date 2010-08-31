/**
@fileOverview
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: inlining specialization

// TODO: move to relevant file, object model implementation
ConstValue.regNamedConst(
    'OBJ_PROTO_PTR_OFFSET',
    4,
    IRType.INT32
);
ConstValue.regNamedConst(
    'OBJ_HASH_PTR_OFFSET',
    ConstValue.getNamedConst('OBJ_PROTO_PTR_OFFSET').value + IRType.OBJPTR.size,
    IRType.INT32
);
ConstValue.regNamedConst(
    'OBJ_HASH_SIZE_OFFSET',
    ConstValue.getNamedConst('OBJ_HASH_PTR_OFFSET').value + IRType.OBJPTR.size,
    IRType.INT32
);
ConstValue.regNamedConst(
    'OBJ_HASH_ENTRY_SIZE',
    16,
    IRType.INT32
);
ConstValue.regNamedConst(
    'OBJ_HASH_KEY_SIZE',
    8,
    IRType.INT32
);
ConstValue.regNamedConst(
    'OBJ_HASH_EMPTY_KEY',
    0,
    IRType.BOXED
);

/**
Perform IR lowering on a function and its subfunctions
*/
function lowerIRFunc(irFunc)
{
    // For each function in the IR
    var funcList = irFunc.getChildrenList();
    for (var i = 0; i < funcList.length; ++i)
    {
        var func = funcList[i];

        // Perform lowering on the function's CFG
        lowerIRCFG(func.virginCFG);
    }
}

/**
Perform IR lowering on a control-flow graph
*/
function lowerIRCFG(cfg)
{
    // For each instruction in the CFG
    for (var itr = cfg.getInstrItr(); itr.valid(); itr.next())
    {
        var instr = itr.get();

        // Test if some instruction uses are boxed values
        var usesBoxed = false
        for (var i = 0; i < instr.uses.length; ++i)
            if (instr.uses[i].type === IRType.BOXED)
                usesBoxed = true;

        // If this is an untyped if instruction
        if (usesBoxed && instr instanceof IfInstr)
        {
            // Create a boolean conversion instruction
            var toBoolInstr = new ToBoolInstr(instr.uses[0]);
            
            // Replace the if instruction by a typed if
            var ifBoolInstr = new IfInstr([toBoolInstr].concat(instr.targets));
            cfg.replInstr(itr, ifBoolInstr);

            // Add the instruction before the if
            cfg.addInstr(itr, toBoolInstr);

            // Move past the new instruction
            itr.next();
        }

        // If this instruction should be translated into a handler call
        else if (usesBoxed && handlerMap[instr.mnemonic])
        {
            var handler = handlerMap[instr.mnemonic];

            // Create a call instruction            
            var callInstr = new CallHandlerInstr(
                [handler].concat(instr.uses).concat(instr.targets)
            );
            
            // Replace the instruction by the call
            cfg.replInstr(itr, callInstr);
        }
    }
}

/**
Map of instruction names to handlers
*/
var handlerMap = {}

/**
Compile the handler source code to enable IR lowering
*/
function compHandlers()
{
    // Parse the handler source code
    var ast = parse_src_file('ir/handlers.js'); 
    var ir = unitToIR(ast, true);

    //print(ir);

    // For each function in the IR
    var funcList = ir.getChildrenList();
    for (var i = 0; i < funcList.length; ++i)
    {
        var func = funcList[i];

        // Add the function to the handler map
        handlerMap[func.funcName] = func;
    }

    // For each handler
    for (var handlerName in handlerMap)
    {
        var func = handlerMap[handlerName];

        // TODO: PROBLEM, if a handler calls another handler, can't
        // Directly inline it unless its lowering is complete

        // Need to do lowering of handlers on demand?
        // - Not necessarily, do inlining/optimization in later pass

        // TODO: PROBLEM, what about other functions that are called, to be inlined?
        // eg: IsBoxInt
        // Could be considered a handler call?
        // Could define the parsed functions as constants?
        // - Requires separate file for all these helper functions
        // - Should really just be other handlers

        // Don't really want handlers to do a global function lookup...
        // Could only look among other handlers when handlers make a global
        // Function call???
        // - Unintuitive and annoying
        // - Just make special instructions when needed!
        //   - More consistent, already have annotated/verified type info

        // Perform IR lowering on the handler
        lowerIRFunc(func);

        // Validate the resulting handler code
        func.validate();

        print(func);
    }
}

// Compile the handlers
compHandlers();

