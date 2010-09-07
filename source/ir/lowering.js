/**
@fileOverview
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: move to relevant file, object model implementation
staticEnv.regBinding(
    'OBJ_PROTO_PTR_OFFSET',
    ConstValue.getConst(
        4,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_PTR_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_PROTO_PTR_OFFSET').value + IRType.optr.size,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_SIZE_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_HASH_PTR_OFFSET').value + IRType.optr.size,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_ENTRY_SIZE',
    ConstValue.getConst(
        16,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_KEY_SIZE',
    ConstValue.getConst(
        8,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_EMPTY_KEY',
    ConstValue.getConst(
        0,
        IRType.box
    )
);

// TODO: inlining specialization
// - Just use the inline annotation!

// TODO: implement get_ctx_var
// - eliminate getNamedConst

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
            if (instr.uses[i].type === IRType.box)
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
            var callInstr = new CallFuncInstr(
                [
                    handler,
                    ConstValue.getConst(undefined) // TODO: get global obj from context?
                ]
                .concat(instr.uses)
                .concat(instr.targets)
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
Compile the primitives source code to enable IR lowering
*/
function compPrimitives()
{
    // Parse the handler source code
    var ast = parse_src_file('ir/primitives.js'); 

    // Parse static bindings in the unit
    staticEnv.parseUnit(ast);

    // Generate IR from the AST
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

        // Perform IR lowering on the handler
        lowerIRFunc(func);

        // Validate the resulting handler code
        func.validate();

        print(func);
    }
}

// Compile the primitives
compPrimitives()

