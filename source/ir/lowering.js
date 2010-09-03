/**
@fileOverview
Implementation of high-level IR lowering and specialization in preparation
for code generation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/*
// TODO
Perhaps a better option would be to enable direct "cross-linking" of
handler code. Enable handlers to call each other without defining IR
instructions?
- Requires special "cross-linking" pass
- Could be special flag to lowering pass
  - Replace global function calls by other handler calls, if available?
  - Potential problem, if coding GC code, not technically a handler?

Probably want all tachyon code to cross-link together?
- Problem: some functions will have non-boxed return types
  - This info is needed for cross-linking
  - Need to parse/translate the functions to know this
*/

// TODO: function input/output type annotation
// - Not all handlers have untyped in/out...
// - Functions should have input/output type
// - ArgValInstr should take type
// - Ret instruction type needs to be validated in IR gen

// TODO: auto static link functions with non-boxed in/out

// TODO: unify constants with compile-time-binding-resolution?
// - Compiler constants are the same for everybody, leave as now
// - Constants such as object prototype should be in "execution context"
//   - Context access instruction, get_ctx_val "", set_ctx_val ""
//   - No need to create before compilation?

/**
// TODO
Annotations:

static
inline
arg types
return type
*/

/*

Function to parse JavaScript file ASTs, get signature info
- Do we want to pre-create function objects too?
- Not strictly necessary, could link at lowering stage?
- Create object with static bindings
  - Needed during IR translation, possibly lowering
- Named constants could also be static bindings?
  - regStaticBinding(name, irvalue)

regBinding
getBinding(name)
- Can return func or other IR value
- If we get func, can set call output type from func type

Can have one object for tachyon static bindings
- Fit all of the tachyon static bindings in there

Generating a typed call, eg: to ThrowError function
getStaticFunc(funcName), gets function object
- Can then create call/construct as desired
- Requires function object to be available

When generating code for calls,
For tachyon code, always try to find matching static func

PROBLEM: if linking at lowering time, cannot turn getpropval into 
static link?
- Calls must be resolved, or marked as static during IR gen
- Can pre-create func obj during pre-parsing, reuse during IR gen if already created***

*/

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

// TODO: inlining specialization

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

