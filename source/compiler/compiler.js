/**
@fileOverview
High-level compiler interface.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace Glue code to tie the frontend and the backend together. */
var compiler = {};

/** 
Function meant to be assigned to the 'execute' field 
of a runtime property of an IRFunction. It executes
the machine code block associated to the IRFunction
with the arguments and the runtime context supplied.
*/
compiler.execute = function (args, runtime) 
{
    return execMachineCodeBlock(this.mcb);    
};

/** 
Function meant to be assigned to the 'free' field 
of a runtime property of an IRFunction. 
It frees the machine code block associated to the IRFunction
and clear the runtime.mcb field
*/
compiler.free = function () 
{
    freeMachineCodeBlock(this.mcb);    
    this.mcb = null;
};

/** 
Function meant to be assigned to the 'link' field 
of a linking property of an IRFunction. 
It links the current machine code block in the runtime
property to its dependencies.
*/
compiler.link = function () 
{
    this.rt.mcb.link();
    this.linked = true;
};

/** 
Compiles an IRFunction and assigns linking and runtime
information to it. The runtime.mcb properties on the 
IRFunction should be freed once it is no longer used.
*/
function compileIR(ir, params) 
{
    assert (params instanceof CompParams);

    ir.linking.linked = false;
    ir.linking.link = compiler.link;

    var mcb = backend.compileIRToMCB(ir, params);
    ir.runtime.mcb = mcb;
    ir.runtime.execute = compiler.execute;
    ir.runtime.free = compiler.free;

    ir.linking.rt = ir.runtime;
};

/** 
    Link the IRFunction.
*/
function linkIR(ir, params) 
{
    ir.linking.link();
};

/**
Compile an ast to compiled IR
*/
function compileAst(ast, params)
{
    assert (
        params instanceof CompParams,
        'compilation parameters expected'
    );

    var ir = unitToIR(ast, params);
    lowerIRFunc(ir, params);
    compileIR(ir, params);
    linkIR(ir, params);

    // Return the compiled IR function
    return ir;
};

/**
Compile a source string to compiled IR
*/
function compileSrcString(str, params)
{
    var ast = parse_src_str(str, params);

    return compileAst(ast, params);
}

/**
Compile a source file to compiled IR
*/
function compileSrcFile(fileName, params)
{
    var ast = parse_src_file(fileName, params);

    return compileAst(ast, params);
}

/**
Creates a compiled fonction from an IRFunction with
linking and runtime information.
*/
/*
function createJSFuncFromCompiledIR(ir)
{
    var f = function () 
    { 
        return ir.runtime.execute();
    };
    f.free = ir.runtime.free;

    return f;
};
*/

/**
Compile a file to an optimized callable function. The function should be freed 
after last usage by calling the 'free' method on the function. Ex:

    var f = compileFileToJSFunc(...);
    print(f());
    f.free();

@filename String containing path to the source file
*/
/*
function compileFileToJSFunc(fileName, params) 
{
    assert (
        params instanceof CompParams,
        'compilation parameters expected'
    );

    var ir = compileSrcFile(fileName, params); 

    return createJSFuncFromCompiledIR(ir, params);
};
*/
