/**
@fileOverview
Definition of compilation parameters.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
@class Description/grouping of compilation parameters required to compile a
       unit of source code.
*/
function CompParams(cfgObj)
{
    assert (
        cfgObj.target instanceof Target,
        'invalid target object'
    );

    assert (
        cfgObj.tachyonSrc === true || cfgObj.tachyonSrc === false,
        'invalid tachyon source flag'
    );

    assert (
        cfgObj.debug === true || cfgObj.debug === false,
        'invalid debug flag'
    );

    assert (
        cfgObj.parserWarnings === true || cfgObj.parserWarnings === false,
        'invalid parser warnings flag'
    );

    assert (
        cfgObj.debugTrace === true || cfgObj.debugTrace === false,
        'invalid debug trace flag'
    );

    assert (
        cfgObj.staticEnv instanceof StaticEnv,
        'invalid static environment'
    );

    /**
    Target architecture
    @field
    */
    this.target = cfgObj.target;

    /**
    Flag indicating that we are compiling Tachyon source
    @field
    */
    this.tachyonSrc = cfgObj.tachyonSrc;

    /**
    Enable debug mode flag
    @field
    */
    this.debug = cfgObj.debug;

    /**
    Enable parser warnings flag
    @field
    */
    this.parserWarnings = cfgObj.parserWarnings;

    /**
    Enable generation of a debug trace
    @field
    */
    this.debugTrace = cfgObj.debugTrace;

    /**
    Static definitions to be used during compilation
    @field
    */
    this.staticEnv = cfgObj.staticEnv;

    /**
    Map of object memory layouts, by name
    @field
    */
    this.memLayouts = {};

    /**
    FFI functions used by compiler
    @field
    */
    this.ffiFuncs = {};

    /**
    Function to allocate string objects
    @field
    */
    this.getStrObj = null;

    /**
    Function used to print output during the compilation.
    By default this is the global print function.
    @field
    */
    this.print = print;

    /**
    Flag to print the HIR generated during the compilation
    @field
    */
    this.printHIR = false;

    /**
    Flag to print the LIR generated during the compilation
    @field
    */
    this.printLIR = false;

    /**
    Flag to print the register allocation information during the compilation
    @field
    */
    this.printRegAlloc = false;

    /**
    Flag to print the assembler code generated during the compilation
    @field
    */
    this.printASM = false;
}

