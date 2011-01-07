/**
@fileOverview
Definition of compilation parameters.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/*
TODO: Should we include runtime, object representation here?
*/

/**
@class Description/grouping of compilation parameters required to compile a
       unit of source code.
*/
function CompParams(cfgObj)
{
    assert (cfgObj.target instanceof Target);

    assert (cfgObj.tachyonSrc === true || cfgObj.tachyonSrc === false);

    // TODO
    //assert (cfgObj.staticEnv instanceof StaticEnv);

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
    Object layout code auto-generated for this platform
    @field
    */
    this.layoutSrc = '';

    /**
    List of IR function objects for primitive functions
    @field
    */
    this.primIR = [];
}

