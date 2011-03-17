/**
@fileOverview
Configuration file for the Tachyon compiler.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Constant to indicate we are running in Tachyon. This will be false
under d8, true under Tachyon. Note that when running under Tachyon,
this becomes a static binding, and does not require global variable
access.
*/
const RUNNING_IN_TACHYON = false;

/**
Configuration object for the compiler
*/
var config = {};

/**
Initialize the Tachyon configuration
*/
function initConfig(is64bitMode)
{
    if (is64bitMode === undefined)
    {
        is64bitMode = false;
    }

    /**
    Compilation parameters for the currently running Tachyon VM.
    The tachyon code has special privileges.
    */
    config.hostParams = new CompParams({
        target          : is64bitMode ? Target.x86_64 : Target.x86_32,
        tachyonSrc      : true,
        debug           : true,
        parserWarnings  : true,
        debugTrace      : false,
        staticEnv       : new StaticEnv()
    });

    /**
    Compilation parameters for the client code tachyon compiles and runs.
    The parameters are the same as for host code, but the client code has
    no special privileges.
    */
    config.clientParams = Object.create(config.hostParams);
    config.clientParams.tachyonSrc = false;
    config.clientParams.parserWarnings = false;

    /**
    Compilation parameters for debugging client code.
    */
    config.clientDebugParams = Object.create(config.clientParams);
    config.clientDebugParams.debug = true;
    config.clientDebugParams.debugTrace = true;

    /**
    Compilation parameters used to bootstrap Tachyon
    */
    config.bootParams = new CompParams({
        target          : is64bitMode ? Target.x86_64 : Target.x86_32,
        tachyonSrc      : true,
        debug           : true,
        parserWarnings  : true,
        debugTrace      : false,
        staticEnv       : new StaticEnv()
    });

    // Set the running in Tachyon constant for the host config
    config.hostParams.staticEnv.regBinding(
        'RUNNING_IN_TACHYON', 
        ConstValue.getConst(true)
    );

    // Set the running in Tachyon constant for the bootstrap config
    config.bootParams.staticEnv.regBinding(
        'RUNNING_IN_TACHYON', 
        ConstValue.getConst(true)
    );

    // TODO: object representation choice
    // TODO: GC parameters
}

