/**
@fileOverview
Configuration file for the Tachyon compiler.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Configuration object for the compiler
*/
var config = {};

/**
Variable to indicate we are running in Tachyon. This will be false
under d8, true under Tachyon.
*/
config.inTachyon = (function ()
{
    var iir = { add: function() { return 0; } };
    return (iir.add(1,2) === 3);
})();

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

    /**
    Configuration for the run-time options
    */
    config.runtime = {};

    // TODO: object representation choice
    // TODO: GC parameters
}

