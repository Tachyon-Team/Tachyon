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
Initialize the Tachyon configuration
*/
function initConfig()
{

    // TODO: create target, put target inside config.params



    // TODO: create static env


    /**
    Current compilation parameters
    */
    config.params = new CompParams({
        target      : Target.Debug_X86_32,
        tachyonSrc  : true,
        staticEnv   : {}
    });

    /**
    Configuration for the run-time options
    */
    config.runtime = {};

    // TODO: object representation choice
    // TODO: GC parameters
}

