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
function initConfig(options)
{
    var is64bit = options['x86_64'] !== undefined;

    config.verbosity = options['v'];
    if (config.verbosity === undefined)
        config.verbosity = log.ERROR;

    config.profile = options['profile']; // Supported values : "auto","manual"
    if (config.profile !== undefined)
    {
        if (config.profile === true)
            config.profile = "auto";
        else if (config.profile !== "auto" && config.profile !== "manual")
            error("Unknown profiling mode: " + config.profile)
    }

    log.trace('Initializing config (' + (is64bit? '64':'32') + 'bit)');

    // Determine the heap size
    var heapSize;
    if (is64bit)
    {
        // Tachyon 64bit => 32GB
        heapSize = Math.pow(2,35)-100;
    }
    else
    {        
        if (RUNNING_IN_TACHYON)
            heapSize = Math.pow(2, 28);     // Tachyon/Tachyon 32-bit => 256MB
        else
            heapSize = 1 * Math.pow(2,30);  // Tachyon/V8 32bit => 1GB
    }

    /**
    Compilation parameters for the currently running Tachyon VM.
    The tachyon code has special privileges.
    */
    config.hostParams = new CompParams({
        target          : is64bit? Target.x86_64 : Target.x86_32,
        tachyonSrc      : true,
        debug           : true,
        parserWarnings  : true,
        debugTrace      : false,
        heapSize        : heapSize,
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
        target          : is64bit ? Target.x86_64 : Target.x86_32,
        tachyonSrc      : true,
        debug           : true,
        parserWarnings  : true,
        debugTrace      : false,
        heapSize        : heapSize,
        staticEnv       : new StaticEnv()
    });

    // TODO: object representation choice
    // TODO: GC parameters
}

