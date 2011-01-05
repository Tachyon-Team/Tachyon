/**
@fileOverview
Compiler target configuration.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/*
TODO: Should we include runtime, object representation here?
*/

/**
*/
function Target(cfgObj)
{
    assert (cfgObj.endian === 'little' || cfgObj.endian === 'big');

    assert (cfgObj.ptrSizeBits === 32 || cfgObj.ptrSizeBits === 64);

    assert (cfgObj.debug === true || cfgObj.debug === false);

    /**
    Back-end name    
    @field
    */
    this.backend = cfgObj.backend;

    /**
    Back-end specific configuration
    @field
    */
    this.backendCfg = cfgObj.backendCfg;   

    /**
    Endianness of the target platform
    @field
    */
    this.endian = cfgObj.endian;

    /**
    Pointer size of the target platform, in bits
    @field
    */
    this.ptrSizeBits = cfgObj.ptrSizeBits;

    /**
    Pointer size of the target platform, in bytes
    @field
    */
    this.ptrSizeBytes = this.ptrSizeBits / 8;    

    /**
    Debug mode flag
    @field
    */
    this.debug = cfgObj.debug;
}

/**
Debug, x86, 32-bit configuration
*/
Target.Debug_X86_32 = new Target({
    backend         : 'backendX86',
    backendCfg      : {/*back-end specific, reg alloc config, etc.*/},
    endian          : 'little',
    ptrSizeBits     : 32,
    debug           : true
});

