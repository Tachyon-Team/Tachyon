/**
@fileOverview
Compiler target configuration.

@author
Maxime Chevalier-Boisvert, Erick Lavoie

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
@class Description of target architecture for compilation
*/
function Target(cfgObj)
{
    assert (
        cfgObj.endian === 'little' || cfgObj.endian === 'big',
        'invalid endian specified'
    );

    assert (
        cfgObj.ptrSizeBits === 32 || cfgObj.ptrSizeBits === 64,
        'invalid pointer size specified'
    );

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
    this.ptrSizeBytes = this.ptrSizeBits >> 3;    
}
