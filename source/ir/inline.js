/**
@fileOverview
Implementation of inline IR

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Object containing IR instructions usable inline inside functions
*/
iir =
{
    // Memory management
    load        : LoadInstr,
    store       : StoreInstr,

    // Type conversion
    unbox       : UnboxInstr,
    box         : BoxInstr,
    icast       : ICastInstr,
    itof        : IToFPInstr,
    ftoi        : FPToIInstr,

    // Branch instructions
    ifb         : IfBoolInstr,
    add_ovf     : IAddOvfInstr,
    sub_ovf     : ISubOvfInstr,
    mul_ovf     : IMulOvfInstr    
}

