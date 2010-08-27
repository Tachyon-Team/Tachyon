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
    add_ovf     : AddOvfInstr,
    sub_ovf     : SubOvfInstr,
    mul_ovf     : MulOvfInstr    
}

