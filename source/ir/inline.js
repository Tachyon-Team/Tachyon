/**
@fileOverview
Implementation of inline IR

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: function to create instructions on demand with some fixed parameters?
// - is this necessary?

// TODO: way of identifying instructions with binary branches?

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

    // Arithmetic operations w/o overflow handling
    iadd        : IAddInstr,
    isub        : ISubInstr,
    imul        : IMulInstr,
    idiv        : IDivInstr,
    imod        : IModInstr,
    fadd        : FAddInstr,
    fsub        : FSubInstr,
    fmul        : FMulInstr,
    fdiv        : FDivInstr,

    // Bitwise integer operations
    inot        : INotInstr,
    iand        : IAndInstr,
    ior         : IOrInstr,
    ixor        : IXorInstr,
    ilsft       : ILsftInstr,
    irsft       : IRsftInstr,
    iursft      : IUrsftInstr,

    // Comparison instructions
    ilt         : ILtInstr,
    ilte        : ILteInstr,
    igt         : IGtInstr,
    igte        : IGteInstr,
    ieq         : IEqInstr,
    ineq        : INeqInstr,
    flt         : FLtInstr,
    flte        : FLteInstr,
    fgt         : FGtInstr,
    fgte        : FGteInstr,
    feq         : FEqInstr,
    fneq        : FNeqInstr,

    // Branch instructions
    ifb         : IfBoolInstr,
    add_ovf     : IAddOvfInstr,
    sub_ovf     : ISubOvfInstr,
    mul_ovf     : IMulOvfInstr    
}

