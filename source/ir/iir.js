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
var iir =
{
    // Memory management
    load        : LoadInstr,
    store       : StoreInstr,

    // Run-time context access
    get_ctx     : GetCtxInstr,
    set_ctx     : SetCtxInstr,

    // Type conversion
    icast       : ICastInstr,
    itof        : IToFPInstr,
    ftoi        : FPToIInstr,

    // Arithmetic instructions
    add         : AddInstr,
    sub         : SubInstr,
    mul         : MulInstr,
    div         : DivInstr,
    mod         : ModInstr,

    // Arithmetic instructions with overflow handling
    add_ovf     : AddOvfInstr,
    sub_ovf     : SubOvfInstr,
    mul_ovf     : MulOvfInstr,

    // Bitwise instructions
    not         : NotInstr,
    and         : AndInstr,
    or          : OrInstr,
    xor         : XorInstr,
    lsft        : LsftInstr,
    rsft        : RsftInstr,
    ursft       : UrsftInstr,

    // Comparison instructions
    lt          : LtInstr,
    le          : LeInstr,
    gt          : GtInstr,
    ge          : GeInstr,
    eq          : EqInstr,
    ne          : NeInstr,

    // Function call instruction
    call        : CallFuncInstr,

    // FFI call instruction
    call_ffi    : CallFFIInstr
};
