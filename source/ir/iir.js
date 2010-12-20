/**
@fileOverview
Implementation of inline IR

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Pseudo-constructor for IIR constants
*/
function IIRConst(args)
{
    assert (
        args.length == 2,
        'IIR constant expected 2 arguments'
    );

    var constVal;

    // If the constant value is negative (0 - value)
    if (args[1] instanceof CallInstr && 
        args[1].uses[0].funcName === "sub" &&
        args[1].uses[args[1].uses.length - 2] instanceof ConstValue &&
        args[1].uses[args[1].uses.length - 2].type === IRType.box &&
        args[1].uses[args[1].uses.length - 2].value === 0
    )
    {
        constVal = -args[1].uses[args[1].uses.length - 1].value;
    }

    // Otherwise, if the value is a non-negative constant 
    else if (args[1] instanceof ConstValue)
    {
        constVal = args[1].value;
    }

    else
    {
        error(
            'IIR constant expects constant value as second argument: ' +
            '"' + args[1] + '"'
        );
    }

    return ConstValue.getConst(constVal, args[0]);
}

/**
Object containing IR instructions usable inline inside functions
*/
var iir =
{
    // Constants
    cst         : IIRConst,

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
    ne          : NeInstr
};
