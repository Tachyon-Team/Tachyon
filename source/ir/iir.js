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
Implementation of inline IR

@author
Maxime Chevalier-Boisvert
*/

/**
Function to create an if with comparison instruction
*/
function makeIfCmp(args, cmp)
{
    return new IfTestInstr(
        args[0],
        args[1],
        cmp,
        args[2],
        args[3]
    );
}

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

    // Run-time Function Call protocol
    get_num_args: GetNumArgsInstr,

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

    // If instructions with comparison   
    if_lt       : function (args) { return makeIfCmp(args, IfTestInstr.cmpOp.LT); },
    if_le       : function (args) { return makeIfCmp(args, IfTestInstr.cmpOp.LE); },
    if_gt       : function (args) { return makeIfCmp(args, IfTestInstr.cmpOp.GT); },
    if_ge       : function (args) { return makeIfCmp(args, IfTestInstr.cmpOp.GE); },
    if_eq       : function (args) { return makeIfCmp(args, IfTestInstr.cmpOp.EQ); },
    if_ne       : function (args) { return makeIfCmp(args, IfTestInstr.cmpOp.NE); },

    // Function call instruction
    call        : CallFuncInstr,

    // Function call with apply instruction
    call_apply  : CallApplyInstr,

    // FFI call instruction
    call_ffi    : CallFFIInstr
};

