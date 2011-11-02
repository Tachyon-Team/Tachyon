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
Class hierarchy for High-level Intermediate Representation (HIR) instructions

@author
Maxime Chevalier-Boisvert
*/

//=============================================================================
//
// HIR implementation core
//
//=============================================================================

/**
@class Base class for HIR instructions
@augments CallInstr
*/
var HIRInstr = function ()
{
};
HIRInstr.prototype = new CallInstr();

/**
Create an HIR instruction constructor
*/
function hirInstrMaker(
    instrName,
    numInputs,
    mayThrow,
    proto
)
{
    minInputs = (typeof numInputs === 'number')? numInputs:numInputs[0];
    maxInputs = (typeof numInputs === 'number')? numInputs:numInputs[1];

    var InstrCtor = instrMaker(
        instrName,
        function (typeParams, inputVals, branchTargets)
        {
            instrMaker.validNumInputs(inputVals, minInputs, maxInputs);
            instrMaker.allValsBoxed(inputVals);

            if (mayThrow)
                instrMaker.validNumBranches(branchTargets, 0, 2);
            
            this.type = IRType.box;
        },
        mayThrow? ['continue', 'throw']:undefined,
        proto? proto:new HIRInstr()
    );

    return InstrCtor;
};

//=============================================================================
//
// Arithmetic operators
//
//=============================================================================

/**
@class Base class for JS arithmetic instructions
@augments HIRInstr
*/
var JSArithInstr = function ()
{
};
JSArithInstr.prototype = new HIRInstr();

/**
@class JS addition instruction
@augments JSArithInstr
*/
var JSAddInstr = hirInstrMaker(
    'js_add',
    2,
    true,
    new JSArithInstr()
);

/**
@class JS subtraction instruction
@augments JSArithInstr
*/
var JSSubInstr = hirInstrMaker(
    'js_sub',
    2,
    true,
    new JSArithInstr()
);

/**
@class JS multiplication instruction
@augments JSArithInstr
*/
var JSMulInstr = hirInstrMaker(
    'js_mul',
    2,
    true,
    new JSArithInstr()
);

/**
@class JS division instruction
@augments JSArithInstr
*/
var JSDivInstr = hirInstrMaker(
    'js_div',
    2,
    true,
    new JSArithInstr()
);

/**
@class JS modulo instruction
@augments JSArithInstr
*/
var JSModInstr = hirInstrMaker(
    'js_mod',
    2,
    true,
    new JSArithInstr()
);

//=============================================================================
//
// Bitwise operators
//
//=============================================================================

/**
@class Base class for JS bitwise instructions
@augments HIRInstr
*/
var JSBitOpInstr = function ()
{
};
JSBitOpInstr.prototype = new HIRInstr();

/**
@class JS bitwise NOT instruction
@augments JSArithInstr
*/
var JSNotInstr = hirInstrMaker(
    'js_not',
    1,
    true,
    new JSBitOpInstr()
);

/**
@class JS bitwise AND instruction
@augments JSArithInstr
*/
var JSAndInstr = hirInstrMaker(
    'js_and',
    2,
    true,
    new JSBitOpInstr()
);

/**
@class JS bitwise OR instruction
@augments JSArithInstr
*/
var JSOrInstr = hirInstrMaker(
    'js_or',
    2,
    true,
    new JSBitOpInstr()
);

/**
@class JS bitwise XOR instruction
@augments JSArithInstr
*/
var JSXorInstr = hirInstrMaker(
    'js_xor',
    2,
    true,
    new JSBitOpInstr()
);

/**
@class JS left shift instruction
@augments JSLsftInstr
*/
var JSLsftInstr = hirInstrMaker(
    'js_lsft',
    2,
    true,
    new JSBitOpInstr()
);

/**
@class JS right shift instruction
@augments JSLsftInstr
*/
var JSRsftInstr = hirInstrMaker(
    'js_rsft',
    2,
    true,
    new JSBitOpInstr()
);

/**
@class JS unsigned right shift instruction
@augments JSLsftInstr
*/
var JSUrsftInstr = hirInstrMaker(
    'js_ursft',
    2,
    true,
    new JSBitOpInstr()
);

//=============================================================================
//
// Comparison operators
//
//=============================================================================

/**
@class Base class for JS comparison instructions
@augments HIRInstr
*/
var JSCompInstr = function ()
{
};
JSCompInstr.prototype = new HIRInstr();

/**
@class JS less-than comparison instruction
@augments JSCompInstr
*/
var JSLtInstr = hirInstrMaker(
    'js_lt',
    2,
    true,
    new JSCompInstr()
);

/**
@class HIR less-than or equal comparison instruction
@augments JSCompInstr
*/
var JSLeInstr = hirInstrMaker(
    'js_le',
    2,
    true,
    new JSCompInstr()
);

/**
@class JS greater-than comparison instruction
@augments JSCompInstr
*/
var JSGtInstr = hirInstrMaker(
    'js_gt',
    2,
    true,
    new JSCompInstr()
);

/**
@class JS greater-than or equal comparison instruction
@augments JSCompInstr
*/
var JSGeInstr = hirInstrMaker(
    'js_ge',
    2,
    true,
    new JSCompInstr()
);

/**
@class JS equality comparison instruction
@augments JSCompInstr
*/
var JSEqInstr = hirInstrMaker(
    'js_eq',
    2,
    true,
    new JSCompInstr()
);

/**
@class JS inequality comparison instruction
@augments JSCompInstr
*/
var JSNeInstr = hirInstrMaker(
    'js_ne',
    2,
    true,
    new JSCompInstr()
);

/**
@class JS strict equality comparison instruction
@augments JSCompInstr
*/
var JSSeInstr = hirInstrMaker(
    'js_se',
    2,
    true,
    new JSCompInstr()
);

/**
@class JS strict inequality comparison instruction
@augments HIRInstr
*/
var JSNsInstr = hirInstrMaker(
    'js_ns',
    2,
    true,
    new JSCompInstr()
);

//=============================================================================
//
// Object/property access operators
//
//=============================================================================

/**
@class Get the global object reference
@augments HIRInstr
*/
var GetGlobalInstr = hirInstrMaker(
    'js_get_global',
    0,
    true
);

/**
@class Property read instruction
@augments HIRInstr
*/
var GetPropInstr = hirInstrMaker(
    'js_get_prop',
    2,
    true
);

/**
@class Property write instruction
@augments HIRInstr
*/
var PutPropInstr = hirInstrMaker(
    'js_put_prop',
    3,
    true
);

/**
@class JavaScript property deletion instruction
@augments HIRInstr
*/
var DelPropInstr = hirInstrMaker(
    'js_del_prop',
    2,
    true
);

/**
@class JavaScript typeof instruction
@augments HIRInstr
*/
var TypeOfInstr = hirInstrMaker(
    'js_typeof',
    1,
    true
);

/**
@class JavaScript instanceof instruction
@augments HIRInstr
*/
var InstOfInstr = hirInstrMaker(
    'js_instof',
    2,
    true
);

/**
@class JavaScript instanceof instruction
@augments HIRInstr
*/
var InInstr = hirInstrMaker(
    'js_in',
    2,
    true
);

//=============================================================================
//
// Call/new instructions
//
//=============================================================================

/**
@class JavaScript function call instruction
@augments HIRInstr
*/
var JSCallInstr = hirInstrMaker(
    'js_call',
    [0,undefined],
    true
);

/**
@class JavaScript new operator instruction
@augments HIRInstr
*/
var JSNewInstr = hirInstrMaker(
    'js_new',
    [0,undefined],
    true
);

// TODO: GetGlobal?

// TODO: CallGlobal?
// Put the lookup and assertions in there




