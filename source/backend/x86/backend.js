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
x86 backend interface.

@author
Maxime Chevalier-Boisvert
*/

/**
@class x86 backend interface
@extends Backend
*/
x86.Backend = function (x86_64)
{
    assert (
        typeof x86_64 === 'boolean',
        'invalid x86_64 flag'
    );

    /**
    Flag for 64-bit mode
    */
    this.x86_64 = x86_64;

    /**
    Stack pointer register
    */
    this.spReg = x86_64? x86.spReg64:x86.spReg32;

    /**
    Context register
    */
    this.ctxReg = x86_64? x86.ctxReg64:x86.ctxReg32;
}
x86.Backend.prototype = new Backend();

/**
Generate machine code for an IR function
*/
x86.Backend.prototype.genCode = function (irFunc, params)
{
    assert (
        irFunc instanceof IRFunction,
        'expected IR function'
    );

    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    // Generate code for the child functions
    for (var i = 0; i < irFunc.childFuncs.length; ++i)
        this.genCode(irFunc.childFuncs[i], params);

    print('');
    print('compiling "' + irFunc.funcName + '"');

    // Get a reference to the CFG
    var cfg = irFunc.virginCFG;

    // Compute a block ordering for the function
    var blockOrder = orderBlocks(cfg.entry, cfg.blocks);

    print('order:');
    for (var i = 0; i < blockOrder.length; ++i)
        print(blockOrder[i].getBlockName());

    // Perform register allocation
    var allocInfo = x86.allocRegs(irFunc, blockOrder, this, params);

    // Produce assembler for the function
    var asm = x86.irToASM(irFunc, blockOrder, allocInfo, this, params);

    // Assemble the code into an executable code block
    var codeBlock = asm.assemble();

    //
    // TODO: store the code block on the function object
    //

    // Return the code block
    return codeBlock;
}

/**
Link the values needing to be linked in the code generated
for an IR function.
*/
x86.Backend.prototype.linkCode = function (irFunction)
{
    assert (
        irFunction instanceof IRFunction,
        'expected IR function'
    );

    //
    // TODO: linker.js
    //




}

/**
Get the calling convention for a given target
*/
x86.Backend.prototype.getCallConv = function (target)
{
    switch (target)
    {
        case 'tachyon':
        if (this.x86_64)
            return CallConv.tachyon64;
        else
            return CallConv.tachyon32;

        case 'c':
        if (this.x86_64)
            return CallConv.amd64;
        else
            return CallConv.cdecl;

        default:
        error('unsupported target: ' + target);
    }
}

