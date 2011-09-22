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
x86 namespace
*/
var x86 = x86 || {};

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
    General-purpose register size in bits
    */
    this.regSizeBits = x86_64? 64:32;

    /**
    General-purpose register size in bytes
    */
    this.regSizeBytes = this.regSizeBits / 8;

    /**
    Number of general-purpose registers
    */
    this.numGpRegs = x86_64? 16:8;

    /**
    Endianness of the target architecture
    */
    this.endian = 'little';

    /**
    Flag for 64-bit mode
    */
    this.x86_64 = x86_64;

    /**
    Stack pointer register
    */
    this.spReg = x86.regs.rsp.getSubOpnd(this.regSizeBits);

    /**
    Context register
    */
    this.ctxReg = x86.regs.rcx.getSubOpnd(this.regSizeBits);

    /**
    Set of general-purpose registers available for allocation
    */
    this.gpRegSet = x86_64? [
        x86.regs.rax,
        //x86.regs.rcx, // For now, used as context register
        x86.regs.rdx,
        x86.regs.rbx,
        x86.regs.rbp,
        x86.regs.rsi,
        x86.regs.rdi,
        x86.regs.r8,
        x86.regs.r9,
        x86.regs.r10,
        x86.regs.r11,
        x86.regs.r12,
        x86.regs.r13,
        x86.regs.r14,
        x86.regs.r15
    ]:[
        x86.regs.eax,
        //x86.regs.ecx, // For now, used as context register
        x86.regs.edx,
        x86.regs.ebx,
        x86.regs.ebp,
        x86.regs.esi,
        x86.regs.edi
    ];

    /**
    Flag to enable the debug trace
    */
    this.debugTrace = false;
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

    log.debug('');
    log.debug('generating code for "' + irFunc.funcName + '"');

    // Get a reference to the CFG
    var cfg = irFunc.virginCFG;

    // Compute a block ordering for the function
    var blockOrder = orderBlocks(cfg.entry, cfg.blocks);

    /*
    log.debug('order:');
    for (var i = 0; i < blockOrder.length; ++i)
        log.debug(blockOrder[i].getBlockName());
    */

    // Perform liveness analysis on the CFG
    var liveness = liveAnalysis(blockOrder);

    // Produce assembler for the function
    var assembler = x86.genCode(irFunc, blockOrder, liveness, this, params);

    log.debug('OPTIMIZING');

    // Run the peephole optimizer
    x86.optimize(assembler);

    log.debug('DONE OPTIMIZING');

    if (config.verbosity >= log.DEBUG)
    {
        log.debug('');
        log.debug('assembly for "' + irFunc.funcName + '":')
        log.debug(assembler.toString(true));
        log.debug('');
    }

    log.debug('ASSEMBLING');

    // Assemble the code into an executable code block
    var codeBlock = assembler.assemble();

    log.debug('DONE ASSEMBLING');

    // Store the compiled code block on the function object
    irFunc.codeBlock = codeBlock;

    // Return the assembler and code block
    return {
        assembler: assembler,
        codeBlock: codeBlock
    };
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

