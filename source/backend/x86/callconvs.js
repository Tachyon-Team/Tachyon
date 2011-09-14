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
Calling convention handling.

@author
Maxime Chevalier-Boisvert
*/

/**
@class Calling convention representation
*/
function CallConv(params)
{
    /**
    @field Calling convention name.
    */
    this.name = params.name;

    /**
    @field Register size for the target platform
    */
    this.regSize = params.regSize;

    /**
    @field Stack pointer alignment.
    */
    this.spAlign = params.spAlign;

    /**
    @field Arguments passing order. Must be 'LTR' or 'RTL'.
    */
    this.argOrder = params.argOrder;

    /**
    @field Registers used to pass the first arguments.
    */
    this.argRegs = params.argRegs;

    /**
    @field Registers used to pass the first floating-point arguments.
    */
    this.fpArgRegs = params.fpArgRegs;

    /**
    @field Argument count register. Can be null if none.
    */
    this.argCountReg = params.argCountReg;

    /**
    @field Return value register. Can be null if none.
    */
    this.retReg = params.retReg;

    /**
    @field Floating-point return value register. Can be null if none.
    */
    this.fpRetReg = params.fpRetReg;

    /**
    @field Caller or callee cleanup. Must be 'CALLER' or 'CALLEE'.
    */
    this.cleanup = params.cleanup;

    /**
    @field Caller-save registers
    */
    this.callerSave = params.callerSave;

    /**
    @field Callee-save registers
    */
    this.calleeSave = params.calleeSave;

    assert (
        typeof this.name === 'string',
        'invalid calling convention name'
    );

    assert (
        this.regSize === 32 || this.regSize === 64,
        'invalid register size'
    );

    assert (
        isPosInt(this.spAlign),
        'invalid sp alignment: ' + this.spAlign + ' (' + this.name + ')'
    );

    assert (
        this.argOrder === 'LTR' || this.argOrder === 'RTL',
        'invalid argument order'
    );

    assert (
        this.argRegs instanceof Array,
        'invalid arg regs array'
    );

    assert (
        this.fpArgRegs instanceof Array,
        'invalid fp arg regs array'
    );

    assert (
        this.argCountReg === null || 
        this.argCountReg instanceof x86.Register,
        'invalid arg count reg'
    );

    assert (
        this.retReg === null || 
        this.retReg instanceof x86.Register,
        'invalid ret reg'
    );

    assert (
        this.fpRetReg === null || 
        this.fpRetReg instanceof x86.Register,
        'invalid fp ret reg'
    );

    assert (
        this.callerSave instanceof Array,
        'invalid caller-save register list'
    );

    assert (
        this.calleeSave instanceof Array,
        'invalid callee-save register list'
    );

    assert (
        this.cleanup === 'CALLER' || this.cleanup === 'CALLEE',
        'invalid caller/callee cleanup argument'
    );
}

/**
cdecl calling convention
*/
CallConv.cdecl = new CallConv({
    name        : 'cdecl',
    regSize     : 32,
    spAlign     : 16,
    argOrder    : 'RTL',
    argRegs     : [],
    fpArgRegs   : [],
    argCountReg : null,
    retReg      : x86.regs.eax,
    fpRetReg    : x86.regs.st0,
    callerSave  : [x86.regs.edx,
                   x86.regs.ecx],
    calleeSave  : [x86.regs.ebx,
                   x86.regs.ebp,
                   x86.regs.esi, 
                   x86.regs.edi],
    cleanup     : 'CALLER'
});

/**
AMD64 ABI calling convention
*/
CallConv.amd64 = new CallConv({
    name        : 'amd64',
    regSize     : 64,
    spAlign     : 16,
    argOrder    : 'RTL',
    argRegs     : [x86.regs.rdi, 
                   x86.regs.rsi, 
                   x86.regs.rdx, 
                   x86.regs.rcx,
                   x86.regs.r8,
                   x86.regs.r9],
    fpArgRegs   : [x86.regs.xmm0,
                   x86.regs.xmm1,
                   x86.regs.xmm2,
                   x86.regs.xmm3,
                   x86.regs.xmm4,
                   x86.regs.xmm5,
                   x86.regs.xmm6,
                   x86.regs.xmm7],
    argCountReg : null,
    retReg      : x86.regs.rax,
    fpRetReg    : x86.regs.xmm0,
    callerSave  : [x86.regs.rdx,
                   x86.regs.rcx,
                   x86.regs.rsi,
                   x86.regs.rdi,
                   x86.regs.r8,
                   x86.regs.r9,
                   x86.regs.r11,
                   x86.regs.r12],
    calleeSave  : [x86.regs.rbx, 
                   x86.regs.rbp, 
                   x86.regs.r10,
                   x86.regs.r13,
                   x86.regs.r14,
                   x86.regs.r15],
    cleanup     : 'CALLER'
});

/**
Tachyon 32-bit calling convention
*/
CallConv.tachyon32 = new CallConv({
    name        : 'tachyon32',
    regSize     : 32,
    spAlign     : 4,
    argOrder    : 'LTR',
    argRegs     : [x86.regs.eax, 
                   x86.regs.edx,
                   x86.regs.ebx,
                   x86.regs.esi],
    fpArgRegs   : [x86.regs.xmm0,
                   x86.regs.xmm1,
                   x86.regs.xmm2,
                   x86.regs.xmm3],
    argCountReg : x86.regs.cl,
    retReg      : x86.regs.eax,
    fpRetReg    : x86.regs.xmm0,
    callerSave  : [x86.regs.edx,
                   x86.regs.ebx,
                   x86.regs.esi,
                   x86.regs.edi,
                   x86.regs.ebp],    
    calleeSave  : [],
    cleanup     : 'CALLEE'
});

/**
Tachyon 64-bit calling convention
*/
CallConv.tachyon64 = new CallConv({
    name        : 'tachyon64',
    regSize     : 64,
    spAlign     : 8,
    argOrder    : 'LTR',
    argRegs     : [x86.regs.rax, 
                   x86.regs.rdx,
                   x86.regs.rbx,
                   x86.regs.rsi,
                   x86.regs.rdi,
                   x86.regs.rbp],
    fpArgRegs   : [x86.regs.xmm0,
                   x86.regs.xmm1,
                   x86.regs.xmm2,
                   x86.regs.xmm3,
                   x86.regs.xmm4,
                   x86.regs.xmm5,
                   x86.regs.xmm6,
                   x86.regs.xmm7],
    argCountReg : x86.regs.cl,
    retReg      : x86.regs.rax,
    fpRetReg    : x86.regs.xmm0,
    callerSave  : [x86.regs.rdx,
                   x86.regs.rbx,
                   x86.regs.rsi,
                   x86.regs.rdi,
                   x86.regs.rbp,
                   x86.regs.r8,
                   x86.regs.r9,
                   x86.regs.r10,
                   x86.regs.r11,
                   x86.regs.r12,
                   x86.regs.r13,
                   x86.regs.r14,
                   x86.regs.r15],
    calleeSave  : [],
    cleanup     : 'CALLEE'
});

