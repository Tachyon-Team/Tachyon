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
Table of x86 instruction encodings.

@author
Maxime Chevalier-Boisvert
*/

/**
x86 namespace
*/
var x86 = x86 || {};

/**
x86 instruction description table.
This table can contain multiple entries per instruction.

mnem    : mnemonic name
op1     : opcode 1 (dest)
op2     : opcode 2
opCode  : opcode bytes
opExt   : opcode extension byte
REX_W   : REX.W bit
szPref  : operand-size prefix required
x86_64  : set to false if invalid in 64-bit mode

*/
x86.instrTable = [

    /*
    TODO: Needed instructions
    look for other issues with this table

    CMOVxx

    Useful but less important:
    INC
    DEC
    RDTSC, read time stamp
    RDPMC, read performance counters
    */

    // Addition
    {mnem: 'add', opnds: ['al', 'imm8'], opCode: [0x04]},
    {mnem: 'add', opnds: ['ax', 'imm16'], opCode: [0x05], szPref: true},
    {mnem: 'add', opnds: ['eax', 'imm32'], opCode: [0x05]},
    {mnem: 'add', opnds: ['rax', 'imm32'], opCode: [0x05], REX_W: 1},
    {mnem: 'add', opnds: ['r/m8', 'imm8'], opCode: [0x80], opExt: 0},
    {mnem: 'add', opnds: ['r/m16', 'imm16'], opCode: [0x81], opExt: 0, szPref: true},
    {mnem: 'add', opnds: ['r/m32', 'imm32'], opCode: [0x81], opExt: 0},
    {mnem: 'add', opnds: ['r/m64', 'imm32'], opCode: [0x81], opExt: 0, REX_W: 1},
    {mnem: 'add', opnds: ['r/m16', 'imm8'], opCode: [0x83], opExt: 0, szPref: true},
    {mnem: 'add', opnds: ['r/m32', 'imm8'], opCode: [0x83], opExt: 0},
    {mnem: 'add', opnds: ['r/m64', 'imm8'], opCode: [0x83], opExt: 0, REX_W: 1},
    {mnem: 'add', opnds: ['r/m8', 'r8'], opCode: [0x00]},
    {mnem: 'add', opnds: ['r/m16', 'r16'], opCode: [0x01], szPref: true},
    {mnem: 'add', opnds: ['r/m32', 'r32'], opCode: [0x01]},
    {mnem: 'add', opnds: ['r/m64', 'r64'], opCode: [0x01], REX_W: 1},
    {mnem: 'add', opnds: ['r8', 'r/m8'], opCode: [0x02]},
    {mnem: 'add', opnds: ['r16', 'r/m16'], opCode: [0x03], szPref: true},
    {mnem: 'add', opnds: ['r32', 'r/m32'], opCode: [0x03]},
    {mnem: 'add', opnds: ['r64', 'r/m64'], opCode: [0x03], REX_W: 1},

    // Bitwise AND
    {mnem: 'and', opnds: ['al', 'imm8'], opCode: [0x24]},
    {mnem: 'and', opnds: ['ax', 'imm16'], opCode: [0x25], szPref: true},
    {mnem: 'and', opnds: ['eax', 'imm32'], opCode: [0x25]},
    {mnem: 'and', opnds: ['rax', 'imm32'], opCode: [0x25], REX_W: 1},
    {mnem: 'and', opnds: ['r/m8', 'imm8'], opCode: [0x80], opExt: 4},
    {mnem: 'and', opnds: ['r/m16', 'imm16'], opCode: [0x81], opExt: 4, szPref: true},
    {mnem: 'and', opnds: ['r/m32', 'imm32'], opCode: [0x81], opExt: 4},
    {mnem: 'and', opnds: ['r/m64', 'imm32'], opCode: [0x81], opExt: 4, REX_W: 1},
    {mnem: 'and', opnds: ['r/m16', 'imm8'], opCode: [0x83], opExt: 4, szPref: true},
    {mnem: 'and', opnds: ['r/m32', 'imm8'], opCode: [0x83], opExt: 4},
    {mnem: 'and', opnds: ['r/m64', 'imm8'], opCode: [0x83], opExt: 4, REX_W: 1},
    {mnem: 'and', opnds: ['r/m8', 'r8'], opCode: [0x20]},
    {mnem: 'and', opnds: ['r/m16', 'r16'], opCode: [0x21], szPref: true},
    {mnem: 'and', opnds: ['r/m32', 'r32'], opCode: [0x21]},
    {mnem: 'and', opnds: ['r/m64', 'r64'], opCode: [0x21], REX_W: 1},
    {mnem: 'and', opnds: ['r8', 'r/m8'], opCode: [0x22]},

    // Call (relative and absolute)
    {mnem: 'call', opnds: ['rel32'], opCode: [0xE8]},
    {mnem: 'call', opnds: ['r/m32'], opCode: [0xFF], opExt: 2, x86_64: false},
    {mnem: 'call', opnds: ['r/m64'], opCode: [0xFF], opExt: 2},

    // Convert word to doubleword (sign extension)
    {mnem: 'cwd', opnds: [], opCode: [0x99], szPref: true},
    {mnem: 'cdq', opnds: [], opCode: [0x99]},
    {mnem: 'cqo', opnds: [], opCode: [0x99], REX_W: 1},    

    // Comparison (integer)
    {mnem: 'cmp', opnds: ['al', 'imm8'], opCode: [0x3C]},
    {mnem: 'cmp', opnds: ['ax', 'imm16'], opCode: [0x3D], szPref: true},
    {mnem: 'cmp', opnds: ['eax', 'imm32'], opCode: [0x3D]},
    {mnem: 'cmp', opnds: ['rax', 'imm32'], opCode: [0x3D], REX_W: 1},
    {mnem: 'cmp', opnds: ['r/m8', 'imm8'], opCode: [0x80], opExt: 7},
    {mnem: 'cmp', opnds: ['r/m16', 'imm16'], opCode: [0x81], opExt: 7, szPref: true},
    {mnem: 'cmp', opnds: ['r/m32', 'imm32'], opCode: [0x81], opExt: 7},
    {mnem: 'cmp', opnds: ['r/m64', 'imm32'], opCode: [0x81], opExt: 7, REX_W: 1},
    {mnem: 'cmp', opnds: ['r/m16', 'imm8'], opCode: [0x83], opExt: 7, szPref: true},
    {mnem: 'cmp', opnds: ['r/m32', 'imm8'], opCode: [0x83], opExt: 7},
    {mnem: 'cmp', opnds: ['r/m64', 'imm8'], opCode: [0x83], opExt: 7, REX_W: 1},
    {mnem: 'cmp', opnds: ['r/m8', 'r8'], opCode: [0x38]},
    {mnem: 'cmp', opnds: ['r/m16', 'r16'], opCode: [0x39], szPref: true},
    {mnem: 'cmp', opnds: ['r/m32', 'r32'], opCode: [0x39]},
    {mnem: 'cmp', opnds: ['r/m64', 'r64'], opCode: [0x39], REX_W: 1},
    {mnem: 'cmp', opnds: ['r8', 'r/m8'], opCode: [0x3A]},
    {mnem: 'cmp', opnds: ['r16', 'r/m16'], opCode: [0x3B], szPref: true},
    {mnem: 'cmp', opnds: ['r32', 'r/m32'], opCode: [0x3B]},
    {mnem: 'cmp', opnds: ['r64', 'r/m64'], opCode: [0x3B], REX_W: 1},

    // CPU id
    {mnem: 'cpuid', opCode: [0x0F, 0xA2]},

    // Division (unsigned integer)
    {mnem: 'div', opnds: ['r/m8'], opCode: [0xF6], opExt: 6},
    {mnem: 'div', opnds: ['r/m16'], opCode: [0xF7], opExt: 6, szPref: true},
    {mnem: 'div', opnds: ['r/m32'], opCode: [0xF7], opExt: 6},
    {mnem: 'div', opnds: ['r/m64'], opCode: [0xF7], opExt: 6, REX_W: 1},

    // Division (signed integer)
    {mnem: 'idiv', opnds: ['r/m8'], opCode: [0xF6], opExt: 7},
    {mnem: 'idiv', opnds: ['r/m16'], opCode: [0xF7], opExt: 7, szPref: true},
    {mnem: 'idiv', opnds: ['r/m32'], opCode: [0xF7], opExt: 7},
    {mnem: 'idiv', opnds: ['r/m64'], opCode: [0xF7], opExt: 7, REX_W: 1},

    // Multiply (signed integer)
    {mnem: 'imul', opnds: ['r/m8'], opCode: [0xF6], opExt: 5},
    {mnem: 'imul', opnds: ['r/m16'], opCode: [0xF7], opExt: 5, szPref: true},
    {mnem: 'imul', opnds: ['r/m32'], opCode: [0xF7], opExt: 5},
    {mnem: 'imul', opnds: ['r/m64'], opCode: [0xF7], opExt: 5, REX_W: 1},
    {mnem: 'imul', opnds: ['r16', 'r/m16'], opCode: [0x0F, 0xAF], szPref: true},
    {mnem: 'imul', opnds: ['r32', 'r/m32'], opCode: [0x0F, 0xAF]},
    {mnem: 'imul', opnds: ['r64', 'r/m64'], opCode: [0x0F, 0xAF], REX_W: 1},
    {mnem: 'imul', opnds: ['r16', 'r/m16', 'imm8'], opCode: [0x6B], szPref: true},
    {mnem: 'imul', opnds: ['r32', 'r/m32', 'imm8'], opCode: [0x6B]},
    {mnem: 'imul', opnds: ['r64', 'r/m64', 'imm8'], opCode: [0x6B], REX_W: 1},
    {mnem: 'imul', opnds: ['r16', 'r/m16', 'imm16'], opCode: [0x69], szPref: true},
    {mnem: 'imul', opnds: ['r32', 'r/m32', 'imm32'], opCode: [0x69]},
    {mnem: 'imul', opnds: ['r64', 'r/m64', 'imm32'], opCode: [0x69], REX_W: 1},

    // Conditional jumps (relative near)
    {mnem: 'ja', opnds: ['rel8'], opCode: [0x77]},
    {mnem: 'jae', opnds: ['rel8'], opCode: [0x73]},
    {mnem: 'jb', opnds: ['rel8'], opCode: [0x72]},
    {mnem: 'jbe', opnds: ['rel8'], opCode: [0x76]},
    {mnem: 'jc', opnds: ['rel8'], opCode: [0x72]},
    {mnem: 'je', opnds: ['rel8'], opCode: [0x74]},
    {mnem: 'jg', opnds: ['rel8'], opCode: [0x7F]},
    {mnem: 'jge', opnds: ['rel8'], opCode: [0x7D]},
    {mnem: 'jl', opnds: ['rel8'], opCode: [0x7C]},
    {mnem: 'jle', opnds: ['rel8'], opCode: [0x7E]},
    {mnem: 'jna', opnds: ['rel8'], opCode: [0x76]},
    {mnem: 'jnae', opnds: ['rel8'], opCode: [0x72]},
    {mnem: 'jnb', opnds: ['rel8'], opCode: [0x73]},
    {mnem: 'jnbe', opnds: ['rel8'], opCode: [0x77]},
    {mnem: 'jnc', opnds: ['rel8'], opCode: [0x73]},
    {mnem: 'jne', opnds: ['rel8'], opCode: [0x75]},
    {mnem: 'jng', opnds: ['rel8'], opCode: [0x7E]},
    {mnem: 'jnge', opnds: ['rel8'], opCode: [0x7C]},
    {mnem: 'jnl', opnds: ['rel8'], opCode: [0x7D]},
    {mnem: 'jnle', opnds: ['rel8'], opCode: [0x7F]},
    {mnem: 'jno', opnds: ['rel8'], opCode: [0x71]},
    {mnem: 'jnp', opnds: ['rel8'], opCode: [0x7B]},
    {mnem: 'jns', opnds: ['rel8'], opCode: [0x79]},
    {mnem: 'jnz', opnds: ['rel8'], opCode: [0x75]},
    {mnem: 'jo', opnds: ['rel8'], opCode: [0x70]},
    {mnem: 'jp', opnds: ['rel8'], opCode: [0x7A]},
    {mnem: 'jpe', opnds: ['rel8'], opCode: [0x7A]},
    {mnem: 'jpo', opnds: ['rel8'], opCode: [0x7B]},
    {mnem: 'js', opnds: ['rel8'], opCode: [0x78]},
    {mnem: 'jz', opnds: ['rel8'], opCode: [0x74]},
    {mnem: 'ja', opnds: ['rel32'], opCode: [0x0F, 0x87]},
    {mnem: 'jae', opnds: ['rel32'], opCode: [0x0F, 0x83]},
    {mnem: 'jb', opnds: ['rel32'], opCode: [0x0F, 0x82]},
    {mnem: 'jbe', opnds: ['rel32'], opCode: [0x0F, 0x86]},
    {mnem: 'jc', opnds: ['rel32'], opCode: [0x0F, 0x82]},
    {mnem: 'je', opnds: ['rel32'], opCode: [0x0F, 0x84]},
    {mnem: 'jz', opnds: ['rel32'], opCode: [0x0F, 0x84]},
    {mnem: 'jg', opnds: ['rel32'], opCode: [0x0F, 0x8F]},
    {mnem: 'jge', opnds: ['rel32'], opCode: [0x0F, 0x8D]},
    {mnem: 'jl', opnds: ['rel32'], opCode: [0x0F, 0x8C]},
    {mnem: 'jle', opnds: ['rel32'], opCode: [0x0F, 0x8E]},
    {mnem: 'jna', opnds: ['rel32'], opCode: [0x0F, 0x86]},
    {mnem: 'jnae', opnds: ['rel32'], opCode: [0x0F, 0x82]},
    {mnem: 'jnb', opnds: ['rel32'], opCode: [0x0F, 0x83]},
    {mnem: 'jnbe', opnds: ['rel32'], opCode: [0x0F, 0x87]},
    {mnem: 'jnc', opnds: ['rel32'], opCode: [0x0F, 0x83]},
    {mnem: 'jne', opnds: ['rel32'], opCode: [0x0F, 0x85]},
    {mnem: 'jng', opnds: ['rel32'], opCode: [0x0F, 0x8E]},
    {mnem: 'jnge', opnds: ['rel32'], opCode: [0x0F, 0x8C]},
    {mnem: 'jnl', opnds: ['rel32'], opCode: [0x0F, 0x8D]},
    {mnem: 'jnle', opnds: ['rel32'], opCode: [0x0F, 0x8F]},
    {mnem: 'jno', opnds: ['rel32'], opCode: [0x0F, 0x81]},
    {mnem: 'jnp', opnds: ['rel32'], opCode: [0x0F, 0x8b]},
    {mnem: 'jns', opnds: ['rel32'], opCode: [0x0F, 0x89]},
    {mnem: 'jnz', opnds: ['rel32'], opCode: [0x0F, 0x85]},
    {mnem: 'jo', opnds: ['rel32'], opCode: [0x0F, 0x80]},
    {mnem: 'jp', opnds: ['rel32'], opCode: [0x0F, 0x8A]},
    {mnem: 'jpe', opnds: ['rel32'], opCode: [0x0F, 0x8A]},
    {mnem: 'jpo', opnds: ['rel32'], opCode: [0x0F, 0x8B]},
    {mnem: 'js', opnds: ['rel32'], opCode: [0x0F, 0x88]},
    {mnem: 'jz', opnds: ['rel32'], opCode: [0x0F, 0x84]},

    // Jump (relative near)
    {mnem: 'jmp', opnds: ['rel8'], opCode: [0xEB]},
    {mnem: 'jmp', opnds: ['rel32'], opCode: [0xE9]},

    // Jump (absolute near)
    {mnem: 'jmp', opnds: ['r/m32'], opCode: [0xFF], opExt: 4, x86_64: false},
    {mnem: 'jmp', opnds: ['r/m64'], opCode: [0xFF], opExt: 4, REX_W: 1},

    // Move
    {mnem: 'mov', opnds: ['r/m8', 'r8'], opCode: [0x88]},
    {mnem: 'mov', opnds: ['r/m16', 'r16'], opCode: [0x89], szPref: true},
    {mnem: 'mov', opnds: ['r/m32', 'r32'], opCode: [0x89]},
    {mnem: 'mov', opnds: ['r/m64', 'r64'], opCode: [0x89], REX_W: 1},
    {mnem: 'mov', opnds: ['r8', 'r/m8'], opCode: [0x8A]},
    {mnem: 'mov', opnds: ['r16', 'r/m16'], opCode: [0x8B], szPref: true},
    {mnem: 'mov', opnds: ['r32', 'r/m32'], opCode: [0x8B]},
    {mnem: 'mov', opnds: ['r64', 'r/m64'], opCode: [0x8B], REX_W: 1},
    {mnem: 'mov', opnds: ['eax', 'moffs32'], opCode: [0xA1]},
    {mnem: 'mov', opnds: ['rax', 'moffs64'], opCode: [0xA1], REX_W: 1},
    {mnem: 'mov', opnds: ['moffs32', 'eax'], opCode: [0xA3]},
    {mnem: 'mov', opnds: ['moffs64', 'rax'], opCode: [0xA3], REX_W: 1},
    {mnem: 'mov', opnds: ['r8', 'imm8'], opCode: [0xB0]},
    {mnem: 'mov', opnds: ['r16', 'imm16'], opCode: [0xB8], szPref: true},
    {mnem: 'mov', opnds: ['r32', 'imm32'], opCode: [0xB8]},
    {mnem: 'mov', opnds: ['r64', 'imm64'], opCode: [0xB8], REX_W: 1},
    {mnem: 'mov', opnds: ['r/m8', 'imm8'], opCode: [0xC6], opExt: 0},
    {mnem: 'mov', opnds: ['r/m16', 'imm16'], opCode: [0xC7], opExt: 0, szPref: true},
    {mnem: 'mov', opnds: ['r/m32', 'imm32'], opCode: [0xC7], opExt: 0},
    {mnem: 'mov', opnds: ['r/m64', 'imm32'], opCode: [0xC7], opExt: 0, REX_W: 1},

    // Move with sign extension
    {mnem: 'movsx', opnds: ['r16', 'r/m8'], opCode: [0x0F, 0xBE], szPref: true},
    {mnem: 'movsx', opnds: ['r32', 'r/m8'], opCode: [0x0F, 0xBE]},
    {mnem: 'movsx', opnds: ['r64', 'r/m8'], opCode: [0x0F, 0xBE], REX_W: 1},
    {mnem: 'movsx', opnds: ['r32', 'r/m16'], opCode: [0x0F, 0xBF]},
    {mnem: 'movsx', opnds: ['r64', 'r/m16'], opCode: [0x0F, 0xBF], REX_W: 1},
    {mnem: 'movsxd', opnds: ['r64', 'r/m32'], opCode: [0x63], REX_W: 1},

    // Move with zero extension
    {mnem: 'movzx', opnds: ['r16', 'r/m8'], opCode: [0x0F, 0xB6], szPref: true},
    {mnem: 'movzx', opnds: ['r32', 'r/m8'], opCode: [0x0F, 0xB6]},
    {mnem: 'movzx', opnds: ['r64', 'r/m8'], opCode: [0x0F, 0xB6], REX_W: 1},
    {mnem: 'movzx', opnds: ['r32', 'r/m16'], opCode: [0x0F, 0xB7]},
    {mnem: 'movzx', opnds: ['r64', 'r/m16'], opCode: [0x0F, 0xB7], REX_W: 1},

    // Multiply (unsigned integer)
    {mnem: 'mul', opnds: ['r/m8'], opCode: [0xF6], opExt: 4},
    {mnem: 'mul', opnds: ['r/m16'], opCode: [0xF7], opExt: 4, szPref: true},
    {mnem: 'mul', opnds: ['r/m32'], opCode: [0xF7], opExt: 4},
    {mnem: 'mul', opnds: ['r/m64'], opCode: [0xF7], opExt: 4, REX_W: 1},

    // Negation (multiplication by -1)
    {mnem: 'neg', opnds: ['r/m8'], opCode: [0xF6], opExt: 3},
    {mnem: 'neg', opnds: ['r/m16'], opCode: [0xF7], opExt: 3, szPref: true},
    {mnem: 'neg', opnds: ['r/m32'], opCode: [0xF7], opExt: 3},
    {mnem: 'neg', opnds: ['r/m64'], opCode: [0xF7], opExt: 3, REX_W: 1},

    // No operation
    {mnem: 'nop', opCode: [0x90]},

    // Bitwise negation
    {mnem: 'not', opnds: ['r/m8'], opCode: [0xF6], opExt: 2},
    {mnem: 'not', opnds: ['r/m16'], opCode: [0xF7], opExt: 2, szPref: true},
    {mnem: 'not', opnds: ['r/m32'], opCode: [0xF7], opExt: 2},
    {mnem: 'not', opnds: ['r/m64'], opCode: [0xF7], opExt: 2, REX_W: 1},

    // Bitwise OR
    {mnem: 'or', opnds: ['al', 'imm8'], opCode: [0x0C]},
    {mnem: 'or', opnds: ['ax', 'imm16'], opCode: [0x0D], szPref: true},           
    {mnem: 'or', opnds: ['eax', 'imm32'], opCode: [0x0D]},           
    {mnem: 'or', opnds: ['rax', 'imm32'], opCode: [0x0D], REX_W: 1},
    {mnem: 'or', opnds: ['r/m8', 'imm8'], opCode: [0x80], opExt: 1},
    {mnem: 'or', opnds: ['r/m16', 'imm16'], opCode: [0x81], opExt: 1, szPref: true},
    {mnem: 'or', opnds: ['r/m32', 'imm32'], opCode: [0x81], opExt: 1},
    {mnem: 'or', opnds: ['r/m64', 'imm32'], opCode: [0x81], opExt: 1, REX_W: 1},
    {mnem: 'or', opnds: ['r/m16', 'imm8'], opCode: [0x83], opExt: 1, szPref: true},
    {mnem: 'or', opnds: ['r/m32', 'imm8'], opCode: [0x83], opExt: 1},
    {mnem: 'or', opnds: ['r/m64', 'imm8'], opCode: [0x83], opExt: 1, REX_W: 1},
    {mnem: 'or', opnds: ['r/m8', 'r8'], opCode: [0x08]},
    {mnem: 'or', opnds: ['r/m16', 'r16'], opCode: [0x09], szPref: true},
    {mnem: 'or', opnds: ['r/m32', 'r32'], opCode: [0x09]},
    {mnem: 'or', opnds: ['r/m64', 'r64'], opCode: [0x09], REX_W: 1},
    {mnem: 'or', opnds: ['r8', 'r/m8'], opCode: [0x0A]},
    {mnem: 'or', opnds: ['r16', 'r/m16'], opCode: [0x0B], szPref: true},
    {mnem: 'or', opnds: ['r32', 'r/m32'], opCode: [0x0B]},
    {mnem: 'or', opnds: ['r64', 'r/m64'], opCode: [0x0B], REX_W: 1},

    // Pop
    {mnem: 'pop', opnds: ['r/m16'], opCode: [0x8F], opExt: 0, szPref: true},
    {mnem: 'pop', opnds: ['r/m32'], opCode: [0x8F], opExt: 0, x86_64: false},
    {mnem: 'pop', opnds: ['r/m64'], opCode: [0x8F], opExt: 0, REX_W: 1},
    {mnem: 'pop', opnds: ['r16'], opCode: [0x58], szPref: true},
    {mnem: 'pop', opnds: ['r32'], opCode: [0x58], x86_64: false},
    {mnem: 'pop', opnds: ['r64'], opCode: [0x58], REX_W: 1},

    // Push
    {mnem: 'push', opnds: ['r/m16'], opCode: [0xFF], opExt: 6, szPref: true},
    {mnem: 'push', opnds: ['r/m32'], opCode: [0xFF], opExt: 6, x86_64: false},
    {mnem: 'push', opnds: ['r/m64'], opCode: [0xFF], opExt: 6, REX_W: 1},
    {mnem: 'push', opnds: ['r16'], opCode: [0x50], szPref: true},
    {mnem: 'push', opnds: ['r32'], opCode: [0x50], x86_64: false},
    {mnem: 'push', opnds: ['r64'], opCode: [0x50], REX_W: 1},
    {mnem: 'push', opnds: ['imm8'], opCode: [0x6A]},
    {mnem: 'push', opnds: ['imm16'], opCode: [0x68], szPref: true},
    {mnem: 'push', opnds: ['imm32'], opCode: [0x68]},

    // Return
    {mnem: 'ret', opCode: [0xC3]},
    {mnem: 'ret', opnds: ['imm16'], opCode: [0xC2]},

    // Shift arithmetic left
    {mnem: 'sal', opnds: ['r/m8', 1], opCode: [0xD0], opExt: 4},
    {mnem: 'sal', opnds: ['r/m8', 'cl'], opCode: [0xD2], opExt: 4},
    {mnem: 'sal', opnds: ['r/m8', 'imm8'], opCode: [0xC0], opExt: 4},
    {mnem: 'sal', opnds: ['r/m16', 1], opCode: [0xD1], opExt: 4, szPref: true},
    {mnem: 'sal', opnds: ['r/m16', 'cl'], opCode: [0xD3], opExt: 4, szPref: true},
    {mnem: 'sal', opnds: ['r/m16', 'imm8'], opCode: [0xC1], opExt: 4, szPref: true},
    {mnem: 'sal', opnds: ['r/m32', 1], opCode: [0xD1], opExt: 4},
    {mnem: 'sal', opnds: ['r/m32', 'cl'], opCode: [0xD3], opExt: 4},
    {mnem: 'sal', opnds: ['r/m32', 'imm8'], opCode: [0xC1], opExt: 4},
    {mnem: 'sal', opnds: ['r/m64', 1], opCode: [0xD1], opExt: 4, REX_W: 1},
    {mnem: 'sal', opnds: ['r/m64', 'cl'], opCode: [0xD3], opExt: 4, REX_W: 1},
    {mnem: 'sal', opnds: ['r/m64', 'imm8'], opCode: [0xC1], opExt: 4, REX_W: 1},

    // Shift arithmetic right (signed)
    {mnem: 'sar', opnds: ['r/m8', 1], opCode: [0xD0], opExt: 7},
    {mnem: 'sar', opnds: ['r/m8', 'cl'], opCode: [0xD2], opExt: 7},
    {mnem: 'sar', opnds: ['r/m8', 'imm8'], opCode: [0xC0], opExt: 7},
    {mnem: 'sar', opnds: ['r/m16', 1], opCode: [0xD1], opExt: 7, szPref: true},
    {mnem: 'sar', opnds: ['r/m16', 'cl'], opCode: [0xD3], opExt: 7, szPref: true},
    {mnem: 'sar', opnds: ['r/m16', 'imm8'], opCode: [0xC1], opExt: 7, szPref: true},
    {mnem: 'sar', opnds: ['r/m32', 1], opCode: [0xD1], opExt: 7},
    {mnem: 'sar', opnds: ['r/m32', 'cl'], opCode: [0xD3], opExt: 7},
    {mnem: 'sar', opnds: ['r/m32', 'imm8'], opCode: [0xC1], opExt: 7},
    {mnem: 'sar', opnds: ['r/m64', 1], opCode: [0xD1], opExt: 7, REX_W: 1},
    {mnem: 'sar', opnds: ['r/m64', 'cl'], opCode: [0xD3], opExt: 7, REX_W: 1},
    {mnem: 'sar', opnds: ['r/m64', 'imm8'], opCode: [0xC1], opExt: 7, REX_W: 1},

    // Shift logical left
    {mnem: 'shl', opnds: ['r/m8', 1], opCode: [0xD0], opExt: 4},
    {mnem: 'shl', opnds: ['r/m8', 'cl'], opCode: [0xD2], opExt: 4},
    {mnem: 'shl', opnds: ['r/m8', 'imm8'], opCode: [0xC0], opExt: 4},
    {mnem: 'shl', opnds: ['r/m16', 1], opCode: [0xD1], opExt: 4, szPref: true},
    {mnem: 'shl', opnds: ['r/m16', 'cl'], opCode: [0xD3], opExt: 4, szPref: true},
    {mnem: 'shl', opnds: ['r/m16', 'imm8'], opCode: [0xC1], opExt: 4, szPref: true},
    {mnem: 'shl', opnds: ['r/m32', 1], opCode: [0xD1], opExt: 4},
    {mnem: 'shl', opnds: ['r/m32', 'cl'], opCode: [0xD3], opExt: 4},
    {mnem: 'shl', opnds: ['r/m32', 'imm8'], opCode: [0xC1], opExt: 4},
    {mnem: 'shl', opnds: ['r/m64', 1], opCode: [0xD1], opExt: 4, REX_W: 1},
    {mnem: 'shl', opnds: ['r/m64', 'cl'], opCode: [0xD3], opExt: 4, REX_W: 1},
    {mnem: 'shl', opnds: ['r/m64', 'imm8'], opCode: [0xC1], opExt: 4, REX_W: 1},

    // Shift logical right (unsigned)
    {mnem: 'shr', opnds: ['r/m8', 1], opCode: [0xD0], opExt: 5},
    {mnem: 'shr', opnds: ['r/m8', 'cl'], opCode: [0xD2], opExt: 5},
    {mnem: 'shr', opnds: ['r/m8', 'imm8'], opCode: [0xC0], opExt: 5},
    {mnem: 'shr', opnds: ['r/m16', 1], opCode: [0xD1], opExt: 5, szPref: true},
    {mnem: 'shr', opnds: ['r/m16', 'cl'], opCode: [0xD3], opExt: 5, szPref: true},
    {mnem: 'shr', opnds: ['r/m16', 'imm8'], opCode: [0xC1], opExt: 5, szPref: true},
    {mnem: 'shr', opnds: ['r/m32', 1], opCode: [0xD1], opExt: 5},
    {mnem: 'shr', opnds: ['r/m32', 'cl'], opCode: [0xD3], opExt: 5},
    {mnem: 'shr', opnds: ['r/m32', 'imm8'], opCode: [0xC1], opExt: 5},
    {mnem: 'shr', opnds: ['r/m64', 1], opCode: [0xD1], opExt: 5, REX_W: 1},
    {mnem: 'shr', opnds: ['r/m64', 'cl'], opCode: [0xD3], opExt: 5, REX_W: 1},
    {mnem: 'shr', opnds: ['r/m64', 'imm8'], opCode: [0xC1], opExt: 5, REX_W: 1},

    // Subtract
    {mnem: 'sub', opnds: ['al', 'imm8'], opCode: [0x2C]},
    {mnem: 'sub', opnds: ['ax', 'imm16'], opCode: [0x2D], szPref: true},
    {mnem: 'sub', opnds: ['eax', 'imm32'], opCode: [0x2D]},           
    {mnem: 'sub', opnds: ['rax', 'imm32'], opCode: [0x2D], REX_W: 1},
    {mnem: 'sub', opnds: ['r/m8', 'imm8'], opCode: [0x80], opExt: 5},
    {mnem: 'sub', opnds: ['r/m16', 'imm16'], opCode: [0x81], opExt: 5, szPref: true},
    {mnem: 'sub', opnds: ['r/m32', 'imm32'], opCode: [0x81], opExt: 5},
    {mnem: 'sub', opnds: ['r/m64', 'imm32'], opCode: [0x81], opExt: 5, REX_W: 1},
    {mnem: 'sub', opnds: ['r/m16', 'imm8'], opCode: [0x83], opExt: 5, szPref: true},
    {mnem: 'sub', opnds: ['r/m32', 'imm8'], opCode: [0x83], opExt: 5},
    {mnem: 'sub', opnds: ['r/m64', 'imm8'], opCode: [0x83], opExt: 5, REX_W: 1},
    {mnem: 'sub', opnds: ['r/m8', 'r8'], opCode: [0x28]},
    {mnem: 'sub', opnds: ['r/m16', 'r16'], opCode: [0x29], szPref: true},
    {mnem: 'sub', opnds: ['r/m32', 'r32'], opCode: [0x29]},
    {mnem: 'sub', opnds: ['r/m64', 'r64'], opCode: [0x29], REX_W: 1},
    {mnem: 'sub', opnds: ['r8', 'r/m8'], opCode: [0x2A]},
    {mnem: 'sub', opnds: ['r16', 'r/m16'], opCode: [0x2B], szPref: true},
    {mnem: 'sub', opnds: ['r32', 'r/m32'], opCode: [0x2B]},
    {mnem: 'sub', opnds: ['r64', 'r/m64'], opCode: [0x2B], REX_W: 1},

    // Logical AND compare
    {mnem: 'test', opnds: ['al', 'imm8'], opCode: [0xA8]},
    {mnem: 'test', opnds: ['ax', 'imm16'], opCode: [0xA9], szPref: true},
    {mnem: 'test', opnds: ['eax', 'imm32'], opCode: [0xA9]},           
    {mnem: 'test', opnds: ['rax', 'imm32'], opCode: [0xA9], REX_W: 1},
    {mnem: 'test', opnds: ['r/m8', 'imm8'], opCode: [0xF6], opExt: 0},
    {mnem: 'test', opnds: ['r/m16', 'imm16'], opCode: [0xF7], opExt: 0, szPref: true},
    {mnem: 'test', opnds: ['r/m32', 'imm32'], opCode: [0xF7], opExt: 0},
    {mnem: 'test', opnds: ['r/m64', 'imm32'], opCode: [0xF7], opExt: 0, REX_W: 1},
    {mnem: 'test', opnds: ['r/m8', 'r8'], opCode: [0x84]},
    {mnem: 'test', opnds: ['r/m16', 'r16'], opCode: [0x85], szPref: true},
    {mnem: 'test', opnds: ['r/m32', 'r32'], opCode: [0x85]},
    {mnem: 'test', opnds: ['r/m64', 'r64'], opCode: [0x85], REX_W: 1},

    // Exchange
    // The ax/eax/rax + rXX variants use the opcode reg field
    {mnem: 'xchg', opnds: ['ax', 'r16'], opCode: [0x90], szPref: true},
    {mnem: 'xchg', opnds: ['r16', 'ax'], opCode: [0x90], szPref: true},
    {mnem: 'xchg', opnds: ['eax', 'r32'], opCode: [0x90]},
    {mnem: 'xchg', opnds: ['r32', 'eax'], opCode: [0x90]},
    {mnem: 'xchg', opnds: ['rax', 'r64'], opCode: [0x90], REX_W: 1},
    {mnem: 'xchg', opnds: ['r64', 'eax'], opCode: [0x90], REX_W: 1},
    {mnem: 'xchg', opnds: ['r/m8', 'r8'], opCode: [0x86]},
    {mnem: 'xchg', opnds: ['r8', 'r/m8'], opCode: [0x86]},
    {mnem: 'xchg', opnds: ['r/m16', 'r16'], opCode: [0x87], szPref: true},
    {mnem: 'xchg', opnds: ['r/m32', 'r32'], opCode: [0x87]},
    {mnem: 'xchg', opnds: ['r/m64', 'r64'], opCode: [0x87], REX_W: 1},
    {mnem: 'xchg', opnds: ['r16', 'r/m16'], opCode: [0x87], szPref: true},
    {mnem: 'xchg', opnds: ['r32', 'r/m32'], opCode: [0x87]},
    {mnem: 'xchg', opnds: ['r64', 'r/m64'], opCode: [0x87], REX_W: 1},

    // Exclusive bitwise OR
    {mnem: 'xor', opnds: ['al', 'imm8'], opCode: [0x34]},
    {mnem: 'xor', opnds: ['ax', 'imm16'], opCode: [0x35], szPref: true},           
    {mnem: 'xor', opnds: ['eax', 'imm32'], opCode: [0x35]},           
    {mnem: 'xor', opnds: ['rax', 'imm32'], opCode: [0x35], REX_W: 1},
    {mnem: 'xor', opnds: ['r/m8', 'imm8'], opCode: [0x80], opExt: 6},
    {mnem: 'xor', opnds: ['r/m16', 'imm16'], opCode: [0x81], opExt: 6, szPref: true},
    {mnem: 'xor', opnds: ['r/m32', 'imm32'], opCode: [0x81], opExt: 6},
    {mnem: 'xor', opnds: ['r/m64', 'imm32'], opCode: [0x81], opExt: 6, REX_W: 1},
    {mnem: 'xor', opnds: ['r/m16', 'imm8'], opCode: [0x83], opExt: 6, szPref: true},
    {mnem: 'xor', opnds: ['r/m32', 'imm8'], opCode: [0x83], opExt: 6},
    {mnem: 'xor', opnds: ['r/m64', 'imm8'], opCode: [0x83], opExt: 6, REX_W: 1},
    {mnem: 'xor', opnds: ['r/m8', 'r8'], opCode: [0x30]},
    {mnem: 'xor', opnds: ['r/m16', 'r16'], opCode: [0x31], szPref: true},
    {mnem: 'xor', opnds: ['r/m32', 'r32'], opCode: [0x31]},
    {mnem: 'xor', opnds: ['r/m64', 'r64'], opCode: [0x31], REX_W: 1},
    {mnem: 'xor', opnds: ['r8', 'r/m8'], opCode: [0x32]},
    {mnem: 'xor', opnds: ['r16', 'r/m16'], opCode: [0x33], szPref: true},
    {mnem: 'xor', opnds: ['r32', 'r/m32'], opCode: [0x33]},
    {mnem: 'xor', opnds: ['r64', 'r/m64'], opCode: [0x33], REX_W: 1},
];

