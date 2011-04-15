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

#ifndef _TACHYON_EXTS_H
#define _TACHYON_EXTS_H

// C/C++ headers
#include <stddef.h>
#include <stdint.h>

extern int cmdArgCount;

extern char** cmdArgVals;

void initTachyonExts();

/*---------------------------------------------------------------------------*/

int getArgCount();

char* getArgVal(int argIdx);

int writeFile(const char* fileName, const char* content);

char* readFile(const char* fileName);

char* shellCommand(const char* command);

char* readConsole(const char* promptStr);

double currentTimeSecs();

int currentTimeMillis();

/*---------------------------------------------------------------------------*/

typedef intptr_t word; // must correspond to natural word width of CPU

typedef word (*mach_code_ptr)();

typedef union
{
    mach_code_ptr fn_ptr;
    uint8_t* data_ptr;
}   data_to_fn_ptr_caster;

uint8_t* allocMemoryBlock(size_t size, int exec);

void freeMemoryBlock(uint8_t* code, size_t size);

void writeToMemoryBlock(uint8_t* block, size_t index, uint8_t byteVal);

uint8_t readFromMemoryBlock(uint8_t* block, size_t index);

/*---------------------------------------------------------------------------*/

void gcCollect(void* ctxPtr);

/*---------------------------------------------------------------------------*/

// Tachyon argument/return value type definition
typedef intptr_t TachVal;

// Pointer to a Tachyon function
typedef TachVal (*TACHYON_FPTR)(void*, ...);

intptr_t tachValToInt(TachVal v);

void* tachValToPtr(TachVal v);

TachVal tachValFromInt(intptr_t i);

TachVal tachValFromPtr(void* p);

TachVal callTachyonFFI(
    TACHYON_FPTR funcPtr,
    uint8_t* ctxPtr,
    int numArgs,
    uint8_t* argData
);

/*---------------------------------------------------------------------------*/

typedef void (*FPTR)();

FPTR getFuncAddr(const char* funcName);

#endif // #ifndef _TACHYON_EXTS_H

