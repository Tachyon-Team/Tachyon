#ifndef _TACHYON_EXTS_H
#define _TACHYON_EXTS_H

// C/C++ headers
#include <cstddef>
#include <stdint.h>

int writeFile(const char* fileName, const char* content);

char* readFile(const char* fileName);

char* shellCommand(const char* command);

char* readConsole(const char* promptStr);

int currentTimeMillis();

/*---------------------------------------------------------------------------*/

typedef intptr_t word; // must correspond to natural word width of CPU

typedef word (*mach_code_ptr)();

typedef union
{
    mach_code_ptr fn_ptr;
    uint8_t* data_ptr;
}   data_to_fn_ptr_caster;

uint8_t* allocMemoryBlock(size_t size, bool exec);

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

intptr_t tachValToInt(const TachVal& v);

void* tachValToPtr(const TachVal& v);

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

