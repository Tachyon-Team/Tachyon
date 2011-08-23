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

/*===========================================================================*/

/*
 * This file contains the extensions to the D8 executable needed by the
 * Tachyon compiler.  It implements some auxiliary functions, in particular:
 *
 * - writeFile("filename", "text")  save text to the file
 * - allocMemoryBlock(n)            allocate a machine code block of length n
 * - freeMemoryBlock(block)         free a machine code block
 * - execMachineCodeBlock(block)    execute a machine code block
 *
 * Note: a MachineCodeBlock is an array of bytes which can be accessed
 * like other JS arrays, in particular you can assign to it.  For example:
 *
 *    var block = allocMemoryBlock(2);
 *    block[0] = 0x90;  // x86 "nop"
 *    block[1] = 0xc3;  // x86 "ret"
 *    execMachineCodeBlock(block);  // execute the code
 */

/*
 * To extend D8, the file <V8>/src/d8.cc must be modified as explained
 * below, the d8-tachyon-exts.cc file must be copied (or soft linked) to
 * the <V8>/src directory, and the following command must be executed in
 * the <V8> directory:
 *
 *   % scons d8
 *
 * You should then copy the files in this directory to the <V8>/src
 * directory, or create symbolic links to the said files.
 *
 * Once this is done, add d8-tachyon-exts.cc to the list of source files
 * to be compiled found in <V8>/src/SConstruct:
 *
 *  ...
 *  SOURCES = {
 *    'all': Split("""
 *      tachyon-exts.cc         // <====== ADDED!
 *      d8-tachyon-exts.cc      // <====== ADDED!
 *      accessors.cc
 *      allocation.cc
 *      api.cc
 *      ...
 *
 * Finally, there are two modifications to make to the D8 source. Near the
 * top of <V8>/src/d8.cc, add an include for the d8-tachyon-exts.h file:
 *  
 *  ...
 *  #include "platform.h"
 *  
 *  #include "d8-tachyon-exts.h"    // <====== ADDED!
 *  
 *  namespace v8 {
 *  ...
 * 
 * Inside of the definition of the method Shell::Initialize() in <V8>/src/d8.cc
 * The code should be modified as follows:
 *
 *  void Shell::Initialize() {
 *  ...
 *
 *  global_template->Set(String::New("load"), FunctionTemplate::New(Load));
 *  global_template->Set(String::New("quit"), FunctionTemplate::New(Quit));
 *  global_template->Set(String::New("version"), FunctionTemplate::New(Version));
 *
 *  INIT_D8_EXTENSIONS;              // <====== ADDED!
 *  ...
 *  }
 */

// V8 headers
#include "v8.h"
#include "d8.h"
#include "d8-debug.h"
#include "debug.h"
#include "api.h"
#include "natives.h"
#include "platform.h"

// Tachyon headers
#include "tachyon-exts.h"

// Posix headers
#include <sys/mman.h>

// C/C++ headers
#include <cassert>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

/*---------------------------------------------------------------------------*/

v8::Handle<v8::Value> v8Proxy_hostIs64bit(const v8::Arguments& args)
{
    if (args.Length() != 0)
    {
        printf("Error in hostIs64bit -- 0 arguments expected\n");
        exit(1);
    }

    int is64bit = (sizeof(void*) == 8);

    return v8::Boolean::New(is64bit);
}

v8::Handle<v8::Value> v8Proxy_writeFile(const v8::Arguments& args)
{
    if (args.Length() != 2)
    {
        printf("Error in writeFile -- 2 arguments expected\n");
        exit(1);
    }
    else
    {
        v8::String::Utf8Value filename_str(args[0]);  
        const char* fileName = *filename_str;
        v8::String::Utf8Value content_str(args[1]);
        const char* content = *content_str;

        writeFile(fileName, content);
    }

    return v8::Undefined();
}

v8::Handle<v8::Value> v8Proxy_readFile(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in readFile -- 1 argument expected\n");
        exit(1);
    }

    v8::String::Utf8Value fileStrObj(args[0]);  
    const char* fileName = *fileStrObj;

    char* outStr = readFile(fileName);

    v8::Local<v8::String> v8Str = v8::String::New(outStr);

    free(outStr);

    return v8Str;
}

v8::Handle<v8::Value> v8Proxy_remove(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in remove -- 1 argument expected\n");
        exit(1);
    }

    v8::String::Utf8Value fileStrObj(args[0]);  
    const char* fileName = *fileStrObj;

    remove(fileName);

    return v8::Undefined();
}

v8::Handle<v8::Value> v8Proxy_shellCommand(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in shellCommand -- 1 argument expected\n");
        exit(1);
    }

    v8::String::Utf8Value cmdStrObj(args[0]);  
    const char* command = *cmdStrObj;

    char* outStr = shellCommand(command);

    v8::Local<v8::String> v8Str = v8::String::New(outStr);

    free(outStr);

    return v8Str;
}

v8::Handle<v8::Value> v8Proxy_readConsole(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in readConsole -- 1 argument expected\n");
        exit(1);
    }

    v8::String::Utf8Value prompStrObj(args[0]);  
    const char* promptStr = *prompStrObj;

    char* buffer = readConsole(promptStr);

    if (buffer != NULL)
    {
        v8::Local<v8::String> v8Str = v8::String::New(buffer);
        free(buffer);
        return v8Str;
    }
    else
    {
        return v8::Undefined();
    }
}

v8::Handle<v8::Value> v8Proxy_currentTimeMillis(const v8::Arguments& args)
{
    if (args.Length() != 0)
    {
        printf("Error in currentTimeMillis -- 0 argument expected\n");
        exit(1);
    }

    return v8::Number::New(currentTimeMillis());
}

#define ACTIVATE_HEAP_PROFILING_not

extern "C" 
{
#ifdef ACTIVATE_HEAP_PROFILING
    extern double bytes_allocated, bytes_alive_at_last_gc; // defined in src/heap.cc
#endif
}

v8::Handle<v8::Value> v8Proxy_memAllocatedKBs(const v8::Arguments& args)
{
    if (args.Length() != 0)
    {
        printf("Error in memAllocatedKBs -- 0 argument expected\n");
        exit(1);
    }

#ifdef ACTIVATE_HEAP_PROFILING

    double allocatedBytes = bytes_allocated + (v8::internal::Heap::SizeOfObjects()-bytes_alive_at_last_gc);

    int allocatedKBs = int(allocatedBytes / 1024);

    return v8::Number::New(allocatedKBs);

#else

    return v8::Number::New(0);

#endif
}

/*---------------------------------------------------------------------------*/

// Convert an array of bytes to a value
template <class T> T arrayToVal(const v8::Handle<v8::Value> value)
{
    const v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(
        value->ToObject()
    );

    assert (array->Length() == sizeof(T));

    T val;

    for (size_t i = 0; i < sizeof(T); ++i)
    {
        uint8_t* bytePtr = (uint8_t*)(&val) + i;

        const v8::Handle<v8::Value> jsVal = array->Get(i);        

        int intVal = jsVal->Int32Value();

        //printf("Reading byte %d\n", intVal);

        if (intVal < 0 || intVal > 255)
        {
            printf("Error in arrayToVal -- value outside of byte range\n");
            exit(1);
        }

        *bytePtr = intVal;
    }

    return val;
}

// Convert a value to an array of bytes
template <class T> v8::Handle<v8::Value> valToArray(T val)
{
    // Create an array to store the pointer data
    v8::Local<v8::Array> ptrArray = v8::Array::New(sizeof(val));

    // Write the value into the array, byte-per-byte
    for (size_t i = 0; i < sizeof(val); ++i) 
    {
        uint8_t* bytePtr = ((uint8_t*)&val) + i;

        //printf("Writing byte: %d\n", int(*bytePtr));

        bool r = ptrArray->Set(i, v8::Number::New(*bytePtr));

        if (!r)
        {
            printf("Error in valToArray -- Set failed\n");
            exit(1);
        }
    }

    return ptrArray;
}

v8::Handle<v8::Value> v8Proxy_allocMemoryBlock(const v8::Arguments& args)
{
    if (args.Length() != 2)
    {
        printf("Error in allocMemoryBlock -- 2 arguments expected\n");
        exit(1);
    }

    size_t allocSize = args[0]->IntegerValue();
    bool execFlag = args[1]->BooleanValue();

    //printf("Allocating memory block (%lu bytes)\n", (unsigned long)allocSize);

    uint8_t* block = allocMemoryBlock(allocSize, execFlag);

    //printf("memory allocated\n");

    v8::Handle<v8::Object> obj = v8::Object::New();
    
    int arraySize = allocSize;
    if (arraySize > i::ExternalArray::kMaxLength)
        arraySize = i::ExternalArray::kMaxLength;

    //printf("setting indexed properties\n");

    // Set the elements to be the external array.
    obj->SetIndexedPropertiesToExternalArrayData(
        block,
        v8::kExternalUnsignedByteArray,
        arraySize
    );

    // Set the real array size in a hidden property
    obj->SetHiddenValue(v8::String::New("tachyon::size"), v8::Number::New(allocSize));
        
    //printf("returning object\n");

    return obj;
}

v8::Handle<v8::Value> v8Proxy_freeMemoryBlock(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in freeMemoryBlock -- 1 argument expected\n");
        exit(1);
    }
    
    if (!args[0]->IsObject())
    {
        printf("Error in freeMemoryBlock -- invalid object passed\n");
        exit(1);
    }

    v8::Local<v8::Object> obj = args[0]->ToObject();

    uint8_t* blockPtr = (uint8_t*)obj->GetIndexedPropertiesExternalArrayData();
    size_t size = (size_t)obj->GetHiddenValue(v8::String::New("tachyon::size"))->IntegerValue();

    freeMemoryBlock(blockPtr, size);

    return v8::Undefined();
}

v8::Handle<v8::Value> v8Proxy_writeToMemoryBlock(const v8::Arguments& args)
{
    if (args.Length() != 3)
    {
        printf("Error in writeToMemoryBlock -- 3 argument expected\n");
        exit(1);
    }

    if (!args[0]->IsObject())
    {
        printf("Error in writeToMemoryBlock -- invalid object passed\n");
        exit(1);
    }

    v8::Local<v8::Object> obj = args[0]->ToObject();

    uint8_t* blockPtr = (uint8_t*)obj->GetIndexedPropertiesExternalArrayData();
    size_t size = (size_t)obj->GetHiddenValue(v8::String::New("tachyon::size"))->IntegerValue();

    size_t index = args[1]->IntegerValue();

    assert (index < size);

    int byteVal = args[2]->IntegerValue();

    assert (byteVal >= 0 && byteVal <= 255);

    writeToMemoryBlock(blockPtr, index, byteVal);

    return v8::Undefined();
}

v8::Handle<v8::Value> v8Proxy_readFromMemoryBlock(const v8::Arguments& args)
{
    if (args.Length() != 2)
    {
        printf("Error in readFromMemoryBlock -- 2 argument expected\n");
        exit(1);
    }

    if (!args[0]->IsObject())
    {
        printf("Error in readFromMemoryBlock -- invalid object passed\n");
        exit(1);
    }

    v8::Local<v8::Object> obj = args[0]->ToObject();

    uint8_t* blockPtr = (uint8_t*)obj->GetIndexedPropertiesExternalArrayData();
    size_t size = (size_t)obj->GetHiddenValue(v8::String::New("tachyon::size"))->IntegerValue();

    size_t index = args[1]->IntegerValue();

    assert (index < size);

    uint8_t byte = readFromMemoryBlock(blockPtr, index);

    return v8::Number::New(byte);
}

v8::Handle<v8::Value> v8Proxy_execMachineCodeBlock(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in execMachineCodeBlock -- 1 argument expected\n");
        exit(1);
    }

    if (!args[0]->IsObject())
    {
        printf("Error in execMachineCodeBlock -- invalid object passed\n");
        exit(1);
    }

    v8::Local<v8::Object> obj = args[0]->ToObject();

    uint8_t* blockPtr = (uint8_t*)obj->GetIndexedPropertiesExternalArrayData();

    data_to_fn_ptr_caster ptr;
    ptr.data_ptr = blockPtr;
    
    // Execute the code
    word result = ptr.fn_ptr();

    return v8::Number::New(result);
}

v8::Handle<v8::Value> v8Proxy_getBlockAddr(const v8::Arguments& args)
{
    if (args.Length() < 1 || args.Length() > 2)
    {
        printf("Error in getBlockAddr -- 1 or 2 argument expected\n");
        exit(1);
    }

    if (!args[0]->IsObject())
    {
        printf("Error in getBlockAddr -- invalid object passed\n");
        exit(1);
    }

    v8::Local<v8::Object> obj = args[0]->ToObject();

    uint8_t* blockPtr = (uint8_t*)obj->GetIndexedPropertiesExternalArrayData();
    size_t size = (size_t)obj->GetHiddenValue(v8::String::New("tachyon::size"))->IntegerValue();

    // Get the index value
    size_t idxVal = 0;
    if (args.Length() > 1)
    {
        i::Handle<i::Object> idxObj = v8::Utils::OpenHandle(*args[1]);
        idxVal = (size_t)idxObj->Number();
    }

    // Ensure that the index is valid
    if (idxVal >= size)
    {
        printf("Error in getBlockAddr -- index is past end of block\n");
        exit(1);
    }

    // Compute the address
    uint8_t* address = blockPtr + idxVal;

    return valToArray(address);
}

/*---------------------------------------------------------------------------*/

// First arg: vector of strings describing arg types
// Second arg: string describing return type
// Third arg: function pointer
// Fourth arg: context pointer
// Fifth arg: vector of args to be passed to the function
v8::Handle<v8::Value> v8Proxy_callTachyonFFI(const v8::Arguments& args)
{
    if (args.Length() != 5)
    {
        printf("Error in callTachyonFFI -- 5 or more argument expected\n");
        exit(1);
    }

    //printf("in v8Proxy_callTachyonFFI\n");

    // Get the array of argument types
    const v8::Handle<v8::Array> argTypeArray = v8::Handle<v8::Array>::Cast(
        args[0]->ToObject()
    );

    // Get the return type string
    v8::String::Utf8Value retTypeStrObj(args[1]);
    const char* retTypeStr = *retTypeStrObj;

    // Get the function pointer
    TACHYON_FPTR funcPtr = arrayToVal<TACHYON_FPTR>(args[2]);
    
    //printf("fun ptr = %p\n", (void*)(intptr_t)funcPtr);

    // Get the context pointer
    uint8_t* ctxPtr = arrayToVal<uint8_t*>(args[3]);

    // Get the argument array
    const v8::Handle<v8::Array> argArray = v8::Handle<v8::Array>::Cast(
        args[4]->ToObject()
    );

    // Get the number of call arguments
    size_t numArgs = argArray->Length();

    // Ensure that there is a type for each argument
    if (numArgs != argTypeArray->Length())
    {
        printf("Error in callTachyonFFI -- argument and type arrays must have the same length\n");
        exit(1);
    }

    // Allocate memory for the argument data
    uint8_t* argData = new uint8_t[numArgs * sizeof(TachVal)];

    // Pointer to the current argument
    uint8_t* argPtr = argData;

    //printf("v8Proxy_callTachyonFFI received %i arguments\n", int(numArgs));
    // For each argument to be passed
    for (size_t i = 0; i < numArgs; ++i)
    {
        // Get the argument object
        const v8::Handle<v8::Value> arg = argArray->Get(i);

        // If there is no argument type string for this argument
        if (!argTypeArray->Has(i))
        {
            printf("Error in callTachyonFFI -- missing argument type string\n");
            exit(1);
        }

        // Get the argument type string
        v8::String::Utf8Value argTypeStrObj(argTypeArray->Get(i));
        const char* argTypeStr = *argTypeStrObj;

        //printf("Arg type str: \"%s\"\n", argTypeStr);

        // Value for the current argument
        TachVal tachArg;

        // If this is an integer argument
        if (!strcmp(argTypeStr, "int"))
        {
            if (arg->IsNumber())
            {
                tachArg = tachValFromInt((intptr_t)arg->NumberValue());
                //printf("Arg %d = %ld\n", int(i), tachValToInt(tachArg));
            }
            else
            {
                printf("Error in callTachyonFFI -- integer arguments should be number values\n");
                exit(1);
            }
        }

        // If this is a pointer argument
        else if (!strcmp(argTypeStr, "void*") || !strcmp(argTypeStr, "char*"))
        {
            if (arg->IsArray())
            {
                tachArg = tachValFromPtr(arrayToVal<void*>(arg));
                //printf("Arg %d = %p\n", int(i), tachValToPtr(tachArg));
            }
            else
            {
                printf("Error in callTachyonFFI -- pointer arguments should be byte arrays\n");
                exit(1);
            }
        }

        // Otherwise, if an unsupported type is passed    
        else
        {
            printf("Error in callTachyonFFI -- unsupported argument type: \"%s\"\n", argTypeStr);
            exit(1);
        }

        // Copy the argument value into the argument data
        memcpy(argPtr, &tachArg, sizeof(tachArg));
        argPtr += sizeof(tachArg);
    }

    //printf("calling callTachyonFFI\n");

    // Call the function
    TachVal retVal = callTachyonFFI(
        funcPtr,
        ctxPtr,
        numArgs,
        argData
    );

    // Delete the argument data
    delete [] argData;

    // Variable for the V8 return value
    v8::Handle<v8::Value> v8RetVal;

    // If the return value is integer
    if (!strcmp(retTypeStr, "int"))
    {
        v8RetVal = v8::Number::New(tachValToInt(retVal));
    }

    // If the return value is a pointer
    else if (!strcmp(retTypeStr, "void*") || !strcmp(retTypeStr, "char*"))
    {
        v8RetVal = valToArray(tachValToPtr(retVal));
    }

    // Otherwise, if an unsupported type is passed    
    else
    {
        printf("Error in callTachyonFFI -- unsupported return type: \"%s\"\n", retTypeStr);
        exit(1);
    }

    //printf("returning from tachyonCallFFI\n");

    return v8RetVal;
}

v8::Handle<v8::Value> v8Proxy_getFuncAddr(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in getFuncAddr -- 1 argument expected\n");
        exit(1);
    }

    v8::String::Utf8Value str(args[0]);
    char* funcName = *str;

    FPTR address = getFuncAddr(funcName);

    //printf("fun ptr = %p\n", (void*)(intptr_t)address);

    return valToArray(address);
}

/*---------------------------------------------------------------------------*/

// Temp profiler extensions

v8::Handle<v8::Value> resumeV8Profile(const v8::Arguments& args)
{
    fprintf(stderr, "[PROF] Resuming profiler\n");

    fprintf(stderr, "Profiler currently deactivated\n");
    exit(0);

    // FIXME:
    //V8::ResumeProfiler();

    return v8::Undefined();
}

v8::Handle<v8::Value> pauseV8Profile(const v8::Arguments& args)
{
    fprintf(stderr, "[PROF] Pausing profiler\n");

    fprintf(stderr, "Profiler currently deactivated\n");
    exit(0);

    // FIXME:
    //V8::PauseProfiler();

    return v8::Undefined();
}

/*---------------------------------------------------------------------------*/

void init_d8_extensions(v8::Handle<v8::ObjectTemplate> global_template)
{
    initTachyonExts();

    global_template->Set(
        v8::String::New("hostIs64bit"), 
        v8::FunctionTemplate::New(v8Proxy_hostIs64bit)
    );

    global_template->Set(
        v8::String::New("writeFile"), 
        v8::FunctionTemplate::New(v8Proxy_writeFile)
    );

    global_template->Set(
        v8::String::New("readFile"), 
        v8::FunctionTemplate::New(v8Proxy_readFile)
    );

    global_template->Set(
        v8::String::New("remove"), 
        v8::FunctionTemplate::New(v8Proxy_remove)
    );

    global_template->Set(
        v8::String::New("shellCommand"), 
        v8::FunctionTemplate::New(v8Proxy_shellCommand)
    );

    global_template->Set(
        v8::String::New("readConsole"), 
        v8::FunctionTemplate::New(v8Proxy_readConsole)
    );

    global_template->Set(
        v8::String::New("currentTimeMillis"), 
        v8::FunctionTemplate::New(v8Proxy_currentTimeMillis)
    );

    global_template->Set(
        v8::String::New("memAllocatedKBs"), 
        v8::FunctionTemplate::New(v8Proxy_memAllocatedKBs)
    );

    global_template->Set(
        v8::String::New("allocMemoryBlock"), 
        v8::FunctionTemplate::New(v8Proxy_allocMemoryBlock)
    );

    global_template->Set(
        v8::String::New("freeMemoryBlock"), 
        v8::FunctionTemplate::New(v8Proxy_freeMemoryBlock)
    );

    global_template->Set(
        v8::String::New("writeToMemoryBlock"), 
        v8::FunctionTemplate::New(v8Proxy_writeToMemoryBlock)
    );

    global_template->Set(
        v8::String::New("readFromMemoryBlock"), 
        v8::FunctionTemplate::New(v8Proxy_readFromMemoryBlock)
    );

    global_template->Set(
        v8::String::New("execMachineCodeBlock"),
        v8::FunctionTemplate::New(v8Proxy_execMachineCodeBlock)
    );

    global_template->Set(
        v8::String::New("getBlockAddr"), 
        v8::FunctionTemplate::New(v8Proxy_getBlockAddr)
    );

    global_template->Set(
        v8::String::New("getFuncAddr"), 
        v8::FunctionTemplate::New(v8Proxy_getFuncAddr)
    );

    global_template->Set(
        v8::String::New("callTachyonFFI"),
        v8::FunctionTemplate::New(v8Proxy_callTachyonFFI)
    );

    global_template->Set(
        v8::String::New("resumeV8Profile"),
        v8::FunctionTemplate::New(resumeV8Profile)
    );

    global_template->Set(
        v8::String::New("pauseV8Profile"),
        v8::FunctionTemplate::New(pauseV8Profile)
    );
}

/*===========================================================================*/

