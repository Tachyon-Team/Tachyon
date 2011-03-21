/*===========================================================================*/

/* File: "d8-tachyon-exts.cc", Time-stamp: <2011-03-15 11:21:46 feeley> */

/* Copyright (c) 2010 by Marc Feeley, All Rights Reserved. */
/* Copyright (c) 2010 by Maxime Chevalier-Boisvert, All Rights Reserved. */

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
 * There are two modifications; just before and inside of the definition of
 * the method Shell::Initialize() in <V8>/src/d8.cc .  The code should be
 * modified like this:
 *
 *  #include "d8-tachyon-exts.cc"    // <====== ADDED!
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

/*---------------------------------------------------------------------------*/

#include <cassert>

int writeFile(const char* fileName, const char* content)
{
    FILE *out = fopen(fileName, "w");
    if (out == NULL)
    {
        printf("Error in writeFile -- can't open file\n");
        exit(1);
    }
    else
    {
        fprintf(out, "%s", content);
        fclose(out);

        return 0;
    }
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

char* readFile(const char* fileName)
{
    FILE* inFile = fopen(fileName, "r");
    if (inFile == NULL)
    {
        printf("Error in readFile -- can't open file\n");
        exit(1);
    }

    char buffer[255];

    char* outStr = NULL;
    size_t strLen = 0;

    while (!feof(inFile))
    {
        int numRead = fread(buffer, 1, sizeof(buffer), inFile);

        if (ferror(inFile))
        {
            printf("Error in readFile -- failed to read file");
            exit(1);        
        }

        outStr = (char*)realloc(outStr, strLen + numRead + 1);
        memcpy(outStr + strLen, buffer, numRead);
        strLen += numRead;
    }

    outStr[strLen] = '\0';

    pclose(inFile);

    return outStr;
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

    delete [] outStr;

    return v8Str;
}

char* shellCommand(const char* command)
{
    FILE* pipeFile = popen(command, "r");

    if (!pipeFile)
    {
        printf("Error in shellCommand -- failed to execute command \"%s\"\n", command);
        exit(1);        
    }

    char buffer[255];

    char* outStr = NULL;
    size_t strLen = 0;

    while (!feof(pipeFile))
    {
        int numRead = fread(buffer, 1, sizeof(buffer), pipeFile);

        if (ferror(pipeFile))
        {
            printf("Error in shellCommand -- failed to read output");
            exit(1);        
        }

        outStr = (char*)realloc(outStr, strLen + numRead + 1);
        memcpy(outStr + strLen, buffer, numRead);
        strLen += numRead;
    }

    outStr[strLen] = '\0';

    pclose(pipeFile);

    return outStr;
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

    delete [] outStr;

    return v8Str;
}

char* readConsole(const char* promptStr)
{
    printf("%s", promptStr);

    int bufSize = 128;

    int strLen = 0;

    char* buffer = new char[bufSize];

    for (;;)
    {
        char ch = getchar();

        if (ch == EOF || ch == '\n')
            break;

        buffer[strLen] = ch;

        ++strLen;

        if (strLen >= bufSize)
        {
            bufSize *= 2;

            char* newBuf = new char[bufSize];
            memcpy(newBuf, buffer, strLen);
            delete [] buffer;
            buffer = newBuf;
        }
    }

    buffer[strLen] = '\0';

    return buffer;
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

    v8::Local<v8::String> v8Str = v8::String::New(buffer);

    delete [] buffer;

    return v8Str;
}

// Time since the initialization of the extensions
double initTime = 0;

int currentTimeMillis()
{
    double curTime = v8::internal::OS::TimeCurrentMillis() - initTime;

    return curTime;
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <sys/mman.h>

typedef intptr_t word; // must correspond to natural word width of CPU

typedef word (*mach_code_ptr)();

typedef union
{
    mach_code_ptr fn_ptr;
    uint8_t* data_ptr;
}   data_to_fn_ptr_caster;

uint8_t* allocMemoryBlock(size_t size, bool exec)
{
    void* p = mmap(
        0,
        size,
        PROT_READ | PROT_WRITE | (exec? PROT_EXEC:0),
        MAP_PRIVATE | MAP_ANON,
        -1,
        0
    );

    if (p == MAP_FAILED)
    {
        printf("alloc_machine_code_block: Could not allocate memory");
        exit(1);
    }

    return (uint8_t*)p;
}

void freeMemoryBlock(uint8_t* code, size_t size)
{
    munmap(code, size);
}

void writeToMemoryBlock(uint8_t* block, size_t index, uint8_t byteVal)
{
    block[index] = byteVal;
}

uint8_t readFromMemoryBlock(uint8_t* block, size_t index)
{
    return block[index];
}

// Convert an array of bytes to a value
template <class T> T arrayToVal(const v8::Value* arrayVal)
{
    const v8::Handle<v8::Object> jsObj = arrayVal->ToObject();

    T val;

    for (size_t i = 0; i < sizeof(T); ++i)
    {
        uint8_t* bytePtr = (uint8_t*)(&val) + i;

        if (!jsObj->Has(i))
        {
            printf("Error in arrayToVal -- array does not match value size\n");
            printf("Expected size: %i\n", int(sizeof(T)));
            printf("Failed on index: %i\n", int(i));
            exit(1);
        }

        const v8::Handle<v8::Value> jsVal = jsObj->Get(i);        

        int intVal = jsVal->Int32Value();

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
    i::Handle<i::JSArray> ptrArray = i::Factory::NewJSArray(sizeof(val));
    ASSERT(ptrArray->IsJSArray() && ptrArray->HasFastElements());

    // Write the value into the array, byte-per-byte
    for (size_t i = 0; i < sizeof(val); ++i) 
    {
        uint8_t* bytePtr = ((uint8_t*)&val) + i;
        i::Object* element = i::Smi::FromInt(*bytePtr);

        v8::internal::MaybeObject* v = ptrArray->SetFastElement(i, element);

        if (v->IsFailure())
        {
            printf("Error in valToArray -- SetFastElement failed\n");
            exit(1);
        }
    }

    return Utils::ToLocal(ptrArray);
}

/*
class MemBlock
{
public:

    uint8_t* blockPtr;
    size_t size;
    bool exec;
};
*/

v8::Handle<v8::Value> v8Proxy_allocMemoryBlock(const v8::Arguments& args)
{
    if (args.Length() != 2)
    {
        printf("Error in allocMemoryBlock -- 2 arguments expected\n");
        exit(1);
    }

    /*
    // Allocate the memory block
    size_t size = args[0]->IntegerValue();
    bool exec = args[1]->BooleanValue();
    uint8_t* blockPtr = allocMemoryBlock(size, exec);

    MemBlock* block = new MemBlock();
    block->blockPtr = blockPtr;
    block->size = size;
    block->exec = exec;

    return v8::External::Wrap(block);
    */

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
    
    /*
    assert (args[0]->IsObject());

    v8::Local<v8::Object> obj = args[0]->ToObject();
    
    uint8_t* blockPtr = (uint8_t*)obj->GetPointerFromInternalField(0);
    size_t size = obj->GetInternalField(1)->IntegerValue();

    freeMemoryBlock(blockPtr, size);
    */

    if (!args[0]->IsObject())
    {
        printf("Error in freeMemoryBlock -- invalid object passed\n");
        exit(1);
    }

    Local<Object> obj = args[0]->ToObject();

    uint8_t* blockPtr = (uint8_t*)obj->GetIndexedPropertiesExternalArrayData();
    size_t size = (size_t)obj->GetHiddenValue(v8::String::New("tachyon::size"))->IntegerValue();

    freeMemoryBlock(blockPtr, size);

    return Undefined();
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

    Local<Object> obj = args[0]->ToObject();

    uint8_t* blockPtr = (uint8_t*)obj->GetIndexedPropertiesExternalArrayData();
    size_t size = (size_t)obj->GetHiddenValue(v8::String::New("tachyon::size"))->IntegerValue();

    size_t index = args[1]->IntegerValue();

    assert (index < size);

    int byteVal = args[2]->IntegerValue();

    assert (byteVal >= 0 && byteVal <= 255);

    writeToMemoryBlock(blockPtr, index, byteVal);

    return Undefined();
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

    Local<Object> obj = args[0]->ToObject();

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

    Local<Object> obj = args[0]->ToObject();

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

    Local<Object> obj = args[0]->ToObject();

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

// Dummy GC code, to be included from another file

void gcCollect(void* ctxPtr)
{
    printf("Entering gcCollect\n");
    printf("Context pointer = %p\n", ctxPtr);



    //
    // TODO
    //




    printf("Leaving gcCollect\n");
}

/*---------------------------------------------------------------------------*/

// Simple FFI.

// Tachyon argument/return value type definition
typedef intptr_t TachVal;

union TachValCaster
{
    intptr_t intVal;
    void* ptrVal;
};

intptr_t tachValToInt(const TachVal& v)
{
    return v;
}

void* tachValToPtr(const TachVal& v)
{
    TachValCaster c;
    c.intVal = v;
    return c.ptrVal;
}

TachVal tachValFromInt(intptr_t i)
{
    return i;
}

TachVal tachValFromPtr(void* p)
{
    TachValCaster c;
    c.ptrVal = p;
    return c.intVal;
}

// Pointer to a Tachyon function
typedef TachVal (*TACHYON_FPTR)(void*, ...);

// Call a Tachyon function through its FFI
// First arg: function pointer
// Second arg: context pointer
// Third arg: number of arguments passed
// Fourth arg: argument data pointer
TachVal callTachyonFFI(
    TACHYON_FPTR funcPtr,
    uint8_t* ctxPtr,
    int numArgs,
    uint8_t* argData
)
{
    assert (
        sizeof(TachVal) == sizeof(intptr_t) &&
        sizeof(TachVal) == sizeof(void*)
    );

    // Allocate memory for the argument values
    TachVal* tachArgs = new TachVal[numArgs];

    // Pointer to the current argument
    uint8_t* argPtr = argData;

    // Read the argument values from the argument data
    for (int i = 0; i < numArgs; ++i)
    {
        memcpy(&tachArgs[i], argPtr, sizeof(TachVal));
        argPtr += sizeof(TachVal);
    }

    // Variable to store the return value
    TachVal retVal;

    //printf("Calling Tachyon func with %i arguments\n", int(numArgs));
    // Switch on the number of arguments to pass
    switch (numArgs)
    {
        case 0:
        retVal = funcPtr(
            ctxPtr
        );
        break;

        case 1:
        retVal = funcPtr(
            ctxPtr,
            tachArgs[0]
        );
        break;

        case 2:
        //printf("Calling Tachyon func with 2 arguments\n");
        retVal = funcPtr(
            ctxPtr, 
            tachArgs[0],
            tachArgs[1]
        );
        //printf("Returned from Tachyon func\n");
        break;

        case 3:
        //printf("Calling Tachyon func with 3 arguments\n");
        retVal = funcPtr(
            ctxPtr,
            tachArgs[0],
            tachArgs[1],
            tachArgs[2]
        );
        //printf("Returned from Tachyon func\n");
        break;

        case 4:
        retVal = funcPtr(
            ctxPtr,
            tachArgs[0],
            tachArgs[1],
            tachArgs[2],
            tachArgs[3]
        );
        break;

        case 5:
        retVal = funcPtr(
            ctxPtr,
            tachArgs[0],
            tachArgs[1],
            tachArgs[2],
            tachArgs[3],
            tachArgs[4]
        );
        break;

        case 6:
        retVal = funcPtr(
            ctxPtr,
            tachArgs[0],
            tachArgs[1],
            tachArgs[2],
            tachArgs[3],
            tachArgs[4],
            tachArgs[5]
        );
        break;

        default:
        printf("Error in callTachyonFFI -- unsupported argument count: %d\n", (int)numArgs);
        exit(1);
    }

    // Delete the argument objects
    delete [] tachArgs;

    return retVal;
}

// First arg: vector of strings describing arg types
// Second arg: string describing return type
// Third arg: function pointer
// Fourth arg: context pointer
// Other args: args to be passed to the function
v8::Handle<v8::Value> v8Proxy_callTachyonFFI(const v8::Arguments& args)
{
    const int MIN_ARG_COUNT = 4;

    if (args.Length() < MIN_ARG_COUNT)
    {
        printf("Error in callTachyonFFI -- %d or more argument expected\n", MIN_ARG_COUNT);
        exit(1);
    }

    // Get the array of argument types
    const v8::Local<v8::Object> argTypeArray = args[0]->ToObject();

    // Get the return type string
    v8::String::Utf8Value retTypeStrObj(args[1]);
    const char* retTypeStr = *retTypeStrObj;

    // Get the function pointer
    TACHYON_FPTR funcPtr = arrayToVal<TACHYON_FPTR>(*args[2]);
    
    // Get the context pointer
    uint8_t* ctxPtr = arrayToVal<uint8_t*>(*args[3]);

    // Get the number of call arguments
    size_t numArgs = args.Length() - MIN_ARG_COUNT;

    // Allocate memory for the argument data
    uint8_t* argData = new uint8_t[numArgs * sizeof(TachVal)];

    // Pointer to the current argument
    uint8_t* argPtr = argData;

    //printf("v8Proxy_callTachyonFFI received %i arguments\n", int(numArgs));
    // For each argument to be passed
    for (size_t i = 0; i < numArgs; ++i)
    {
        // Get the argument object
        const v8::Value* arg = *args[i + MIN_ARG_COUNT];

        // If there is no argument type string for this argument
        if (!argTypeArray->Has(i))
        {
            printf("Error in callTachyonFFI -- missing argument type string\n");
            exit(1);
        }

        // Get the argument type string
        v8::String::Utf8Value argTypeStrObj(argTypeArray->Get(i));
        const char* argTypeStr = *argTypeStrObj;

        // Value for the current argument
        TachVal tachArg;

        // If this is an integer argument
        if (!strcmp(argTypeStr, "int"))
        {
            if (arg->IsNumber())
            {
                tachArg = tachValFromInt((intptr_t)arg->NumberValue());
                //printf("Arg %d = %d\n", int(i), tachValToInt(tachArg));
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

void printInt(intptr_t val)
{
    printf("%ld\n", (long int)val);
}

void printPtr(void* ptr)
{
    printf("%p\n", ptr);
}

int sum2Ints(int v1, int v2)
{
    return v1 + v2;
}

void* testCallFFI(void* ctxPtr, void* p1, int v1)
{
    /*
    printf("self ptr: %p\n", (void*)(intptr_t)testCallFFI);
    printf("ctx ptr: %p\n", ctxPtr);
    printf("p1 ptr: %p\n", p1);

    void* val = p1;
    for (int i = 0; i < (int)sizeof(val); ++i) 
    {
        uint8_t* bytePtr = ((uint8_t*)&val) + i;
        int byteVal = *bytePtr;

        printf("p1 byte #%i = %i\n", i, byteVal);
    }

    printf("v1: %i\n", v1);
    */

    return p1;
}

void runtimeError(char* errorStr, int errorCode)
{
    printf("*** RUN-TIME ERROR ***\n");

    if (errorStr != NULL)
        printf("%s\n", errorStr);
    else
        printf("error code: %i\n", errorCode);

    exit(0);
}

typedef void (*FPTR)();

FPTR getFuncAddr(const char* funcName)
{
    FPTR address = NULL;

    if (strcmp(funcName, "malloc") == 0)
        address = (FPTR)(malloc);
    else if (strcmp(funcName, "free") == 0)
        address = (FPTR)(free);
    else if (strcmp(funcName, "exit") == 0)
        address = (FPTR)(exit);
    else if (strcmp(funcName, "puts") == 0)
        address = (FPTR)(puts);
    else if (strcmp(funcName, "printInt") == 0)
        address = (FPTR)(printInt);
    else if (strcmp(funcName, "printPtr") == 0)
        address = (FPTR)(printPtr);
    else if (strcmp(funcName, "sum2Ints") == 0)
        address = (FPTR)(sum2Ints);
    else if (strcmp(funcName, "testCallFFI") == 0)
        address = (FPTR)(testCallFFI);
    else if (strcmp(funcName, "runtimeError") == 0)
        address = (FPTR)(runtimeError);
    else if (strcmp(funcName, "writeFile") == 0)
        address = (FPTR)(writeFile);
    else if (strcmp(funcName, "readFile") == 0)
        address = (FPTR)(readFile);
    else if (strcmp(funcName, "shellCommand") == 0)
        address = (FPTR)(shellCommand);
    else if (strcmp(funcName, "readConsole") == 0)
        address = (FPTR)(readConsole);
    else if (strcmp(funcName, "currentTimeMillis") == 0)
        address = (FPTR)(currentTimeMillis);
    else if (strcmp(funcName, "rawAllocMemoryBlock") == 0)
        address = (FPTR)(allocMemoryBlock);
    else if (strcmp(funcName, "rawFreeMemoryBlock") == 0)
        address = (FPTR)(freeMemoryBlock);
    else if (strcmp(funcName, "gcCollect") == 0)
        address = (FPTR)(gcCollect);
    else if (strcmp(funcName, "rawCallTachyonFFI") == 0)
        address = (FPTR)(callTachyonFFI);
    else if (strcmp(funcName, "getFuncAddr") == 0)
        address = (FPTR)(getFuncAddr);

    if (address == NULL)
    {
        printf("Error in getFuncAddr -- C function not found: \"%s\"\n", funcName);
        exit(1);
    }

    return address;
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

    return valToArray(address);
}

// Temp profiler extensions

v8::Handle<v8::Value> resumeV8Profile(const v8::Arguments& args)
{
    fprintf(stderr, "[PROF] Resuming profiler\n");
    V8::ResumeProfiler();
    return Undefined();
}

v8::Handle<v8::Value> pauseV8Profile(const v8::Arguments& args)
{
    fprintf(stderr, "[PROF] Pausing profiler\n");
    V8::PauseProfiler();
    return Undefined();
}

/*---------------------------------------------------------------------------*/

#define INIT_D8_EXTENSIONS init_d8_extensions(global_template)

void init_d8_extensions(v8::Handle<ObjectTemplate> global_template)
{
    // Store the initialization time
    initTime = v8::internal::OS::TimeCurrentMillis();

    global_template->Set(
        v8::String::New("writeFile"), 
        v8::FunctionTemplate::New(v8Proxy_writeFile)
    );

    global_template->Set(
        v8::String::New("readFile"), 
        v8::FunctionTemplate::New(v8Proxy_readFile)
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

