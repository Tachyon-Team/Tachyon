/*===========================================================================*/

/* File: "d8-tachyon-exts.cc", Time-stamp: <2011-03-03 16:38:58 feeley> */

/* Copyright (c) 2010 by Marc Feeley, All Rights Reserved. */
/* Copyright (c) 2010 by Maxime Chevalier-Boisvert, All Rights Reserved. */

/*===========================================================================*/

/*
 * This file contains the extensions to the D8 executable needed by the
 * Tachyon compiler.  It implements some auxiliary functions, in particular:
 *
 * - writeFile("filename", "text")  save text to the file
 * - allocMachineCodeBlock(n)       allocate a machine code block of length n
 * - freeMachineCodeBlock(block)    free a machine code block
 * - execMachineCodeBlock(block)    execute a machine code block
 *
 * Note: a MachineCodeBlock is an array of bytes which can be accessed
 * like other JS arrays, in particular you can assign to it.  For example:
 *
 *    var block = allocMachineCodeBlock(2);
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

v8::Handle<v8::Value> v8Proxy_timeCurrentMillis(const v8::Arguments& args)
{
    if (args.Length() != 0)
    {
        printf("Error in timeCurrentMillis -- 0 argument expected\n");
        exit(1);
    }

    return v8::Number::New(v8::internal::OS::TimeCurrentMillis());
}

extern "C" {
#ifdef ACTIVATE_HEAP_PROFILING
extern double bytes_allocated, bytes_alive_at_last_gc; // defined in src/heap.cc
#else
double bytes_allocated = 0, bytes_alive_at_last_gc = 0;
#endif
}

v8::Handle<v8::Value> v8Proxy_bytesAllocated(const v8::Arguments& args)
{
    if (args.Length() != 0)
    {
        printf("Error in bytesAllocated -- 0 argument expected\n");
        exit(1);
    }

    return v8::Number::New(bytes_allocated + (v8::internal::Heap::SizeOfObjects()-bytes_alive_at_last_gc));
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

uint8_t* allocMachineCodeBlock(int size)
{
    void* p = mmap(
        0,
        size,
        PROT_READ | PROT_WRITE | PROT_EXEC,
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

void freeMachineCodeBlock(uint8_t* code, int size)
{
    munmap(code, size);
}

v8::Handle<v8::Value> v8Proxy_allocMachineCodeBlock(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in allocMachineCodeBlock -- 1 argument expected\n");
        exit(1);
    }
    else
    {
        v8::Handle<v8::Object> obj = v8::Object::New();
        i::Handle<i::JSObject> jsobj = v8::Utils::OpenHandle(*obj);

        int len = args[0]->Int32Value();
        uint8_t* block = static_cast<uint8_t*>(allocMachineCodeBlock(len));

        /* Set the elements to be the external array. */
        obj->SetIndexedPropertiesToExternalArrayData(
            block,
            v8::kExternalUnsignedByteArray,
            len
        );

        return obj;
    }
}

v8::Handle<v8::Value> v8Proxy_freeMachineCodeBlock(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in freeMachineCodeBlock -- 1 argument expected\n");
        exit(1);
    }

    i::Handle<i::Object> obj = v8::Utils::OpenHandle(*args[0]);

    if (!obj->IsJSObject())
    {
        printf("Error in freeMachineCodeBlock -- invalid object passed\n");
        exit(1);
    }

    i::Handle<i::JSObject> jsObj = i::Handle<i::JSObject>::cast(obj);

    Handle<v8::internal::ExternalUnsignedByteArray> array(
        v8::internal::ExternalUnsignedByteArray::cast(jsObj->elements())
    );

    uint32_t len = static_cast<uint32_t>(array->length());
    uint8_t* block = static_cast<uint8_t*>(array->external_pointer());

    freeMachineCodeBlock(block, len);

    return Undefined();
}

v8::Handle<v8::Value> v8Proxy_execMachineCodeBlock(const v8::Arguments& args)
{
    if (args.Length() != 1)
    {
        printf("Error in execMachineCodeBlock -- 1 argument expected\n");
        exit(1);
    }
    else
    {
        i::Handle<i::JSObject> obj = i::Handle<i::JSObject>::cast(v8::Utils::OpenHandle(*args[0]));

        Handle<v8::internal::ExternalUnsignedByteArray> array(
            v8::internal::ExternalUnsignedByteArray::cast(obj->elements())
        );

        uint8_t* block = static_cast<uint8_t*>(array->external_pointer());

        data_to_fn_ptr_caster ptr;
        ptr.data_ptr = block;
    
        // Execute the code
        word result = ptr.fn_ptr();

        return v8::Number::New(result);
    }
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

v8::Handle<v8::Value> v8Proxy_getBlockAddr(const v8::Arguments& args)
{
    if (args.Length() < 1 || args.Length() > 2)
    {
        printf("Error in getBlockAddr -- 1 or 2 argument expected\n");
        exit(1);
    }

    // Get the address of the block
    i::Handle<i::JSObject> blockObj = i::Handle<i::JSObject>::cast(v8::Utils::OpenHandle(*args[0]));

    i::Handle<v8::internal::ExternalUnsignedByteArray> array(
        v8::internal::ExternalUnsignedByteArray::cast(blockObj->elements())
    );
    uint8_t* blockPtr = static_cast<uint8_t*>(array->external_pointer());
    uint32_t len = static_cast<uint32_t>(array->length());

    // Get the index value
    size_t idxVal = 0;
    if (args.Length() > 1)
    {
        i::Handle<i::Object> idxObj = v8::Utils::OpenHandle(*args[1]);
        idxVal = (size_t)idxObj->Number();
    }

    // Ensure that the index is valid
    if (idxVal >= len)
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

int tachValToInt(const TachVal& v)
{
    return v;
}

void* tachValToPtr(const TachVal& v)
{
    TachValCaster c;
    c.intVal = v;
    return c.ptrVal;
}

TachVal tachValFromInt(int i)
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
int callTachyonFFI(
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
        retVal = funcPtr(
            ctxPtr, 
            tachArgs[0],
            tachArgs[1]
        );
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
                tachArg = tachValFromInt(arg->Int32Value());
                //printf("Arg %d = %d\n", i, tachArg.intVal);
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
                //printf("Arg %d = %p\n", i, tachArg.ptrVal);
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

void printInt(int val)
{
    printf("%d\n", val);
}

int sum2Ints(int v1, int v2)
{
    return v1 + v2;
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
    else if (strcmp(funcName, "sum2Ints") == 0)
        address = (FPTR)(sum2Ints);
    else if (strcmp(funcName, "writeFile") == 0)
        address = (FPTR)(writeFile);
    else if (strcmp(funcName, "readFile") == 0)
        address = (FPTR)(readFile);
    else if (strcmp(funcName, "shellCommand") == 0)
        address = (FPTR)(shellCommand);
    else if (strcmp(funcName, "readConsole") == 0)
        address = (FPTR)(readConsole);
    else if (strcmp(funcName, "rawAllocMachineCodeBlock") == 0)
        address = (FPTR)(allocMachineCodeBlock);
    else if (strcmp(funcName, "rawFreeMachineCodeBlock") == 0)
        address = (FPTR)(freeMachineCodeBlock);
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
        v8::String::New("timeCurrentMillis"), 
        v8::FunctionTemplate::New(v8Proxy_timeCurrentMillis)
    );

    global_template->Set(
        v8::String::New("bytesAllocated"), 
        v8::FunctionTemplate::New(v8Proxy_bytesAllocated)
    );

    global_template->Set(
        v8::String::New("allocMachineCodeBlock"), 
        v8::FunctionTemplate::New(v8Proxy_allocMachineCodeBlock)
    );

    global_template->Set(
        v8::String::New("freeMachineCodeBlock"), 
        v8::FunctionTemplate::New(v8Proxy_freeMachineCodeBlock)
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

