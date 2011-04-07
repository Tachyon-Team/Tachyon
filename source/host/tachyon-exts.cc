// Tachyon headers
#include "tachyon-exts.h"

// Posix headers
#include <sys/mman.h>
#include <sys/time.h>

// C/C++ headers
#include <iostream>
#include <cassert>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

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

char* readFile(const char* fileName)
{
    FILE* inFile = fopen(fileName, "r");
    if (inFile == NULL)
    {
        printf("Error in readFile -- can't open file \"%s\"\n", fileName);
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

    fclose(inFile);

    return outStr;
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

char* readConsole(const char* promptStr)
{
    printf("%s", promptStr);

    int bufSize = 128;

    int strLen = 0;

    char* buffer = new char[bufSize];

    for (;;)
    {
        char ch = getchar();

        if (ch == EOF)
        {
            delete [] buffer;
            return NULL;
        }
        
        if (ch == '\n')
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

class Timer
{
public:

    Timer()
    {
        initTime = 0;
        initTime = getTimeSecs();
        lastTime = 0;
    }

    double getTimeSecs()
    {
        struct timeval timeVal;

        int r = gettimeofday(&timeVal, NULL);

        if (r != 0)
        {
            printf("Error in getTimeSecs\n");
            exit(0);
        }

        double curTime = (timeVal.tv_sec + timeVal.tv_usec / 1000000.0);

        double deltaTime = curTime - initTime;

        // If the clock value is updated, avoid returning
        // a time smaller than the last value
        if (deltaTime < lastTime)
        {
            return lastTime;
        }

        lastTime = deltaTime;

        return deltaTime;
    }

private:

    // Time since the initialization of the extensions
    double initTime;

    // Last time value produced
    double lastTime;

} timer;

int currentTimeMillis()
{
    double timeSecs = timer.getTimeSecs();

    int timeMs = int(timeSecs * 1000);

    return timeMs;
}

/*---------------------------------------------------------------------------*/

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
        //printf("fun ptr = %p\n", (void*)(intptr_t)funcPtr);
        //printf("ptr arg = %p\n", (void*)tachArgs[0]);
        //printf("int arg = %ld\n", (long)tachArgs[1]);
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

/*---------------------------------------------------------------------------*/

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

