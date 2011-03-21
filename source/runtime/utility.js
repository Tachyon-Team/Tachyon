/**
@fileOverview
Low-level utility functions for the run-time.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Print an error string and stop the execution
*/
function error(errorVal)
{
    "tachyon:static";
    "tachyon:noglobal";

    /*
    printBox('*** RUN-TIME ERROR ***');
    printBox(errorStr);
    exit(0);
    */

    // TODO: call fail FFI function?
    // runtimeError(str); ?


    if (boxIsString(errorVal))
        runtimeError(errorVal, 0);
    else if (boxIsInt(errorVal))
        runtimeError(null, errorVal);
    else
        runtimeError(null, 0);
}

/**
Perform an assertion test
*/
function assert(testVal, errorVal)
{
    "tachyon:static";
    "tachyon:noglobal";

    if (!testVal)
    {
        error(errorVal);
    }
}

/**
Print values to the console
*/
function printBox(val)
{
    "tachyon:static";
    "tachyon:noglobal";

    // Convert the value to a string
    var strVal = boxToString(val);
       
    // Print the string
    puts(strVal);
}

/**
Get a reference to the global object
*/
function getGlobalObj()
{
    "tachyon:inline";
    "tachyon:noglobal";

    // Get a reference to the global object
    var globalObj = get_ctx_globalobj(iir.get_ctx());

    return globalObj;
}

/**
Test of an object is the global object
*/
function isGlobalObj(obj)
{
    "tachyon:inline";
    "tachyon:noglobal";

    // Get a reference to the global object
    var globalObj = getGlobalObj();

    // Test if our object is the global object
    return (obj === globalObj);
}

/**
Get the amount of memory allocated in KBs
*/
function memAllocatedKBs()
{
    "tachyon:static";
    "tachyon:noglobal";

    var ctx = iir.get_ctx();

    var allocPtr = get_ctx_allocptr(ctx);
    var heapStart = get_ctx_heapstart(ctx);
    var heapSizeKBs = (allocPtr - heapStart) / pint(1024);

    return boxInt(heapSizeKBs);
}

/**
Print information about the state of the Tachyon VM
*/
function printTachyonState()
{
    "tachyon:static";
    "tachyon:noglobal";

    var ctx = iir.get_ctx();

    var heapSizeKB = memAllocatedKBs();

    var strtbl = get_ctx_strtbl(ctx);
    var numStrings = iir.icast(IRType.pint, get_strtbl_numstrs(strtbl));

    var globalobj = get_ctx_globalobj(ctx);
    var numGlobals = iir.icast(IRType.pint, get_obj_numprops(globalobj));

    printBox('Heap size  : ' + heapSizeKB + ' KB');
    printBox('Num strings: ' + boxInt(numStrings));
    printBox('Num globals: ' + boxInt(numGlobals));
}

