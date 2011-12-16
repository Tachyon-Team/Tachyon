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
Low-level utility functions for the run-time.

@author
Maxime Chevalier-Boisvert
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
Get a string representation for a pointer
*/
function ptrToStr(ptr)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg ptr rptr";

    var ptrInt = iir.icast(IRType.puint, ptr);

    var str = '';

    for (var i = pint(0); i < PTR_NUM_BYTES; ++i)
    {
        var byteVal = iir.icast(IRType.pint, ptrInt & puint(0xFF));
        ptrInt >>= puint(8);

        var byteStr = getIntStr(byteVal, pint(16));
        if (byteStr.length < 2) byteStr = '0' + byteStr;

        str = byteStr + str;
    }

    str = '0x' + str;

    return str;
}

/**
Get a reference to the global object
*/
function getGlobalObj()
{
    "tachyon:static";
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

    var freePtr = get_ctx_freeptr(ctx);
    var heapStart = get_ctx_heapstart(ctx);
    var heapSizeKBs = (freePtr - heapStart) / pint(1024);

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

    var functbl = get_ctx_functbl(ctx);
    var numFuncs = iir.icast(IRType.pint, get_functbl_numfuncs(functbl));

    var globalobj = get_ctx_globalobj(ctx);
    var numGlobals = iir.icast(IRType.pint, get_obj_numprops(globalobj));

    var gcCount = iir.icast(IRType.pint, get_ctx_gccount(ctx));

    printBox('Heap size    : ' + heapSizeKB + ' KB');
    printBox('Num strings  : ' + boxInt(numStrings));
    printBox('Num functions: ' + boxInt(numFuncs));
    printBox('Num globals  : ' + boxInt(numGlobals));
    printBox('gc col. count: ' + boxInt(gcCount));
}

