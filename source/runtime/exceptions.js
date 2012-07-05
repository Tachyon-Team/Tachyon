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
Implementation of the runtime part of exception support.

@author
Maxime Chevalier-Boisvert
*/

//=============================================================================
//
// Exception and stack unwinding implementation
//
//=============================================================================

/**
Unwind the stack and pass an exception value to the topmost exception handler.
*/
function throwExc(val)
{
    "tachyon:static";
    "tachyon:noglobal";

    // TODO

    iir.trace_print('exception thrown');

    error(val);
}

//=============================================================================
//
// Exception/error creating functions
//
//=============================================================================

/**
Print an error message and stop the execution
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
Create a TypeError object without a global reference.
*/
function typeError(message)
{
    "tachyon:static";
    "tachyon:noglobal";

    // FIXME: for now, no exception support, call the error function
    error(message);

    var ctor = get_ctx_typeerror(iir.get_ctx());
    throw new ctor(message);
}

/**
Create a ReferenceError object without a global reference.
*/
function refError(message)
{
    "tachyon:static";
    "tachyon:noglobal";

    // FIXME: for now, no exception support, call the error function
    error(message);

    var ctor = get_ctx_referror(iir.get_ctx());
    throw new ctor(message);
}

/**
Create a SyntaxError object without a global reference.
*/
function syntaxError(message)
{
    "tachyon:static";
    "tachyon:noglobal";

    // FIXME: for now, no exception support, call the error function
    error(message);

    var ctor = get_ctx_syntaxerror(iir.get_ctx());
    throw new ctor(message);
}

/**
Called when an FP operation is required but no support is available.
Also serves as a placeholder for the type analysis: this function is
assumed to return an unknown integer or floating-point number.
*/
function noFPSupport(funcName)
{
    "tachyon:static";
    "tachyon:noglobal";

    error("no floating-point support: " + funcName);
}

