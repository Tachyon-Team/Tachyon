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
Utility code to facilitate debugging.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/** Throw an exception with message and args */
function error(message)
{
    var errMsg = '';
    for (var i = 0; i < arguments.length; ++i)
    {
        errMsg += arguments[i];
    }

    var errObj = new Error(errMsg);

    // If we are running within V8, capture a stack trace
    if (Error.captureStackTrace !== undefined)
        Error.captureStackTrace(errObj);

    throw errObj;
};

/**
Retrow an error with added information
*/
function rethrowError(exc, message)
{
    if (message === undefined)
        throw exc;

    var errMsg = '';
    for (var i=1; i < arguments.length; ++i)
    {
        errMsg += arguments[i];
    }

    errMsg += ':\n' + exc;

    error(errMsg);
}

/** Ensure a boolean condition is met, otherwise throw an exception */
function assert(bool, message)
{
    if (!bool) 
    { 
        error(message);
    }
};

/** 
Tells if an object corresponds to the global object for the current
execution context.
*/
function isGlobal(obj)
{
    // Retrieve the current global object 
    var global = (function () { return this; })();

    return obj === global;
}

/** 
Ensure the new operator has been called for this particular constructor
*/
function assertNew(obj)
{
    assert(!isGlobal(obj), 
           "Constructor has been called without the new operator");
};

/** 
Ensure a received exception is equal to an expected exception, 
otherwise rethrow the exception.
*/
function assertExceptionEqual(expected, received)
{
    var exMsg = expected.message? expected.message:expected.toString();
    var reMsg = received.message? received.message:received.toString();

    if (exMsg !== reMsg)
    {
        throw received;
    }
}

