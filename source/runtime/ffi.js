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
Runtime functions needed for FFI interfacing.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Create/allocate a C (UTF-8) string from a string object.
*/
function boxToCString(strVal)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:ret rptr";

    // If the string value is null, return the null pointer
    if (strVal === null)
        return NULL_PTR;

    // Get the string length
    var strLen = iir.icast(IRType.pint, get_str_size(strVal));

    // Allocate memory for the C string
    var strPtr = malloc(strLen + pint(1));

    // For each character
    for (var i = pint(0); i < strLen; i++)
    {
        var ch = get_str_data(strVal, i);

        var cCh = iir.icast(IRType.i8, ch);

        iir.store(IRType.i8, strPtr, i, cCh);
    }

    // Store the null terminator
    iir.store(IRType.i8, strPtr, i, i8(0));

    return strPtr;
}

/**
Create a string object from a C (UTF-8) string pointer.
*/
function cStringToBox(strPtr)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg strPtr rptr";

    // If the string pointer is null, return the JS null value
    if (strPtr === NULL_PTR)
        return null;

    // Compute the string length
    for (var strLen = pint(0); ; strLen++)
    {
        var ch = iir.load(IRType.i8, strPtr, strLen);

        if (ch === i8(0))
            break;
    }

    // Allocate a string object
    var strObj = alloc_str(strLen);

    // For each character
    for (var i = pint(0); i < strLen; i++)
    {
        var cCh = iir.load(IRType.i8, strPtr, i);

        var ch = iir.icast(IRType.u16, cCh);

        set_str_data(strObj, i, ch);
    }

    // Compute the hash code for the new string
    compStrHash(strObj);

    // Attempt to find the string in the string table
    return getTableStr(strObj);
}

/**
Convert a raw pointer to a byte array
*/
function ptrToByteArray(ptr)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg ptr rptr";

    var array = [];
    var ptrInt = iir.icast(IRType.pint, ptr);

    for (var i = 0; i < boxInt(PTR_NUM_BYTES); ++i)
    {
        var byteVal = ptrInt & pint(0xff);

        ptrInt >>= pint(8);

        array[i] = boxInt(byteVal);
    }

    return array;
}

/**
Convert a byte array to a pointer
*/
function byteArrayToPtr(array)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:ret rptr";

    assert (
        array.length === boxInt(PTR_NUM_BYTES),
        'invalid array length in byteArrayToPtr'
    );

    var ptrInt = pint(0);

    for (var i = boxInt(PTR_NUM_BYTES) - 1; i >= 0; --i)
    {
        var byteVal = unboxInt(array[i]);

        ptrInt = (ptrInt << pint(8)) + byteVal;

        array[i] = boxInt(byteVal);
    }

    return iir.icast(IRType.rptr, ptrInt);
}

