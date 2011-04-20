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
Useful string-related functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Escape JavaScript strings for output
*/
function escapeJSString(input)
{
    var chars = [];

    for (var i = 0; i < input.length; ++i)
    {
        var charCode = input.charCodeAt(i);

        switch (charCode)
        {
            // '\0'
            case 0:
            chars.push(92);
            chars.push(48);
            break;

            // '\b'
            case 8:
            chars.push(92);
            chars.push(98);
            break;

            // '\t'
            case 9:
            chars.push(92);
            chars.push(116);
            break;

            // '\n'
            case 10:
            chars.push(92);
            chars.push(110);
            break;

            // '\v'
            case 11:
            chars.push(92);
            chars.push(118);
            break;

            // '\f'
            case 12:
            chars.push(92);
            chars.push(102);
            break;

            // '\r'
            case 13:
            chars.push(92);
            chars.push(114);
            break;

            // '\"'
            case 34:
            chars.push(92);
            chars.push(34);
            break;

            // '\''
            case 39:
            chars.push(92);
            chars.push(39);
            break;

            // '\r'
            case 92:
            chars.push(92);
            chars.push(92);
            break;

            default:
            {
                // If this character is in the printable ASCII range
                if (charCode >= 32 && charCode <= 126)
                {
                    chars.push(charCode);
                }
                else
                {
                    var numStr = charCode.toString(16);
                    var hexStr = '0000'.slice(numStr.length) + numStr;

                    chars.push(92);
                    chars.push(117);                    
                    for (var j = 0; j < hexStr.length; ++j)
                        chars.push(hexStr.charCodeAt(j));                               
                }
            }
        }
    }

    return String.fromCharCode.apply(null, chars);
}

/**
Escape XML strings for output
*/
function escapeXMLString(input, isHTML)
{
    var chars = [];

    function pushStr(str)
    {
        for (var i = 0; i < str.length; ++i)
            chars.push(str.charCodeAt(i));
    }

    for (var i = 0; i < input.length; ++i)
    {
        var charCode = input.charCodeAt(i);

        switch (charCode)
        {
            // &
            case 38:
            pushStr('&amp;');
            break;

            // <
            case 60:
            pushStr('&lt;');
            break;

            // >
            case 62:
            pushStr('&gt;');
            break;

            // "
            case 34:
            pushStr('&quot;');
            break;

            // '
            case 39:
            pushStr('&apos;');
            break;

            // space
            case 32:
            pushStr('&nbsp;');
            break;

            // newline
            case 10:
            if (isHTML)
                pushStr('<br />');
            else
                chars.push(charCode);
            break;

            default:
            chars.push(charCode);
        }
    }

    return String.fromCharCode.apply(null, chars);
}

/**
Indent each line of a text string
*/
function indentText(inputStr, indentStr)
{
    if (indentStr === undefined)
        indentStr = '\t';

    if (typeof inputStr !== 'string')
        inputStr = inputStr.toString();

    var chars = [];

    if (inputStr.length > 0)
    {
        for (var i = 0; i < indentStr.length; ++i)
            chars.push(indentStr.charCodeAt(i));
    }

    for (var i = 0; i < inputStr.length; ++i)
    {
        var charCode = inputStr.charCodeAt(i);

        chars.push(charCode);

        if (charCode === 10 && i !== inputStr.length - 1)
        {
            for (var j = 0; j < indentStr.length; ++j)
                chars.push(indentStr.charCodeAt(j));
        }
    }

    return String.fromCharCode.apply(null, chars);
}

/**
Pluralize a word in function of a count
*/
function pluralize(word, count)
{
    return word + ((count !== 1)? 's':'');
}

/**
Left-pad a string until it has the desired length
*/
function leftPadStr(str, pad, len)
{
    str = str.toString();

    while (str.length < len)
        str = pad + str;

    return str;
}

/**
Right-pad a string until it has the desired length
*/
function rightPadStr(str, pad, len)
{
    str = str.toString();

    while (str.length < len)
        str = str + pad;

    return str;
}

/**
Remove a given set of characters from the start and end of a string
*/
function stripStr(str, chars)
{
    if (!chars)
        chars = ' \v\t\r\n';

    var endIdx = str.length;
    while (endIdx > 0 && memberStr(str.charAt(endIdx-1), chars))
        endIdx--;

    var startIdx = 0;
    while (startIdx < endIdx && memberStr(str.charAt(startIdx), chars))
        startIdx++;

    return str.substr(startIdx, endIdx);
}

/**
Test if a character is a member of a set of characters
*/
function memberStr(ch, chars)
{
    var i = chars.length-1;

    while (i >= 0 && ch !== chars.charAt(i))
        i--;

    return i >= 0;
}

