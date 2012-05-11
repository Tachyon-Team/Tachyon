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
Basic system/OS interface functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Store the command-line arguments in a globally visible variable.
*/
var command_line_arguments = [];

if (typeof arguments !== "undefined") /* make it work in the browser */
    command_line_arguments = arguments;
else if (typeof process !== "undefined") /* support node.js as an alternative */
    command_line_arguments = process.argv.slice(2)

/**
Get the command-line arguments
*/
function command_line()
{
    return command_line_arguments;
}

/**
Parse command-line arguments
*/
function parseCmdLine()
{
    var args = command_line();

    // Map for named arguments
    var options = {};

    // List for trailing unnamed arguments
    var files = [];

    var argIdx = 0;

    // For each named argument
    for (; argIdx < args.length; argIdx++)
    {
        var arg = args[argIdx];

        // If this is not an option argument, stop
        if (arg.charAt(0) !== '-')
            break;

        // Get the option name
        var optName = arg.slice(1);

        if (optName === 'e')
        {
            ++argIdx;
            if (argIdx >= args.length)
            {
                error("No argument specified for -e");
            }
            var optVal = args[argIdx];
        }
        else
        {
            var eqIndex = optName.indexOf("=");

            if (eqIndex === -1)
            {
                var optVal = true;
            }
            else
            {
                var optVal = optName.slice(eqIndex+1);
                optName = optName.slice(0, eqIndex);
            }
        }

        // Store the option value
        options[optName] = optVal;
    }

    // For each remaining argument
    for (; argIdx < args.length; ++argIdx)
    {
        // Add it to the file arguments
        files.push(args[argIdx]);
    }

    // Return the parsed arguments
    return {
        "options"   : options,
        "files"     : files
    };
}

