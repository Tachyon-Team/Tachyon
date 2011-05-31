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

//=============================================================================

// File: "js.js", Time-stamp: <2010-12-31 11:41:09 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

function main()
{
    var args = command_line();
    var opt_debug = false;
    var opt_warn = false;
    var opt_ast = false;
    var opt_profiling = true;
    var i = 0;
    var n;

    while (i < args.length)
    {
        if (args[i] === "-debug")
            opt_debug = true;
        else if (args[i] === "-warn")
            opt_warn = true;
        else if (args[i] === "-ast")
            opt_ast = true;
        else if (args[i] === "-eventrec")
            opt_profiling = true;
        else
            break;
        i++;
    }

    n = args.length - i;

    while (i < args.length)
    {
        var filename = args[i];

        if (n > 1)
        {
            print(filename + ":");
        }
        var port = new File_input_port(filename);
        var s = new Scanner(port);
        var p = new Parser(s, opt_warn);
        var prog = p.parse();
        if (prog !== null)
        {
            var normalized_prog = ast_normalize(prog, opt_debug, opt_profiling);
            if (opt_ast)
                pp(normalized_prog);
        }
        i++;
    }
}

main();

//=============================================================================
