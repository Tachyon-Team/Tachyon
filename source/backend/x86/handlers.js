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
Handlers for the x86 backend.

@author
Maxime Chevalier-Boisvert
*/

/**
x86 namespace
*/
var x86 = x86 || {};

/**
Handler generator functions
*/
x86.handlerGens = {};

/**
Get the code block for a given handler. This will lazily compile the
handler if needed.
*/
x86.getHandler = function (handlerName, params)
{
    assert (
        handlerName in x86.handlerGens,
        'invalid handler name: "' + handlerName + '"'
    );

    const backend = params.backend;

    // If code was already generated for this handler, return it
    if (backend.handlers[handlerName] !== undefined)
        return backend.handlers[handlerName];

    // Create an assembler to generate code into
    var asm = new x86.Assembler(backend.x86_64);

    // Get the generator function for the handler
    var genFunc = x86.handlerGens[handlerName];

    // Generate the handler code
    genFunc(asm, params, backend);

    // Run the peephole optimizer
    x86.optimize(assembler);

    // Assemble the code into an executable code block
    var codeBlock = assembler.assemble();

    // Memoize the generated code block
    backend.handlers[handlerName] = codeBlock;

    // Return the code block
    return codeBlock;
}

/**
Handler generator for the arguments object handler
*/
x86.handlerGens['ARG_OBJ_HANDLER'] = function (asm, params, backend)
{
    /*
    Arguments object creation depends on:
    - 32/64 bit
    - Calling convention
    - Create arg obj function
    */

    // TODO
}

/**
Handler generator for the argument normalization handler
*/
x86.handlerGens['ARG_NORM_HANDLER'] = function (asm, params, backend)
{
    /*
    Perhaps using a left-to-right stack argument order for the Tachyon calling
    convention would make more sense. That way, extra arguments can easily be
    removed.

    If there are more than 254 arguments, can set cl to 255 and push the
    argument count on top of the stack. It can then easily be popped from
    there into a register.

    Input argument: number of arguments expected
    */

    // TODO






    // TODO: remove the extended arg count from the stack if present

}

