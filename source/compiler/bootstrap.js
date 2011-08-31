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
Implementation of the compilation of Tachyon using Tachyon.

@author
Maxime Chevalier-Boisvert
*/

/**
Compile and initialize the Tachyon compiler using Tachyon
*/
function bootstrap(params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters in bootstrap'
    );

    log.trace('Beginning bootstrap (gen #' + TACHYON_GEN_NUMBER + ')');

    // Compile the runtime and standard library
    initPrimitives(params);
    initStdlib(params);

    // Get the Tachyon compiler source code
    var tachyonSrcs = getTachyonSrcs(params);
    var tachyonIRs;

    measurePerformance(
        "Compiling Tachyon",
        function ()
        {
            // Compile the Tachyon sources
            tachyonIRs = compileSrcs(tachyonSrcs, params);
        });

    reportPerformance();

    log.trace("Code bytes allocated: " + codeBytesAllocated);

    // Execute the Tachyon code units
    for (var i = 0; i < tachyonIRs.length; ++i)
    {
        log.trace('Executing unit for: "' + tachyonSrcs[i] + '"'); 
        execUnit(tachyonIRs[i], params);
    }

    log.trace('Tachyon initialization complete');

    // TODO: ask Tachyon to serialize its own heap
}

