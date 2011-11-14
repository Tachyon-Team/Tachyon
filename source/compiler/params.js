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
 * _________________________________________________________________________ */

/**
@fileOverview
Definition of compilation parameters.

@author
Maxime Chevalier-Boisvert
Erick Lavoie
*/

/**
@namespace Compiler initialization states
*/
const initState = {
    NO_RUNTIME: 0,
    PARTIAL_RUNTIME: 1,
    FULL_RUNTIME: 2,
    FULL_STDLIB: 3
}

/**
@class Description/grouping of compilation parameters required to compile a
       unit of source code.
*/
function CompParams(cfgObj)
{
    assert (
        cfgObj.backend instanceof Backend,
        'invalid backend object'
    );

    assert (
        cfgObj.tachyonSrc === true || cfgObj.tachyonSrc === false,
        'invalid tachyon source flag'
    );

    assert (
        cfgObj.debug === true || cfgObj.debug === false,
        'invalid debug flag'
    );

    assert (
        cfgObj.parserWarnings === true || cfgObj.parserWarnings === false,
        'invalid parser warnings flag'
    );

    assert (
        cfgObj.debugTrace === true || cfgObj.debugTrace === false,
        'invalid debug trace flag'
    );

    assert(
        typeof cfgObj.heapSize === "number",
        'invalid heapSize'
    );

    assert (
        cfgObj.staticEnv instanceof StaticEnv,
        'invalid static environment'
    );

    /**
    Backend object used to generate machine code
    @field
    */
    this.backend = cfgObj.backend;

    /**
    Flag indicating that we are compiling Tachyon source
    @field
    */
    this.tachyonSrc = cfgObj.tachyonSrc;

    /**
    Enable debug mode flag
    @field
    */
    this.debug = cfgObj.debug;

    /**
    Enable parser warnings flag
    @field
    */
    this.parserWarnings = cfgObj.parserWarnings;

    /**
    Enable generation of a debug trace
    @field
    */
    this.debugTrace = cfgObj.debugTrace;

    /**
    Heap size for runtime
    @field
     */
    this.heapSize = cfgObj.heapSize;

    /**
    Static definitions to be used during compilation
    @field
    */
    this.staticEnv = cfgObj.staticEnv;

    /**
    Compiler initialization state
    @field
    */
    this.initState = initState.NO_RUNTIME;

    /**
    Map of object memory layouts, by name
    @field
    */
    this.memLayouts = {};

    /**
    FFI functions imported by Tachyon, by name
    @field
    */
    this.ffiFuncs = {};

    /**
    Map auto-generated specialized primitives
    @field
    */
    this.specPrims = new HashMap(specHashFunc, specEqualFunc);

    /**
    Function to allocate string objects
    @field
    */
    this.getStrObj = null;

    /**
    Type propagation analysis instance
    @field
    */
    this.typeProp = new TypeProp();

    /**
    Flag to print the ASTs generated during the compilation
    @field
    */
    this.printAST = false;

    /**
    Flag to print the HIR generated during the compilation
    @field
    */
    this.printHIR = false;

    /**
    Flag to print the LIR generated during the compilation
    @field
    */
    this.printLIR = false;

    /**
    Flag to print the assembler code generated during the compilation
    @field
    */
    this.printASM = false;
}

