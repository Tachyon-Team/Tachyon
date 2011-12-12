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
Implementation of runtime context objects

@author
Maxime Chevalier-Boisvert
*/

/**
Create the context object layout for a given architecture
*/
function makeContextLayout(params)
{
    /** 
    Static environment constants for the context object 
    */
   
    // Alignment for heap allocation 
    params.staticEnv.regBinding(
        'CTX_ALIGN',
        IRConst.getConst(
            256,
            IRType.puint
        )
    );

    /**
    Run-time context layout object.
    */
    var ctxLayout = new MemLayout("ctx", IRType.ref, undefined, params);

    // Ensure that the context pointer type is valid
    assert (
        ctxLayout.ptrType === IRType.ref,
        'invalid pointer type for context layout'
    );

    // Global object
    ctxLayout.addField(
        'globalobj',
        IRType.box,
        'null'
    );

    // Heap start pointer
    ctxLayout.addField(
        'heapstart',
        IRType.rptr
    );

    // Heap limit pointer
    ctxLayout.addField(
        'heaplimit',
        IRType.rptr
    );

    // Heap allocation pointer
    ctxLayout.addField(
        'freeptr',
        IRType.rptr
    );

    // To-space start pointer
    ctxLayout.addField(
        'tostart',
        IRType.rptr
    );

    // To-space limit pointer
    ctxLayout.addField(
        'tolimit',
        IRType.rptr
    );

    // To-space free pointer
    ctxLayout.addField(
        'tofree',
        IRType.rptr
    );

    // To-space scan pointer
    ctxLayout.addField(
        'toscan',
        IRType.rptr
    );

    // String table
    ctxLayout.addField(
        'strtbl',
        IRType.box,
        'null'
    );

    // Object prototype object
    ctxLayout.addField(
        'objproto',
        IRType.box,
        'null'
    );

    // Function prototype object
    ctxLayout.addField(
        'funcproto',
        IRType.box,
        'null'
    );

    // Array prototype object
    ctxLayout.addField(
        'arrproto',
        IRType.box,
        'null'
    );

    // Boolean prototype object
    ctxLayout.addField(
        'boolproto',
        IRType.box,
        'null'
    );

    // Number prototype object
    ctxLayout.addField(
        'numproto',
        IRType.box,
        'null'
    );

    // String prototype object
    ctxLayout.addField(
        'strproto',
        IRType.box,
        'null'
    );

    // RegExp constructor
    ctxLayout.addField(
        'regexp',
        IRType.box,
        'null'
    );

    // Range error constructor
    ctxLayout.addField(
        'rangeerror',
        IRType.box,
        'null'
    );

    // Reference error constructor
    ctxLayout.addField(
        'referror',
        IRType.box,
        'null'
    );

    // Syntax error constructor
    ctxLayout.addField(
        'syntaxerror',
        IRType.box,
        'null'
    );

    // Type error constructor
    ctxLayout.addField(
        'typeerror',
        IRType.box,
        'null'
    );

    // URI error constructor
    ctxLayout.addField(
        'urierror',
        IRType.box,
        'null'
    );

    // Profiler enabling boolean
    ctxLayout.addField(
        'profenable',
        IRType.box,
        'false'
    );

    // Profiler data
    ctxLayout.addField(
        'profdata',
        IRType.box,
        'null'
    );

    // Finalize the context layout
    ctxLayout.finalize();
}

