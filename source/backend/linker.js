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
Code block linker implementation.

@author
Maxime Chevalier-Boisvert
*/

/**
Link the imported references in a code block
*/
function linkCode(codeBlock, backend, params)
{
    assert (
        codeBlock instanceof CodeBlock,
        'invalid code block'
    );

    // For each imported value in the code block
    for (var i = 0; i < codeBlock.imports.length; ++i)
    {
        var import = codeBlock.imports[i];

        // Get a reference to the value
        var value = import.value;

        // Set the write position
        codeBlock.setWritePos(import.pos);

        // If this is a static function reference
        if (value instanceof IRFunction)
        {
            // If the function has no compiled code block
            if ((value.codeBlock instanceof CodeBlock) === false)
            {
                log.debug('function not linked: ' + value.getValName());
                continue;
            }

            log.debug('*** linking func: ' + value.getValName());

            assert (
                value.codeBlock instanceof CodeBlock,
                'invalid function code block'
            );

            // Get the default entry point address
            var entryAddr = value.codeBlock.getExportAddr('ENTRY_DEFAULT');

            // Write the entry point address
            codeBlock.writeBytes(entryAddr);
        }
        
        // If this is a C function reference
        else if (value instanceof CFunction)
        {
            log.debug('*** linking C func: ' + value.getValName());

            assert (
                value.funcPtr instanceof Array,
                'invalid function pointer'
            );

            // Write the function address
            codeBlock.writeBytes(value.funcPtr);
        }

        // If this is a string value
        else if (value instanceof ConstValue && value.isString())
        {
            // If strings cannot yet be linked, do nothing
            if ((params.getStrObj instanceof Function) == false)
                continue;

            // Get the address for this string
            var stringAddr = params.getStrObj(value.value);

            // Write the string address
            codeBlock.writeBytes(stringAddr);
        }

        else
        {
            error('invalid link value');
        }
    }
}

