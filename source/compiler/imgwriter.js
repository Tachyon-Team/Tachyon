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
Implementation of Tachyon executable image generation.

@author
Maxime Chevalier-Boisvert
*/

/**
Write an image containing the compiled Tachyon machine code
*/
function writeImage(params, primIRs, libIRs, tachyonIRs)
{
    const asmFile = 'host/tachyon.S';

    var asmStr = '';

    function writeln(str)
    {
        if (str === undefined)
            str = '';

        asmStr += str + '\n';
    }

    // Preprocessor definitions
    writeln('#if !defined(__USER_LABEL_PREFIX__)');
    writeln('#define __USER_LABEL_PREFIX__');
    writeln('#endif');
    writeln();
    writeln('#define EXTERN(id) CONCAT1(__USER_LABEL_PREFIX__,id)');
    writeln('#define CONCAT1(x,y) CONCAT0(x,y)');
    writeln('#define CONCAT0(x,y) x ## y');
    writeln();
    writeln('#define PAGE_SIZE_LOG2 12');
    writeln();
    writeln('#if defined(__MACOSX__) || (defined(__APPLE__) && defined(__MACH__))');
    writeln('#define PAGE_ALIGN .align PAGE_SIZE_LOG2');
    writeln('#else');
    writeln('#define PAGE_ALIGN .align 1<<PAGE_SIZE_LOG2');
    writeln('#endif');
    writeln();

    // Zone boundaries of image exported to C
    writeln('.globl EXTERN(code_start)');
    writeln('.globl EXTERN(code_end)');
    writeln('.globl EXTERN(data_start)');
    writeln('.globl EXTERN(data_end)');
    writeln();

    // Label for Tachyon main function
    writeln('.globl EXTERN(tachyon_main)');
    writeln();

    //
    // TODO: list imported C functions
    //
    //# C functions imported to image
    //.globl EXTERN(print)
    //.globl EXTERN(gcCollect)
    //

    // Start of code section
    writeln('.text');
    writeln('PAGE_ALIGN');
    writeln('EXTERN(code_start):');
    writeln('CODE:');
    writeln();

    // TODO: implement and locate tachyon main function
    // Label of the Tachyon main function
    writeln('EXTERN(tachyon_main):');

    // FIXME: temporary
    writeln('nop');
    writeln('ret');
    writeln();

    /*
    # nop ; nop ; nop ; nop ; nop ; nop
     .byte 144,144,144,144,144,144

    # movl $gcCollect,%eax ; call *%eax
     .byte 184
     .long EXTERN(gcCollect)
     .byte 255,208

    # nop ; nop ; nop ; nop ; nop ; nop
     .byte 144,144,144,144,144,144

    # ret
     .byte 195
    */

    // End of code section
    writeln('PAGE_ALIGN');
    writeln('EXTERN(code_end):');
    writeln();

    // Start of the data section
    writeln('.data');
    writeln('PAGE_ALIGN');
    writeln('EXTERN(data_start):');
    writeln();

    /*
    DATA:
     .byte 144,144,144,144,144,144,144
     .long DATA+20
     .byte 144,144,144,144,144,144,144
     .byte 144,144,144,144,144,144,144
    */

    // End of the data section
    writeln('PAGE_ALIGN');
    writeln('EXTERN(data_end):');
    writeln();

    // Write the ASM file
    writeFile(asmFile, asmStr);
}

