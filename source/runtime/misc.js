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
Misc. Tachyon-specific parameters and constants.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Constant to indicate we are running in Tachyon. This will be false
under d8, true under Tachyon. Note that when running under Tachyon,
this becomes a static binding, and does not require global variable
access.
*/
const RUNNING_IN_TACHYON = false;

/**
Constant to indicate the bootstrap generation number. This will be 0
under d8, and incremented by one for every level of bootstrap. Note
that when running under Tachyon, this becomes a static binding, and
does not require global variable access.
*/
const TACHYON_GEN_NUMBER = 0;

/**
Constant to indicate that we are running in debug mode. This will be
true under d8, and change depending on the bootstrap configuration.
Note that when running under Tachyon, this becomes a static binding,
and does not require global variable access.
*/
const DEBUG = true;

/**
Constant to indicate that we are running on a 64-bit platform.
Note that when running under Tachyon, this becomes a static binding,
and does not require global variable access.
*/
const PLATFORM_64BIT = RUNNING_IN_TACHYON? false:hostIs64bit();

/**
Create the Tachyon-specific constants
*/
function makeTachyonConsts(params)
{
    // Set the running in Tachyon constant to true
    params.staticEnv.regBinding(
        'RUNNING_IN_TACHYON', 
        ConstValue.getConst(true)
    );

    // Compute the bootstrap generation number
    params.staticEnv.regBinding(
        'TACHYON_GEN_NUMBER',
        ConstValue.getConst(
            TACHYON_GEN_NUMBER + 1,
            IRType.box
        )
    );

    // Bind the debug flag constant
    params.staticEnv.regBinding(
        'DEBUG',
        ConstValue.getConst(
            params.debug,
            IRType.box
        )
    );

    // Bind the 64-bit platform constant
    params.staticEnv.regBinding(
        'PLATFORM_64BIT',
        ConstValue.getConst(
            params.staticEnv.getValue('PTR_NUM_BYTES') === 8,
            IRType.box
        )
    );
}

