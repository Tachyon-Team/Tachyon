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

function test(v3, v16)
{
    var v3i = pint(v3);
    var v16i = pint(v16);

    //
    // Multiplication by powers of 2
    //    

    if (v3 * 4 !== 12)
        return 1;

    if (4 * v3 !== 12)
        return 2;

    if (v3i * pint(4) !== pint(12))
        return 3;

    if (pint(4) * v3i !== pint(12))
        return 4;

    //
    // Division by powers of 2
    //

    if (v16 / 4 !== 4)
        return 5;

    if (v16i / pint(4) !== pint(4))
        return 6;

    //
    // Addition elimination
    //

    if (v3 + 0 !== v3)
        return 7;

    if (0 + v3 !== v3)
        return 8;

    //
    // Subtraction elimination
    //

    if (v3 - 0 !== v3)
        return 9;

    //
    // Multiplication elimination
    //

    if (v3 * 0 !== 0)
        return 10;

    if (0 * v3 !== 0)
        return 11;

    if (v3 * 1 !== v3)
        return 12;

    if (1 * v3 !== v3)
        return 13;

    //
    // Division elimination
    //

    if (v3 / 1 !== v3)
        return 14;

    if (v3i / pint(1) !== v3i)
        return 15;

    //
    // Bitwise OR elimination
    //

    if ((v3 | 0) !== v3)
        return 16;

    if ((0 | v3) !== v3)
        return 17;

    //
    // Shift elimination
    //

    if (v3 << 0 !== v3)
        return 18;

    if (v3 >> 0 !== v3)
        return 19;

    if (v3 >>> 0 !== v3)
        return 20;

    //
    // Compound expressions
    //

    if (v3 * (v16 * 4) !== 192)
        return 21;

    if (v3i * (v16i * pint(4)) !== pint(192))
        return 22;

    if ((v16 * 4) * v3 !== 192)
        return 23;

    if ((v16i * pint(4)) * v3i !== pint(192))
        return 24;

    if (v3 * (v16 << 2) !== 192)
        return 25;

    if (v3i * (v16i << pint(2)) !== pint(192))
        return 26;

    if ((v16 << 2) * v3 !== 192)
        return 27;

    if ((v16i << pint(2)) * v3i !== pint(192))
        return 28;

    if (4 * (v16 << 2) !== 256)
        return 29;

    if (pint(4) * (v16i << pint(2)) !== pint(256))
        return 30;

    if (4 * (v16 << 0) !== 64)
        return 31;

    if (pint(4) * (v16i << pint(0)) !== pint(64))
        return 32;

    return 0;
}

function proxy()
{
    return test(3, 16);
}

