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
Tests for asm related functions.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/
(function () { // local namespace

    tests.asm = tests.testSuite();
    tests.asm.address = tests.testSuite();
    const t = tests.asm.address; 

    t.identity = function ()
    {
        const byteArray = [255,255,255,1];
        const addr = asm.address(byteArray);
        const byteArray2 = addr.getBytes();

        const bigEndianByteArray = byteArray.slice(0).reverse();
        const bigEndianAddr = asm.address(bigEndianByteArray, true);
        const bigEndianByteArray2 = bigEndianAddr.getBytes();

        for (var i=0; i<byteArray.length; ++i)
        {
            assert(byteArray[i] === byteArray2[i]);
            assert(bigEndianByteArray[i] === bigEndianByteArray2[i]);
        }
    };

    t.creation = function ()
    {
        
        const byteArray32 = [1,0,0,0];
        const addr32 = asm.address(byteArray32);
        assert(addr32.width() === 32);

        const byteArray64 = [1,0,0,0,0,0,0,0];
        const addr64 = asm.address(byteArray64);
        assert(addr64.width() === 64);
    };

    t.cmp  = function () 
    {
        const addr1 = asm.address([3,0,0,0]);
        const addr2 = asm.address([1,0,0,0]);
        const addr3 = asm.address([1,0,0,0]);

        assert(addr2.cmp(addr1) === -1);
        assert(addr1.cmp(addr2) === 1);
        assert(addr2.cmp(addr3) === 0);
        assert(addr3.cmp(addr2) === 0);
    };

    t.copy = function ()
    {
        const byteArray = [1,0,0,0];
        const addr = asm.address(byteArray);
        const addr2 = addr.copy();

        assert(addr.cmp(addr2) === 0);

    };

    t.addOffset = function ()
    {
        const addr1 = asm.address([255,255,255,1]);
        const addr2 = asm.address([0,0,0,2]);
        assert(addr1.addOffset(1).cmp(addr2) === 0);
        assert(addr2.addOffset(-1).cmp(addr1) === 0);
    };

    t.subOffset = function ()
    {
        const addr1 = asm.address([255,255,255,1]);
        const addr2 = asm.address([0,0,0,2]);
        assert(addr1.subOffset(-1).cmp(addr2) === 0);
        assert(addr2.subOffset(1).cmp(addr1) === 0);
    };

    t.addAddress = function ()
    {
        const addr1 = asm.address([255,255,255,1]);
        const addr2 = asm.address([1,0,0,0]);
        const addr3 = asm.address([0,0,0,2]);
        assert(addr1.addAddress(addr2).cmp(addr3) === 0);

    };

    t.complement = function ()
    {
        const addr1 = asm.address([255,255,255,1]);
        const addr2 = asm.address([0,0,0,2]);
        const addr3 = asm.address([1,0,0,0]);
        assert(addr1.complement().complement().cmp(addr1) === 0);
        assert(addr2.addAddress(addr3.complement()).cmp(addr1) === 0);
    };

    t.addrOffsetBytes = function ()
    {
        const addr1 = asm.address([255,255,255,1]);
        const addr2 = asm.address([0,0,0,2]);
        const offset1 = addr1.getAddrOffsetBytes(addr2);
        const offset2 = addr2.getAddrOffsetBytes(addr1);
        const expected1  = [1,0,0,0];
        const expected2 = [255,255,255,255];
        var i;

        for (i=0; i<offset1.length; ++i)
        {
            assert(offset1[i] === expected1[i]);
            assert(offset2[i] === expected2[i]);
        }
    };
    

})(); // end of local namespace
