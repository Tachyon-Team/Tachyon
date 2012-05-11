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
Tests for iterators.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

(function () { // local namespace
    tests.utility = tests.utility || tests.testSuite(); 
    tests.utility.iterators = tests.testSuite();

    var t = tests.utility.iterators;

    t.setup = function ()
    {
        t.a = [1,2,3,4];
    };

    t.iterator = function ()
    {
        function assertUnimplemented(obj, method)
        {
            var flag = false;
            try
            {
                obj[method]();
                flag = true;
            } 
            catch (e)
            {
                assertExceptionEqual("unimplemented", e);
            }

            if (flag)
            {
                error("failed");
            }
        }

        var it = new Iterator(); 

        assertUnimplemented(it, "next");
        assertUnimplemented(it, "valid");
        assertUnimplemented(it, "get");
        
    };

    t.arrayItr = function ()
    {
        var it = new ArrayIterator(t.a);
        var i;

        for (i=0; i < t.a.length; ++i)
        {
            assert(it.get() === t.a[i]);
            it.next();
        }

        assert(!it.valid());
    };

    t.filterItr = function ()
    {
        function even (n) { return (n % 2) === 0; };
        var it = new FilterIterator(new ArrayIterator(t.a), even);

        var i;

        for (i=1; i < t.a.length; i=i+2)
        {
            assert(it.get() === t.a[i]);
            it.next();
        }

        assert(!it.valid());
    };

})(); // end of local namespace 
