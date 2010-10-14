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
