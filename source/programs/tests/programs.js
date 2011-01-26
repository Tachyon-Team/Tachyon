/**
@fileOverview
Utility code for running and testing programs as unit tests.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Compile and run a source file, returning the result.
*/
function compileAndRunSrc(srcFile)
{
    var ir = compileSrcFile(srcFile, config.clientParams);

    var bridge = makeBridge(
        ir,
        [],
        'int'
    );

    var result = bridge(config.clientParams.ctxPtr);

    return result;
}

/**
Generate a unit test for a source file, testing the return value
obtained after execution.
*/
function genTest(srcFile, expectResult)
{
    return function()
    {
        var result = compileAndRunSrc(srcFile);

        assert (
            result === expectResult,
            'Invalid return value "' + result + 
            '", expected "' + expectResult + '"'
        );
    };
}

/**
Value return test
*/
tests.basic_ret = tests.testSuite();
tests.basic_ret.main = genTest('programs/basic_ret/basic_ret.js', 20);

/**
If statement test.
*/
tests.basic_if = tests.testSuite();
tests.basic_if.main = genTest('programs/basic_if/basic_if.js', 2);

/**
Argument passing test.
*/
tests.basic_many_args = tests.testSuite();
tests.basic_many_args.main = genTest('programs/basic_many_args/basic_many_args.js', 20);

/**
This test is meant to ensure that values are correctly merged after 
conditionals and that local variable values are properly preserved across
calls.
*/
tests.cond_calls = tests.testSuite();
tests.cond_calls.main = genTest('programs/cond_calls/cond_calls.js', 20);

/**
Test of multiple function calls with computations in between.
*/
tests.two_calls = tests.testSuite();
tests.two_calls.main = genTest('programs/two_calls/two_calls.js', 39);

/**
Fibonacci implementation to test recursive calls.
*/
tests.fib = tests.testSuite();
tests.fib.main = genTest('programs/fib/fib.js', 6765);

/**
Test of a loop computing a sum.
*/
tests.loop_sum = tests.testSuite();
tests.loop_sum.main = genTest('programs/loop_sum/loop_sum.js', 45);

/**
Test of a function call followed by a loop.
*/
tests.call_loop = tests.testSuite();
tests.call_loop.main = genTest('programs/call_loop/call_loop.js', 15);

/**
Test of function calls before, inside and after a loop.
*/
tests.loop_calls = tests.testSuite();
tests.loop_calls.main = genTest('programs/loop_calls/loop_calls.js', 14338);

/**
Test of two loops, one after the other, each performing function calls.
*/
tests.loop_loop = tests.testSuite();
tests.loop_loop.main = genTest('programs/loop_loop/loop_loop.js', 60);

/**
Nested loops unit test.
*/
tests.nested_loops = tests.testSuite();
tests.nested_loops.main = genTest('programs/nested_loops/nested_loops.js', 503);

