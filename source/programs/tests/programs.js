/**
@fileOverview
Utility code for running and testing programs as unit tests.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Compile and run a source file, returning the result.
*/
function compileAndRunSrc(srcFile, funcName, inputArgs, hostParams)
{
    var argTypes = [];
    for (var i = 0; i < inputArgs.length; ++i)
    {
        assert (
            isInt(inputArgs[i]),
            'only integer arguments supported for now'
        );
        argTypes.push('int');
    }

    if (hostParams === true)
        var params = config.hostParams;
    else
        var params = config.clientParams;

    // Compile the unit
    var ir = compileSrcFile(srcFile, params);

    // Get the function of the specified name in the unit
    var func = ir.getChild(funcName);

    var unitBridge = makeBridge(
        ir,
        [],
        'int'
    );

    var funcBridge = makeBridge(
        func,
        argTypes,
        'int'
    );

    // Execute the compilation unit to initialize it
    unitBridge(params.ctxPtr);

    // Call the function with the given arguments
    var result = funcBridge.apply(undefined, [params.ctxPtr].concat(inputArgs));

    return result;
}

/**
Generate a unit test for a source file, testing the return value
obtained after execution.
*/
function genTest(srcFile, funcName, inputArgs, expectResult, hostParams)
{
    return function()
    {
        var result = compileAndRunSrc(
            srcFile, 
            funcName,
            inputArgs,
            hostParams
        );

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
tests.basic_ret.main = genTest(
    'programs/basic_ret/basic_ret.js', 
    'f', 
    [20], 
    20
);

/**
If statement test.
*/
tests.basic_if = tests.testSuite();
tests.basic_if.main = genTest(
    'programs/basic_if/basic_if.js', 
    'f', 
    [],
    2
);

/**
Argument passing test.
*/
tests.basic_many_args = tests.testSuite();
tests.basic_many_args.main = genTest(
    'programs/basic_many_args/basic_many_args.js', 
    'f',
    [0,0,0,0,20],
    20
);

/**
This test is meant to ensure that values are correctly merged after 
conditionals and that local variable values are properly preserved across
calls.
*/
tests.cond_calls = tests.testSuite();
tests.cond_calls.main = genTest(
    'programs/cond_calls/cond_calls.js',
    'fee',
    [],
    20
);

/**
Test of multiple function calls with computations in between.
*/
tests.two_calls = tests.testSuite();
tests.two_calls.main = genTest(
    'programs/two_calls/two_calls.js',
    'foo',
    [],
    39
);

/**
Fibonacci implementation to test recursive calls.
*/
tests.fib = tests.testSuite();
tests.fib.main = genTest(
    'programs/fib/fib.js',
    'fib',
    [20],
    6765
);

/**
Test of a loop computing a sum.
*/
tests.loop_sum = tests.testSuite();
tests.loop_sum.main = genTest(
    'programs/loop_sum/loop_sum.js',
    'loop_sum',
    [10],
    45
);

/**
Test of a function call followed by a loop.
*/
tests.call_loop = tests.testSuite();
tests.call_loop.main = genTest(
    'programs/call_loop/call_loop.js',
    'foo',
    [],
    15
);

/**
Test of function calls before, inside and after a loop.
*/
tests.loop_calls = tests.testSuite();
tests.loop_calls.main = genTest(
    'programs/loop_calls/loop_calls.js',
    'foo',
    [1],
    14338
);

/**
Test of two loops, one after the other, each performing function calls.
*/
tests.loop_loop = tests.testSuite();
tests.loop_loop.main = genTest(
    'programs/loop_loop/loop_loop.js',
    'foo',
    [5],
    60
);

/**
Nested loops unit test.
*/
tests.nested_loops = tests.testSuite();
tests.nested_loops.main = genTest(
    'programs/nested_loops/nested_loops.js',
    'foo',
    [3],
    503
);

/**
Linked list unit test.
*/
tests.linked_list = tests.testSuite();
tests.linked_list.main = genTest(
    'programs/linked_list/linked_list.js',
    'linkedlist',
    [5],
    10, 
    true
);

