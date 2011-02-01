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

    //if (funcName === 'linkedlist')
    //    params.print = print;

    // Compile the unit
    var ir = compileSrcFile(srcFile, params);

    //params.print = undefined;

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
Passing arguments and getting a return value from an FFI function
*/
tests.ffi_sum = tests.testSuite();
tests.ffi_sum.main = genTest(
    'programs/ffi_sum/ffi_sum.js',
    'f',
    [10,15],
    25,
    true
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
    20,
    true
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
Loop with enough variables to force spilling of phi nodes.
*/
tests.loop_spills = tests.testSuite(); 
tests.loop_spills.main = genTest(
    'programs/loop_spills/loop_spills.js',
    'foo',
    [42],
    122
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
Object property put/get unit test.
*/
tests.object_props = tests.testSuite();
tests.object_props.main = genTest(
    'programs/object_props/object_props.js',
    'foo',
    [33],
    1584
);

/**
Linked list unit test.
*/
tests.linked_list = tests.testSuite();
tests.linked_list.main = genTest(
    'programs/linked_list/linked_list.js',
    'linkedlist',
    [5],
    10
);

/**
String equality and non-equality.
*/
tests.str_equality = tests.testSuite(); 
tests.str_equality.main = genTest(
    'programs/str_equality/str_equality.js',
    'foo',
    [],
    0
);

/**
String concatenation with another string.
*/
tests.str_cat_str = tests.testSuite(); 
tests.str_cat_str.main = genTest(
    'programs/str_cat_str/str_cat_str.js',
    'foo',
    [],
    0
);

/**
String concatenation with integers.
*/
tests.str_cat_str = tests.testSuite(); 
tests.str_cat_str.main = genTest(
    'programs/str_cat_int/str_cat_int.js',
    'foo',
    [],
    0
);

/**
Array indexing test.
*/
tests.array_idx = tests.testSuite(); 
tests.array_idx.main = genTest(
    'programs/array_idx/array_idx.js',
    'foo',
    [12],
    132
);

/**
Array length property test.
*/
tests.array_length = tests.testSuite(); 
tests.array_length.main = genTest(
    'programs/array_length/array_length.js',
    'foo',
    [],
    0
);

/**
Closure variable capture.
*/
tests.clos_capt = tests.testSuite(); 
tests.clos_capt.main = genTest(
    'programs/clos_capt/clos_capt.js',
    'foo',
    [5],
    8
);

