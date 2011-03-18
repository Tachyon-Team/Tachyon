/**
@fileOverview
Utility code for running and testing programs as unit tests.

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
Compile and run a source file, returning the result.
*/
function compileAndRunSrcs(srcFiles, funcName, inputArgs, compParams)
{
    var argTypes = [];
    for (var i = 0; i < inputArgs.length; ++i)
    {
        assert (
            isInt(inputArgs[i]),
            'only integer arguments supported for now'
        );
        argTypes.push(new CIntAsBox());
    }

    if (compParams === undefined)
        compParams = 'clientParams';

    var params = config[compParams];

    assert (
        params instanceof CompParams,
        'invalid compilation parameters'
    );

    // For each source file
    for (var i = 0; i < srcFiles.length; ++i)
    {
        var srcFile = srcFiles[i];

        // Compile the unit
        var ir = compileSrcFile(srcFile, params);

        // Create a bridge to execute this unit
        var unitBridge = makeBridge(
            ir,
            config.hostParams,
            [],
            new CIntAsBox()
        );

        // Execute the compilation unit to initialize it
        unitBridge(params.ctxPtr);

        // Try to find the function of the specified name in the unit
        var func = ir.getChild(funcName);

        if (func !== null)
            var funcIR = func;
    }

    assert (
        funcIR !== null,
        'test function not found'
    );

    var funcBridge = makeBridge(
        funcIR,
        config.hostParams,
        argTypes,
        new CIntAsBox()
    );

    // Call the function with the given arguments
    var result = funcBridge.apply(undefined, [params.ctxPtr].concat(inputArgs));

    return result;
}

/**
Generate a unit test for a source file, testing the return value
obtained after execution.
*/
function genTest(srcFiles, funcName, inputArgs, expectResult, compParams)
{
    if (typeof srcFiles === 'string')
        srcFiles = [srcFiles];

    return function()
    {
        var result = compileAndRunSrcs(
            srcFiles, 
            funcName,
            inputArgs,
            compParams
        );

        assert (
            result === expectResult,
            'Invalid return value "' + result + 
            '", expected "' + expectResult + '"'
        );
    };
}

/**
Test suite for test programs.
*/
tests.programs = tests.testSuite();

/**
Value return test.
*/
tests.programs.basic_ret = genTest(
    'programs/basic_ret/basic_ret.js', 
    'f', 
    [20], 
    20
);

/**
If statement test.
*/
tests.programs.basic_if = genTest(
    'programs/basic_if/basic_if.js', 
    'f', 
    [],
    2
);

/**
Argument passing test.
*/
tests.programs.basic_many_args = genTest(
    'programs/basic_many_args/basic_many_args.js', 
    'f',
    [0,0,0,0,20],
    20
);

/**
Comparison operators test
*/
tests.programs.basic_cmp = genTest(
    'programs/basic_cmp/basic_cmp.js',
    'test',
    [5],
    0
);

/**
Arithmetic operators test.
*/
tests.programs.basic_arith = genTest(
    'programs/basic_arith/basic_arith.js',
    'test',
    [],
    0
);

/**
Bitwise operators test.
*/
tests.programs.basic_bitops = genTest(
    'programs/basic_bitops/basic_bitops.js',
    'test',
    [],
    0
);

/**
Arithmetic shift test.
*/
tests.programs.basic_shift = genTest(
    'programs/basic_shift/basic_shift.js',
    'foo',
    [],
    0
);

/**
Test of limits of integer overflow handling.
*/
tests.programs.basic_ovf = genTest(
    'programs/basic_ovf/basic_ovf.js',
    'proxy',
    [],
    0,
    'hostParams'
);

/**
Test of basic optimization patterns.
*/
tests.programs.basic_opts = genTest(
    'programs/basic_opts/basic_opts.js',
    'proxy',
    [],
    0,
    'hostParams'
);

/**
Test of assignment expressions.
*/
tests.programs.basic_assign = genTest(
    'programs/basic_assign/basic_assign.js',
    'proxy',
    [],
    0,
    'clientParams'
);

/**
Test of boolean value evaluation.
*/
tests.programs.basic_bool_eval = genTest(
    'programs/basic_bool_eval/basic_bool_eval.js',
    'test',
    [],
    0,
    'clientParams'
);

/**
Multiple files/units test
*/
tests.programs.multi_file = genTest(
    ['programs/multi_file/file1.js', 'programs/multi_file/file2.js'],
    'test',
    [],
    0
);

/**
Test of typed IIR variables.
*/
tests.programs.iir_vars = genTest(
    'programs/iir_vars/iir_vars.js',
    'test',
    [],
    0,
    'hostParams'
);

/**
Passing arguments and getting a return value from an FFI function
*/
tests.programs.ffi_sum = genTest(
    'programs/ffi_sum/ffi_sum.js',
    'f',
    [10,15],
    25,
    'hostParams'
);

/**
This test is meant to ensure that values are correctly merged after 
conditionals and that local variable values are properly preserved across
calls.
*/
tests.programs.cond_calls = genTest(
    'programs/cond_calls/cond_calls.js',
    'fee',
    [],
    20
);

/**
Test of multiple function calls with computations in between.
*/
tests.programs.two_calls = genTest(
    'programs/two_calls/two_calls.js',
    'foo',
    [],
    39
);

/**
Fibonacci implementation to test recursive calls.
*/
tests.programs.fib = genTest(
    'programs/fib/fib.js',
    'fib',
    [20],
    6765
);

/**
Test of a loop computing a sum.
*/
tests.programs.loop_sum = genTest(
    'programs/loop_sum/loop_sum.js',
    'loop_sum',
    [10],
    45
);

/**
Test of a function call followed by a loop.
*/
tests.programs.call_loop = genTest(
    'programs/call_loop/call_loop.js',
    'foo',
    [],
    15
);

/**
Test of function calls before, inside and after a loop.
*/
tests.programs.loop_calls = genTest(
    'programs/loop_calls/loop_calls.js',
    'foo',
    [1],
    14338
);

/**
Test of two loops, one after the other, each performing function calls.
*/
tests.programs.loop_loop = genTest(
    'programs/loop_loop/loop_loop.js',
    'foo',
    [5],
    60
);

/**
Test of a for loop inside a while loop.
*/
tests.programs.while_for = genTest(
    'programs/while_for/while_for.js',
    'foo',
    [5],
    225
);

/**
Loop with enough variables to force spilling of phi nodes.
*/
tests.programs.loop_spills = genTest(
    'programs/loop_spills/loop_spills.js',
    'foo',
    [42],
    122
);

/**
Nested loops unit test.
*/
tests.programs.nested_loops = genTest(
    'programs/nested_loops/nested_loops.js',
    'foo',
    [3],
    503
);

/**
Object property put/get unit test.
*/
tests.programs.obj_props = genTest(
    'programs/obj_props/obj_props.js',
    'foo',
    [33],
    1584
);

/**
Linked list unit test.
*/
tests.programs.linked_list = genTest(
    'programs/linked_list/linked_list.js',
    'linkedlist',
    [5],
    10
);

/**
String equality and non-equality.
*/
tests.programs.str_equality = genTest(
    'programs/str_equality/str_equality.js',
    'foo',
    [],
    0
);

/**
String concatenation with another string.
*/
tests.programs.str_cat_str = genTest(
    'programs/str_cat_str/str_cat_str.js',
    'foo',
    [],
    0
);

/**
String concatenation with integers.
*/
tests.programs.str_cat_str = genTest(
    'programs/str_cat_int/str_cat_int.js',
    'foo',
    [],
    0
);

/**
Array indexing test.
*/
tests.programs.array_idx = genTest(
    'programs/array_idx/array_idx.js',
    'foo',
    [12],
    132
);

/**
Array length property test.
*/
tests.programs.array_length = genTest(
    'programs/array_length/array_length.js',
    'foo',
    [],
    0
);

/**
Array size extension test.
*/
tests.programs.array_ext = genTest(
    'programs/array_ext/array_ext.js',
    'test',
    [],
    0
);

/**
Recursive n-queens solver. Uses arrays extensively.
*/
tests.programs.nqueens = genTest(
    'programs/nqueens/nqueens.js',
    'test',
    [],
    0
);

/**
Closure variable initialization.
*/
tests.programs.clos_init = genTest(
    'programs/clos_init/clos_init.js',
    'test',
    [],
    0
);

/**
Closure variable capture.
*/
tests.programs.clos_capt = genTest(
    'programs/clos_capt/clos_capt.js',
    'foo',
    [5],
    8
);

/**
Closure variable access.
*/
tests.programs.clos_access = genTest(
    'programs/clos_access/clos_access.js',
    'test',
    [],
    0
);

/**
Calls across closure boundaries.
*/
tests.programs.clos_xcall = genTest(
    'programs/clos_xcall/clos_xcall.js',
    'test',
    [5],
    5
);

/**
Closure and global variable test.
*/
tests.programs.clos_globals = genTest(
    'programs/clos_globals/clos_globals.js',
    'test',
    [],
    0
);

/**
Closure capturing an argument and global variable access test.
*/
tests.programs.clos_arg = genTest(
    'programs/clos_arg/clos_arg.js',
    'test',
    [],
    0
);

/**
Constructor/new test.
*/
tests.programs.ctor_new = genTest(
    'programs/ctor_new/ctor_new.js',
    'foo',
    [5],
    6
);

/**
Constructor, prototype and methods test.
*/
tests.programs.ctor_proto = genTest(
    'programs/ctor_proto/ctor_proto.js',
    'test',
    [5],
    9
);

/**
Constructor and instanceof test
*/
tests.programs.ctor_instof = genTest(
    'programs/ctor_instof/ctor_instof.js',
    'test',
    [],
    0
);

/**
Constructor, prototype and toString method test.
*/
tests.programs.obj_tostring = genTest(
    'programs/obj_tostring/obj_tostring.js',
    'test',
    [],
    0
);

/**
Test of the use of objects as properties.
*/
tests.programs.obj_objprops = genTest(
    'programs/obj_objprops/obj_objprops.js',
    'test',
    [],
    0
);

/**
Bubble-sort implementation. Uses closures and string conversion of arrays.
*/
tests.programs.bubble_sort = genTest(
    'programs/bubble_sort/bubble_sort.js',
    'test',
    [],
    0
);

/**
Property deletion on objects.
*/
tests.programs.obj_delete = genTest(
    'programs/obj_delete/obj_delete.js',
    'test',
    [],
    0
);

/**
For-in loop, property enumeration.
*/
tests.programs.for_in = genTest(
    'programs/for_in/for_in.js',
    'test',
    [],
    0
);

/**
Variable number of arguments test.
*/
tests.programs.var_args = genTest(
    'programs/var_args/var_args.js',
    'foo_proxy',
    [],
    0
);

/**
Arguments object test.
*/
tests.programs.arg_obj = genTest(
    'programs/arg_obj/arg_obj.js',
    'foo_proxy',
    [],
    0
);

/**
Function apply test.
*/
tests.programs.apply = genTest(
    'programs/apply/apply.js',
    'foo_proxy',
    [],
    0
);

/**
Matrix computations, uses arrays, constructors, strings, closures.
*/
tests.programs.matrix_comp = genTest(
    'programs/matrix_comp/matrix_comp.js',
    'test',
    [],
    10
);

/**
Standard conformance test for comparison operators.
*/
tests.programs.es5_cmp = genTest(
    'programs/es5_cmp/es5_cmp.js',
    'test',
    [],
    0
);

/**
Standard library function code tests.
*/
tests.programs.stdlib_functions = genTest(
    'programs/stdlib_functions/stdlib_functions.js',
    'test',
    [],
    0
);

/**
Standard library math code tests.
*/
tests.programs.stdlib_math = genTest(
    'programs/stdlib_math/stdlib_math.js',
    'test',
    [],
    0
);

/**
Standard library array code tests.
*/
tests.programs.stdlib_arrays = genTest(
    'programs/stdlib_arrays/stdlib_arrays.js',
    'test',
    [],
    0
);

/**
Standard library string code tests.
*/
tests.programs.stdlib_strings = genTest(
    'programs/stdlib_strings/stdlib_strings.js',
    'test',
    [],
    0
);

/**
Tachyon hash map utility code test
*/
tests.programs.tachyon_hashmap = genTest(
    [
        'utility/debug.js',
        'utility/hashmap.js',
        'programs/tachyon_hashmap/tachyon_hashmap.js'
    ],
    'test',
    [],
    0
);

/**
Tachyon graph utility code test
*/
tests.programs.tachyon_graph = genTest(
    [
        'utility/debug.js',
        'utility/iterators.js',
        'utility/arrays.js',
        'utility/graph.js',
        'programs/tachyon_graph/tachyon_graph.js'
    ],
    'test',
    [],
    0/*,
    'clientDebugParams'*/
);

/**
Tachyon bignum utility code test
*/
tests.programs.tachyon_num = genTest(
    [
        'utility/debug.js',
        'utility/num.js',
        'utility/misc.js',
        'programs/tachyon_num/tachyon_num.js'
    ],
    'test',
    [],
    0
);

/**
Tachyon machine code block test code.
*/
tests.programs.tachyon_mcb = genTest(
    [
        'utility/debug.js',
        'platform/mcb.js',
        'programs/tachyon_mcb/tachyon_mcb.js'
    ],
    'test',
    [],
    0,
    'hostParams'
);

/**
Print the state of the Tachyon VM.
*/
tests.programs.tachyon_state = genTest(
    'programs/tachyon_state/tachyon_state.js',
    'printState',
    [],
    0,
    'hostParams'
);

