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
Utility code for running and testing programs as unit tests.

@author
Maxime Chevalier-Boisvert
*/

/**
Compile and run a source file, returning the result.
*/
function compileAndRunSrcs(srcFiles, funcName, inputArgs, compParams)
{
    var argTypes = [];

    // If input arguments are specified
    if (inputArgs !== undefined)
    {
        for (var i = 0; i < inputArgs.length; ++i)
        {
            var arg = inputArgs[i];

            if (isInt(arg))
            {
                argTypes.push(new CIntAsBox());
            }           

            else if (typeof arg === 'string')
            {
                print('Adding string arg type');

                argTypes.push(new CStringAsBox());
            }
            else
            {
                error('unsupported argument type');
            }
        }
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

    // If a function to be called was specified
    if (funcName !== undefined)
    {
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

        //print('compileAndRunSrcs, calling w/ args: ' + inputArgs);

        // Call the function with the given arguments
        var result = funcBridge.apply(undefined, [params.ctxPtr].concat(inputArgs));
    }

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
            (inputArgs === undefined && expectResult === undefined) ||
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
Runtime initialization
*/
tests.programs.initPrimitives = function ()
{
    // Reset the Tachyon configuration
    initConfig(PLATFORM_64BIT, config.verbosity);

    // Initialize the Tachyon runtime
    initPrimitives(config.hostParams);

    reportPerformance();
};

/*
// Temporary test, for debugging
tests.programs.test = genTest(
    'test_backend.js', 
    'test', 
    [], 
    0,
    'hostParams'
);
*/

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
Global object access test
*/
tests.programs.global_obj = genTest(
    'programs/global_obj/global_obj.js',
    'test',
    [],
    0,
    'hostParams'
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
    0
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
Time-related FFI functions
*/
tests.programs.ffi_time = genTest(
    'programs/ffi_time/ffi_time.js',
    'test',
    [],
    0,
    'hostParams'
);

/**
File I/O FFI functions
*/
tests.programs.ffi_fileio = genTest(
    'programs/ffi_fileio/ffi_fileio.js',
    'test',
    [],
    0,
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
tests.programs.str_cat_int = genTest(
    'programs/str_cat_int/str_cat_int.js',
    'foo',
    [],
    0
);

/**
String conversion from/to integer
*/
tests.programs.str_int_conv = genTest(
    'programs/str_int_conv/str_int_conv.js',
    'test',
    [],
    0
);

/**
Comma operator test
*/
tests.programs.comma_op = genTest(
    'programs/comma_op/comma_op.js',
    'test',
    [],
    0
);

/**
Switch statement test
*/
tests.programs.switch = genTest(
    'programs/switch/switch.js',
    'test',
    [],
    0
);

/**
With statement test
*/
tests.programs.with = genTest(
    'programs/with/with.js',
    'test',
    [],
    0
);

/**
Exceptions test
*/
tests.programs.exceptions = genTest(
    'programs/exceptions/exceptions.js',
    'test',
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
Property deletion on objects.
*/
tests.programs.obj_delete = genTest(
    'programs/obj_delete/obj_delete.js',
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
Iterative mergesort test.
*/
tests.programs.merge_sort = genTest(
    'programs/merge_sort/merge_sort.js',
    'test',
    [],
    0,
    'hostParams'
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
    0,
    'hostParams'
);

/**
Low-level apply function call test
*/
tests.programs.apply_iir = genTest(
    'programs/apply_iir/apply_iir.js',
    'test',
    [],
    0,
    'hostParams'
);

/**
Standard library initialization
*/
tests.programs.initStdlib = function ()
{
    // Initialize the Tachyon standard library
    initStdlib(config.hostParams);

    reportPerformance();
};

/**
Object access runtime primitives test.
*/
tests.programs.obj_prims = genTest(
    'programs/obj_prims/obj_prims.js',
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
Function apply test.
*/
tests.programs.apply = genTest(
    'programs/apply/apply.js',
    'foo_proxy',
    [],
    0,
    'hostParams'
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
Standard library global code tests.
*/
/*
FIXME: currently disabled
tests.programs.stdlib_global = genTest(
    'programs/stdlib_global/stdlib_global.js',
    'test',
    [],
    0
);
*/

/**
Standard library objects code tests.
*/
tests.programs.stdlib_object = genTest(
    'programs/stdlib_object/stdlib_object.js',
    'test',
    [],
    0
);

/**
Standard library function code tests.
*/
tests.programs.stdlib_function = genTest(
    'programs/stdlib_function/stdlib_function.js',
    'test',
    [],
    0
);

/**
Standard library array code tests.
*/
tests.programs.stdlib_array = genTest(
    'programs/stdlib_array/stdlib_array.js',
    'test',
    [],
    0
);

/**
Standard library boolean code tests.
*/
tests.programs.stdlib_boolean = genTest(
    'programs/stdlib_boolean/stdlib_boolean.js',
    'test',
    [],
    0
);

/**
Standard library number code tests.
*/
tests.programs.stdlib_number = genTest(
    'programs/stdlib_number/stdlib_number.js',
    'test',
    [],
    0
);

/**
Standard library string code tests.
*/
tests.programs.stdlib_string = genTest(
    'programs/stdlib_string/stdlib_string.js',
    'test',
    [],
    0
);

/**
Standard library regexp code tests.
*/
tests.programs.stdlib_regexp = genTest(
    'programs/stdlib_regexp/stdlib_regexp.js',
    'test',
    [],
    0
);

/**
Standard library json code tests.
*/
tests.programs.stdlib_json = genTest(
    'programs/stdlib_json/stdlib_json.js',
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
Simple object access performance test.
*/
tests.programs.perf_obj_access = genTest(
    'programs/perf_obj_access/perf_obj_access.js',
    'test',
    [],
    0
);

/**
Simple array access performance test.
*/
tests.programs.perf_arr_access = genTest(
    'programs/perf_arr_access/perf_arr_access.js',
    'test',
    [],
    0
);

/**
Tests for sunspider.
*/
tests.programs.sunspider = tests.testSuite();
tests.programs.sunspider['access-binary-trees'] = genTest(
    'programs/sunspider/access-binary-trees.js'
);
tests.programs.sunspider['access-fannkuch'] = genTest(
    'programs/sunspider/access-fannkuch.js'
);
tests.programs.sunspider['access-nsieve'] = genTest(
    'programs/sunspider/access-nsieve.js'
);
tests.programs.sunspider['bitops-3bit-bits-in-byte'] = genTest(
    'programs/sunspider/bitops-3bit-bits-in-byte.js'
);
tests.programs.sunspider['bitops-bits-in-byte'] = genTest(
    'programs/sunspider/bitops-bits-in-byte.js'
);
/* Only works in 64-bit for now
tests.programs.sunspider['bitops-bitwise-and'] = genTest(
    'programs/sunspider/bitops-bitwise-and.js'
);
*/
/* Only works in 64-bit for now
tests.programs.sunspider['bitops-nsieve-bits'] = genTest(
    'programs/sunspider/bitops-nsieve-bits.js'
);
*/
tests.programs.sunspider['controlflow-recursive'] = genTest(
    'programs/sunspider/controlflow-recursive.js'
);
/* Only works in 64-bit for now
tests.programs.sunspider['crypto-md5'] = genTest(
    'programs/sunspider/crypto-md5.js'
);
*/
/* Only works in 64-bit for now
tests.programs.sunspider['crypto-sha1'] = genTest(
    'programs/sunspider/crypto-sha1.js'
);
*/
/* Uses Math.random 
tests.programs.sunspider['string-base64'] = genTest(
    'programs/sunspider/string-base64.js'
);*/

/**
V8 benchmark suite
*/
tests.programs.v8bench = tests.testSuite();
/*
//FIXME: requires int32/FP support
//FIXME: requires Math.random (FP support)
tests.programs.v8bench['crypto'] = genTest(
    ['programs/v8bench/base.js',
     'programs/v8bench/crypto.js',
     'drv-crypto']
);
*/
tests.programs.v8bench['deltablue'] = genTest(
    ['programs/v8bench/base.js',
     'programs/v8bench/deltablue.js',
     'programs/v8bench/drv-deltablue.js']
);
/*
//FIXME: requires Math.random (FP support)
tests.programs.v8bench['earley-boyer'] = genTest(
    ['programs/v8bench/base.js',
     'programs/v8bench/earley-boyer.js',
     'programs/v8bench/drv-earley-boyer.js']
);
*/
/*
// FIXME: requires FP support
tests.programs.v8bench['raytrace'] = genTest(
    ['programs/v8bench/base.js',
     'programs/v8bench/raytrace.js',
     'renderScene']
);
*/
/*
//FIXME: requires Math.random (FP support)
tests.programs.v8bench['regexp'] = genTest(
    ['programs/v8bench/base.js',
     'programs/v8bench/regexp.js',
     'programs/v8bench/drv-regexp.js']
);
*/
tests.programs.v8bench['richards'] = genTest(
    ['programs/v8bench/base.js',
     'programs/v8bench/richards.js',
     'programs/v8bench/drv-richards.js']
);
/*
//FIXME: requires Math.random (FP support)
tests.programs.v8bench['splay'] = genTest(
    ['programs/v8bench/base.js',
     'programs/v8bench/splay.js',
     'programs/v8bench/drv-splay.js',
     'runSplayBenchmark']
);
*/

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
Tachyon bridge test code.
*/
tests.programs.tachyon_bridge = genTest(
    [
        'programs/tachyon_bridge/tachyon_bridge.js',
        'utility/debug.js',
        'ir/types.js',
        'platform/ffi.js',
        'platform/mcb.js',
    ],
    'test',
    [],
    0,
    'hostParams'
);

/**
Garbage collector tests
*/
tests.programs.gc = tests.testSuite();
tests.programs.gc.walk_stack = genTest(
    'programs/gc/walk_stack.js',
    'test',
    [],
    0,
    'hostParams'
);
tests.programs.gc.collect = genTest(
    'programs/gc/collect.js',
    'test',
    [],
    0,
    'hostParams'
);
tests.programs.gc.deepstack = genTest(
    'programs/gc/deepstack.js',
    'test',
    [],
    0,
    'hostParams'
);
tests.programs.gc.apply = genTest(
    'programs/gc/apply.js',
    'test',
    [],
    0,
    'hostParams'
);
tests.programs.gc.arguments = genTest(
    'programs/gc/arguments.js',
    'test',
    [],
    0,
    'hostParams'
);
tests.programs.gc.arrays = genTest(
    'programs/gc/arrays.js',
    'test',
    [],
    0,
    'hostParams'
);
tests.programs.gc.graph = genTest(
    'programs/gc/graph.js',
    'test',
    [],
    0,
    'hostParams'
);
tests.programs.gc.stackvm = genTest(
    'programs/gc/stackvm.js',
    'test',
    [],
    0,
    'hostParams'
);

/*
Type analysis test programs
*/
tests.programs.type_analysis = tests.testSuite();
tests.programs.type_analysis.global_add = genTest(
    'programs/type_analysis/global_add.js'
);
tests.programs.type_analysis.call_simple = genTest(
    'programs/type_analysis/call_simple.js'
);
tests.programs.type_analysis.func_2ret = genTest(
    'programs/type_analysis/func_2ret.js'
);
tests.programs.type_analysis.func_2calls = genTest(
    'programs/type_analysis/func_2calls.js'
);
tests.programs.type_analysis.func_calls = genTest(
    'programs/type_analysis/func_calls.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.arith_simple = genTest(
    'programs/type_analysis/arith_simple.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.cmp_simple = genTest(
    'programs/type_analysis/cmp_simple.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.fib = genTest(
    'programs/type_analysis/fib.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.loop_sum = genTest(
    'programs/type_analysis/loop_sum.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.obj_simple = genTest(
    'programs/type_analysis/obj_simple.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.get_undef = genTest(
    'programs/type_analysis/get_undef.js'
);
tests.programs.type_analysis.linked_list = genTest(
    'programs/type_analysis/linked_list.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.cond_return = genTest(
    'programs/type_analysis/cond_return.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.cond_prop = genTest(
    'programs/type_analysis/cond_prop.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.cond_global = genTest(
    'programs/type_analysis/cond_global.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.cond_objs = genTest(
    'programs/type_analysis/cond_objs.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.cond_call = genTest(
    'programs/type_analysis/cond_call.js'
);
tests.programs.type_analysis.cond_pass2 = genTest(
    'programs/type_analysis/cond_pass2.js'
);
tests.programs.type_analysis.cond_ret_obj = genTest(
    'programs/type_analysis/cond_ret_obj.js'
);
tests.programs.type_analysis.loop_cond_obj = genTest(
    'programs/type_analysis/loop_cond_obj.js'
);
tests.programs.type_analysis.arr_simple = genTest(
    'programs/type_analysis/arr_simple.js'
);
tests.programs.type_analysis.obj_methods = genTest(
    'programs/type_analysis/obj_methods.js'
);
tests.programs.type_analysis.obj_init = genTest(
    'programs/type_analysis/obj_init.js'
);
tests.programs.type_analysis.factory_2calls = genTest(
    'programs/type_analysis/factory_2calls.js'
);
tests.programs.type_analysis.factory_2paths = genTest(
    'programs/type_analysis/factory_2paths.js'
);
tests.programs.type_analysis.factory_global = genTest(
    'programs/type_analysis/factory_global.js'
);
tests.programs.type_analysis.factory_cond = genTest(
    'programs/type_analysis/factory_cond.js'
);
tests.programs.type_analysis.factory_inc = genTest(
    'programs/type_analysis/factory_inc.js'
);
tests.programs.type_analysis.ctor_simple = genTest(
    'programs/type_analysis/ctor_simple.js',
    'test',
    [],
    0
);
tests.programs.type_analysis.ctor_array = genTest(
    'programs/type_analysis/ctor_array.js'
);
tests.programs.type_analysis.proto_chain = genTest(
    'programs/type_analysis/proto_chain.js'
);
tests.programs.type_analysis.args_sum = genTest(
    'programs/type_analysis/args_sum.js'
);
tests.programs.type_analysis.clos_simple = genTest(
    'programs/type_analysis/clos_simple.js'
);
tests.programs.type_analysis.multi_file = genTest(
    [
        'programs/type_analysis/multi_file1.js',
        'programs/type_analysis/multi_file2.js'
    ]
);
tests.programs.type_analysis.stdlib_math = genTest(
    'programs/type_analysis/stdlib_math.js'
);
tests.programs.type_analysis.stdlib_array = genTest(
    'programs/type_analysis/stdlib_array.js'
);
tests.programs.type_analysis.regress_btree = genTest(
    'programs/type_analysis/regress_btree.js'
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

