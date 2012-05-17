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
Test suite for type analysis.
*/
tests.spstf = tests.testSuite();

/**
Create an SPSTF analysis unit test
*/
SPSTF.makeTest = function (fileList, useStdlib)
{
    if (typeof fileList === 'string')
        fileList = [fileList];

    if (useStdlib === undefined)
        useStdlib = false;

    var test = function ()
    {
        const params = config.hostParams;

        var analysis = new SPSTF(params);

        analysis.testOnFiles(fileList, useStdlib);
    }

    test.srcFiles = fileList;

    return test;
}


tests.spstf.global_add = SPSTF.makeTest(
    'programs/type_analysis/global_add.js',
    false
);


/*
tests.spstf.string_simple = SPSTF.makeTest(
    'programs/type_analysis/string_simple.js', 
    false
);
*/

/*
tests.spstf.array_simple = SPSTF.makeTest(
    'programs/type_analysis/array_simple.js', 
    false
);

tests.spstf.call_simple = SPSTF.makeTest(
    'programs/type_analysis/call_simple.js', 
    false
);

tests.spstf.func_2ret = SPSTF.makeTest(
    'programs/type_analysis/func_2ret.js', 
    false
);

tests.spstf.func_2calls = SPSTF.makeTest(
    'programs/type_analysis/func_2calls.js', 
    false
);

tests.spstf.func_calls = SPSTF.makeTest(
    'programs/type_analysis/func_calls.js', 
    false
);

tests.spstf.arith_simple = SPSTF.makeTest(
    'programs/type_analysis/arith_simple.js', 
    false
);

tests.spstf.cmp_simple = SPSTF.makeTest(
    'programs/type_analysis/cmp_simple.js', 
    false
);

tests.spstf.fib = SPSTF.makeTest(
    'programs/type_analysis/fib.js', 
    false
);

tests.spstf.loop_sum = SPSTF.makeTest(
    'programs/type_analysis/loop_sum.js',
    false
);

tests.spstf.obj_simple = SPSTF.makeTest(
    'programs/type_analysis/obj_simple.js', 
    false
);

tests.spstf.get_undef = SPSTF.makeTest(
    'programs/type_analysis/get_undef.js', 
    false
);

tests.spstf.linked_list = SPSTF.makeTest(
    'programs/type_analysis/linked_list.js',
    false
);

tests.spstf.cond_return = SPSTF.makeTest(
    'programs/type_analysis/cond_return.js', 
    false
);

tests.spstf.cond_prop = SPSTF.makeTest(
    'programs/type_analysis/cond_prop.js', 
    false
);

tests.spstf.cond_global = SPSTF.makeTest(
    'programs/type_analysis/cond_global.js', 
    false
);

tests.spstf.cond_objs = SPSTF.makeTest(
    'programs/type_analysis/cond_objs.js', 
    false
);

tests.spstf.cond_call = SPSTF.makeTest(
    'programs/type_analysis/cond_call.js', 
    false
);

tests.spstf.cond_pass2 = SPSTF.makeTest(
    'programs/type_analysis/cond_pass2.js',
    false
);

tests.spstf.cond_ret_obj = SPSTF.makeTest(
    'programs/type_analysis/cond_ret_obj.js',
    false
);

tests.spstf.loop_cond_obj = SPSTF.makeTest(
    'programs/type_analysis/loop_cond_obj.js',
    false
);

tests.spstf.arr_simple = SPSTF.makeTest(
    'programs/type_analysis/arr_simple.js', 
    false
);

tests.spstf.obj_methods = SPSTF.makeTest(
    'programs/type_analysis/obj_methods.js',
    false
);

tests.spstf.obj_init = SPSTF.makeTest(
    'programs/type_analysis/obj_init.js',
    false
);

tests.spstf.obj_init_junk = SPSTF.makeTest(
    'programs/type_analysis/obj_init_junk.js',
    false
);

tests.spstf.factory_2calls = SPSTF.makeTest(
    'programs/type_analysis/factory_2calls.js',
    false
);

tests.spstf.factory_2paths = SPSTF.makeTest(
    'programs/type_analysis/factory_2paths.js',
    false
);

tests.spstf.factory_global = SPSTF.makeTest(
    'programs/type_analysis/factory_global.js',
    false
);

tests.spstf.factory_cond = SPSTF.makeTest(
    'programs/type_analysis/factory_cond.js',
    false
);

tests.spstf.factory_inc = SPSTF.makeTest(
    'programs/type_analysis/factory_inc.js',
    false
);

tests.spstf.ctor_simple = SPSTF.makeTest(
    'programs/type_analysis/ctor_simple.js',
    false
);

tests.spstf.ctor_array = SPSTF.makeTest(
    'programs/type_analysis/ctor_array.js',
    false
);

tests.spstf.proto_chain = SPSTF.makeTest(
    'programs/type_analysis/proto_chain.js',
    false
);

tests.spstf.proto_clos = SPSTF.makeTest(
    'programs/type_analysis/proto_clos.js',
    false
);

tests.spstf.proto_loop = SPSTF.makeTest(
    'programs/type_analysis/proto_loop.js',
    false
);

tests.spstf.proto_cond = SPSTF.makeTest(
    'programs/type_analysis/proto_cond.js',
    false
);

tests.spstf.args_sum = SPSTF.makeTest(
    'programs/type_analysis/args_sum.js',
    false
);

tests.spstf.args_max = SPSTF.makeTest(
    'programs/type_analysis/args_max.js',
    false
);

tests.spstf.clos_simple = SPSTF.makeTest(
    'programs/type_analysis/clos_simple.js',
    false
);

tests.spstf.multi_file = SPSTF.makeTest(
    [
        'programs/type_analysis/multi_file1.js',
        'programs/type_analysis/multi_file2.js'
    ],
    false
);

tests.spstf.stdlib_math = SPSTF.makeTest(
    'programs/type_analysis/stdlib_math.js',
    true
);

tests.spstf.stdlib_object = SPSTF.makeTest(
    'programs/type_analysis/stdlib_object.js',
    true
);

tests.spstf.stdlib_array = SPSTF.makeTest(
    'programs/type_analysis/stdlib_array.js',
    true
);

tests.spstf.stdlib_function = SPSTF.makeTest(
    'programs/type_analysis/stdlib_function.js',
    true
);

tests.spstf.stdlib_string = SPSTF.makeTest(
    'programs/type_analysis/stdlib_string.js',
    true
);

tests.spstf.regress_btree = SPSTF.makeTest(
    'programs/type_analysis/regress_btree.js',
    false
);

tests.spstf.regress_btree2 = SPSTF.makeTest(
    'programs/type_analysis/regress_btree2.js',
    false
);

tests.spstf.regress_btree3 = SPSTF.makeTest(
    'programs/type_analysis/regress_btree3.js',
    false
);

tests.spstf.regress_base64 = SPSTF.makeTest(
    'programs/type_analysis/regress_base64.js',
    true
);

tests.spstf.regress_crypto = SPSTF.makeTest(
    'programs/type_analysis/regress_crypto.js',
    false
);

tests.spstf['bitops-3bit-bits-in-byte'] = SPSTF.makeTest(
    'programs/sunspider/bitops-3bit-bits-in-byte.js',
    false
);

tests.spstf['bitops-bitwise-and'] = SPSTF.makeTest(
    'programs/sunspider/bitops-bitwise-and.js',
    false
);
*/

/*
tests.spstf['access-binary-trees'] = SPSTF.makeTest(
    'programs/sunspider/access-binary-trees.js',
    true
);
*/

/*
tests.spstf['access-fannkuch'] = SPSTF.makeTest(
    'programs/sunspider/access-fannkuch.js',
    true
);
*/

/*
tests.spstf['access-nsieve'] = SPSTF.makeTest(
    'programs/sunspider/access-nsieve.js',
    true
);
*/

/*
tests.spstf['bitops-bits-in-byte'] = SPSTF.makeTest(
    'programs/sunspider/bitops-bits-in-byte.js',
    true
);
*/

/*
tests.spstf['bitops-nsieve-bits'] = SPSTF.makeTest(
    'programs/sunspider/bitops-nsieve-bits.js',
    true
);
*/

/*
tests.spstf['string-base64'] = SPSTF.makeTest(
    'programs/sunspider/string-base64.js',
    true
);
*/

/*
tests.spstf['deltablue'] = SPSTF.makeTest(
    [
        'programs/v8bench/deltablue.js',
        'programs/v8bench/drv-deltablue.js'
    ],
    true
);
*/

/*
tests.spstf['richards'] = SPSTF.makeTest(
    [
        'programs/v8bench/richards.js',
        'programs/v8bench/drv-richards.js'
    ],
    true
);
*/

