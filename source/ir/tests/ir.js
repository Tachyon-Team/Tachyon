/**
@fileOverview
Unit tests for AST->IR translation code

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Test suite for AST->IR translation code
*/
tests.ir = tests.testSuite();

/**
Helper functions for this test suite
*/
tests.ir.helpers = {};

/**
Parse a source code string, copy the resulting IR and validate it
*/
tests.ir.helpers.testSource = function (sourceStr)
{
    // Parse the source string
    var ast = parse_src_str(sourceStr);

    // Translate the AST to IR
    var ir = unitToIR(ast);

    /*
    pp(ast); // pretty-print AST
    print('\n');
    print(ir);
    */

    // Copy the resulting function
    ir.copy();

    // Validate the IR
    ir.validate();

    // Perform lowering on the IR
    lowerIRFunc(ir);

    // Validate the IR
    ir.validate();

    // Return the generated IR for further testing
    return ir;
}

/**
Apply a function to each instruction in an IR function and its sub-functions
*/
tests.ir.helpers.forEachInstr = function (ir, instrFunc)
{
    // Get the list of all functions in the tree
    var flist = ir.getChildrenList();

    // Apply the function to each instruction of each function
    for (var i = 0; i < flist.length; ++i)
    {
        for (var it = flist[i].virginCFG.getInstrItr(); it.valid(); it.next())
        {
            instrFunc(it.get());
        }
    }
}

/**
Object literal expression
*/
tests.ir.objectExpr = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var o = {                           \
                'a':1,                          \
                b: 'foo'                        \
            };                                  \
            print(o);                           \
        "
    );
}

/**
Array literal expression
*/
tests.ir.arrayExpr = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var o = [                           \
                'a', 1, 'foo', (2+2)            \
            ];                                  \
            print(o);                           \
        "
    );
}

/**
Arithmetic expressions
*/
tests.ir.arithExprs = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            var o = a + b * (c - d) % (e / f);  \
            print(o);                           \
        "
    );
}

/**
While loop statement
*/
tests.ir.whileStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            while (v)                           \
            {                                   \
                print(v);                       \
                                                \
                if (t)                          \
                    break;                      \
                else                            \
                    continue;                   \
            }                                   \
        "
    );
}

/**
Do-while loop statement
*/
tests.ir.doWhileStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            do                                  \
            {                                   \
                print(v);                       \
                                                \
                if (t)                          \
                    break;                      \
                else                            \
                    continue;                   \
            } while (v);                        \
        "
    );
}

/**
For loop statement
*/
tests.ir.forStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            for (var i = 0; i < 10; ++i)        \
            {                                   \
                print(i);                       \
            }                                   \
                                                \
            print(i);                           \
        "
    );
}

/**
Infinite loop
*/
tests.ir.infiniteLoop = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                var i = 0;                      \
                while (true)                    \
                {                               \
                    ++i;                        \
                }                               \
                print(i);                       \
            }                                   \
        "
    );
}

/**
Switch statement
*/
tests.ir.switchStmt = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function switcher(v)                \
            {                                   \
                var o = 0;                      \
                                                \
                switch (v)                      \
                {                               \
                    case 1:     o = 1; break;   \
                    case 'a':   o = 2;          \
                    default:    o = -1;         \
                }                               \
                                                \
                return 0;                       \
            }                                   \
        "
    );
}

/**
With statement
*/
tests.ir.withStmt = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo(obj, x, y)             \
            {                                   \
                with (obj)                      \
                {                               \
                    x = 3;                      \
                    print(y);                   \
                }                               \
            }                                   \
        "
    );

    // Ensure that there are get and put property instructions in the IR
    var hasGet = false;
    var hasPut = false;
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (instr instanceof CallFuncInstr && 
                instr.uses[0].funcName == 'get_prop_val')
                hasGet = true;
            if (instr instanceof CallFuncInstr && 
                instr.uses[0].funcName == 'put_prop_val')
                hasPut = true;
        }
    );
    assert (
        hasGet && hasPut,
        'With statement code missing get or put property instruction'
    );
}

/**
Fibonacci function
*/
tests.ir.fibFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function fib(n)                     \
            {                                   \
                if (n < 2)                      \
                    return n;                   \
                                                \
                return fib(n-1) + fib(n-2);     \
            }                                   \
        "
    );
}

/**
Nested if and return statements in a function
*/
tests.ir.ifRetFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                           \
            function foo()                          \
            {                                       \
                if (true)                           \
                {                                   \
                    var v = foo;                    \
                                                    \
                    if (true)                       \
                        return 0;                   \
                    else                            \
                        return 0;                   \
                }                                   \
            }                                       \
        "
    );
}

/**
Constructor function
*/
tests.ir.ctorFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function ctor(a0)                   \
            {                                   \
                this.a0 = a0;                   \
            }                                   \
            var obj = new ctor(1);              \
            print(obj);                         \
        "
    );
}

/**
Closure function
*/
tests.ir.closFunc = function ()
{
    tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                var v = 1;                      \
                                                \
                function bar()                  \
                {                               \
                    v++;                        \
                    print(v);                   \
                }                               \
                                                \
                bar();                          \
                print(v);                       \
            }                                   \
        "
    );
}

/**
Arguments object
*/
tests.ir.argsObj = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo(a0, a1)                \
            {                                   \
                arguments[0] = 3;               \
                                                \
                function bar()                  \
                {                               \
                    a1 = 2;                     \
                    print(a0);                  \
                }                               \
                                                \
                bar();                          \
                print(a0);                      \
            }                                   \
        "
    );

    // Ensure that there are no direct references to argument values
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            for (var j = 0; j < instr.uses.length; ++j)
            {
                assert (
                    instr.uses[j].getValName() != 'a0' && 
                    instr.uses[j].getValName() != 'a1',
                    'argument values should not be used directly'
                );
            }
        }
    );
}

/**
Inlining of function calls
*/
tests.ir.inlining = function ()
{
    var ir = tests.ir.helpers.testSource(
        "                                       \
            function foo()                      \
            {                                   \
                bar(1);                         \
            }                                   \
                                                \
            function bar(a0)                    \
            {                                   \
                print(a0);                      \
            }                                   \
        "
    );

    // Find the bar call instruction
    var callInstr;
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (
                instr instanceof CallFuncInstr &&
                instr.parentBlock.parentCFG.ownerFunc.funcName == 'foo'
            )
            {
                callInstr = instr;
            }
        }
    );

    // Find the bar function
    var barFunc;
    ir.getChildrenList().forEach(
        function (func)
        {
            if (func.funcName == 'bar')
                barFunc = func;
        }
    );

    // Inline the bar call
    inlineCall(callInstr, barFunc);

    //print(ir);

    // Validate the resulting IR
    ir.validate();
}

