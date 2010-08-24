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
    var port = new String_input_port(sourceStr);
    var p = new Parser(new Scanner(port), true);
    var ast = p.parse();
    var normalized_ast = ast_normalize(ast);

    // Translate the AST to IR
    var ir = unitToIR(normalized_ast);

    /*
    pp(normalized_ast); // pretty-print AST
    print('\n');
    print(ir);
    */

    // Copy the resulting function
    ir.copy();

    // Validate the function
    ir.validate();

    // Return the generated IR for further testing
    return ir;
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
    var flist = ir.getChildrenList();
    for (var i = 0; i < flist.length; ++i)
    {
        for (var it = flist[i].virginCFG.getInstrItr(); it.valid(); it.next())
        {
            var instr = it.get();
            for (var j = 0; j < instr.uses.length; ++j)
            {
                assert (
                    instr.uses[j].getValName() != 'a0' && 
                    instr.uses[j].getValName() != 'a1',
                    'argument values should not be used directly'
                );
            }
        }
    }
}

