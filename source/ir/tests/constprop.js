/**
@fileOverview
Unit tests for AST->IR translation code

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Test suite for constant propagation
*/
tests.constprop = tests.testSuite();

/**
Helper functions for this test suite
*/
tests.constprop.helpers = {};

/**
Test that only a constant value is returned
*/
tests.constprop.helpers.returnsCst = function (ir, constVal)
{
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (instr instanceof RetInstr)
            {
                if (instr.uses[0] !== constVal)
                    error('wrong constant returned');
            }
        },
        false
    );
};

/**
Generate a constant propagation test
*/
tests.constprop.helpers.genTest = function (testCode, constVal)
{
    return function ()
    {
        ir = tests.ir.helpers.testSource(
            "                                       \
                function foo(a)                     \
                {                                   \
                " + testCode + "                    \
                }                                   \
            "
        );

        tests.constprop.helpers.returnsCst(ir, constVal);
    }
}

/**
Addition test
*/
tests.constprop.add = tests.constprop.helpers.genTest(
    'return 6 + 4;',
    ConstValue.getConst(10)
);

/**
Addition test
*/
tests.constprop.sub = tests.constprop.helpers.genTest(
    'return 6 - 3;',
    ConstValue.getConst(3)
);

/**
Multiplication test
*/
tests.constprop.mul = tests.constprop.helpers.genTest(
    'return 6 * 3;',
    ConstValue.getConst(18)
);

/**
Division test
*/
tests.constprop.div = tests.constprop.helpers.genTest(
    'return 6 / 3;',
    ConstValue.getConst(2)
);

/**
Reciprocal test
*/
tests.constprop.divmul = tests.constprop.helpers.genTest(
    'return (6 / 3) * 3;',
    ConstValue.getConst(6)
);

/**
Complex arithmetic expression test
*/
tests.constprop.arithexpr = tests.constprop.helpers.genTest(
    'return ((6 / 3) + 2) * 3;',
    ConstValue.getConst(12)
);

/**
Left shift test
*/
tests.constprop.lsft = tests.constprop.helpers.genTest(
    'return 3 << 1;',
    ConstValue.getConst(6)
);

/**
Right shift test
*/
tests.constprop.rsft = tests.constprop.helpers.genTest(
    'return 8 >> 1;',
    ConstValue.getConst(4)
);

/**
Unsigned right shift test
*/
tests.constprop.ursft = tests.constprop.helpers.genTest(
    'return 8 >> 1;',
    ConstValue.getConst(4)
);

