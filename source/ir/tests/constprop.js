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
Object literal expression
*/
tests.constprop.arith = function ()
{
    ir = tests.ir.helpers.testSource(
        "                                       \
            function foo(a)                     \
            {                                   \
                return (2+3) * 4;               \
            }                                   \
        "
        //, true
    );

    // TODO: test that return value is correct
    // Write a function to do this
};

