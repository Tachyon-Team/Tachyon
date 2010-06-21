/**
@fileOverview
Unit testing framework implementation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Base class for test cases
*/
function TestCase(testName)
{
}
TestCase.prototype = {};






/**
@class Test suite class
*/
function TestSuite(suiteName)
{
}
TestSuite.prototype = {};







/**
@class Class to orchestrate the unit testing process
*/
function TestManager()
{
    /**
    Run all unit tests
    */
    this.runTests = function ()
    {
    };

    /**
    List of test suites
    @field
    */
    this.testSuites = [];
}

// Global test manager instance
testManager = new TestManager();

