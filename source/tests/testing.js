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
function TestCase(testName, testFunc)
{
    /**
    Function to perform this unit test
    NOTE: tests should return true if they succeed, false otherwise
    */
    this.runTest = testFunc;

    /**
    Unit test name
    @field
    */
    this.testName = testName;
}

/**
@class Test suite class
*/
function TestSuite(suiteName)
{
    /**
    Run this test suite
    */
    this.runSuite = function ()
    {
        print(
            'Running test suite: ' + this.suiteName + ' (' +
            this.testCases.length + ' ' + 
            pluralize('test', this.testCases.length) + ')\n'
        );

        var failCount = 0;

        for (var i = 0; i < this.testCases.length; ++i)
        {
            var testCase = this.testCases[i];

            print('Running test: ' + testCase.testName);

            var output = testCase.runTest();

            if (output === true)
            {
                print('PASSED');
            }
            else
            {
                // If the test returned an output string, print it
                if (typeof output == 'string')
                    print ('*** FAILED: ' + output);
                else
                    print('*** FAILED ***');

                failCount++;
            }
        }

        if (failCount > 0)
            print('WARNING: ' + failCount + ' test ' + pluralize('case', failCount) + ' failed');
        else
            print('All test cases passed');

        return failCount;
    };

    /**
    Add a test to the suite
    */
    this.addTest = function (test)
    {
        if (this.testCases.length == 0)
            this.testCases = [test];
        else
            this.testCases.push(test);
    };

    /**
    Unit test name
    @field
    */
    this.suiteName = suiteName;

    /**
    List of test suites
    @field
    */
    this.testCases = [];
}

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
        print('Running ' + this.testSuites.length + ' test ' + pluralize('suite', this.testSuites.length) + '\n');

        var failSuiteCount = 0;

        for (var i = 0; i < this.testSuites.length; ++i)
        {
            var testSuite = this.testSuites[i];

            var failTestCount = testSuite.runSuite();

            if (failTestCount > 0)
                ++failSuiteCount;

            print('');
        }

        if (failSuiteCount > 0)
            print('WARNING: ' + failSuiteCount + ' test ' + pluralize('suite', failSuiteCount) + ' failed');
        else
            print('All test suites passed');

        print('');
    };

    /**
    Add a test suite
    */
    this.addSuite = function (suite)
    {
        this.testSuites.push(suite);
    };

    /**
    List of test suites
    @field
    */
    this.testSuites = [];
}

// Global test manager instance
testManager = new TestManager();

