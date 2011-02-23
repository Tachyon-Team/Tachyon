/**
@fileOverview
Unit testing framework implementation.

Provides facilities to add test cases and organize them into test suites. 
Test cases are functions and belong to a test suite.  Test suite may contain
test cases and/or nested test suites. Test cases fail by throwing an exception.
All first-level test suites belong to a global 'tests' object.

Unit tests are implemented using the javascript reflection facilities. 
First-level test suites are properties of the global 'tests' object.  Nested
test suites and test cases are properties of a test suite object.  Test cases
are functions.  For convenience, a setup and a teardown function might be added
to a child test suite.  They will respectively be executed before and after each
test case.  This is useful for initializing complex objects that might be used
in different test cases while still garanteeing independance of tests. Since
reflection is used, no special step is required for test registration,
it is done automatically.

Example of a test file:

// Add a new test suite
tests.fooSuite = tests.testSuite();

// Add a new setup function
tests.fooSuite.setup = function () 
{
    tests.fooSuite.myHugeComplexObject = myHugeComplexObjectInit();
}

// Add a new test case
tests.fooSuite.trivialInit = function ()
{
    assert(tests.fooSuite.myHugeComplexObject !== null);
}


@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace for all tests */
var tests = tests || {};

/** 
    Root test function to run all known test suites.
    @param {Boolean} catchError Fail on test case exceptions or continue 
    @param {String} testName Specific test to run
    @param {Boolean} verbose Display test names when they succeed or not
*/
tests.run = function (catchError, testName, verbose)
{
    if (verbose === undefined)
    {
        verbose = false;
    }

    // TODO: Handle test nested test suite name
    var suiteName = (testName !== undefined) ? testName.split(".")[0] : undefined;
    var caseName = (testName !== undefined) ? testName.split(".")[1] : undefined;

    var p;
    var failedNb = 0; 
    for (p in this)
    {
        if (this.hasOwnProperty(p) && 
            typeof this[p] !== "function" &&
            (suiteName === undefined || suiteName === p))
        {
            failedNb += this[p].run(catchError, p, caseName, verbose);
        } 
    }

    if (failedNb > 0)
    {
        print("WARNING: " + failedNb + pluralize(" test", failedNb) + " failed");
    } else
    {
        print("All tests passed.");
    }
};

/** @class Test suite */
tests.testSuite = function () 
{
    var that = Object.create(tests.testSuite.prototype);
    return that;
};

/** run all tests cases and nested test suites for this test suite */
tests.testSuite.prototype.run = function (catchError, suiteName, caseName, verbose)
{
    function runTestCase(suite, caseName)
    {
        if (suite["setup"] !== undefined)
        {
            suite.setup();
        }
        suite[caseName]();
        if (suite["teardown"] !== undefined)
        {
            suite.teardown();
        }
    }

    var p;
    var result;
    var failedNb = 0;
    var exception;



    for (p in this)
    {
        if (this.hasOwnProperty(p) && 
            typeof this[p] === "function" &&
            p !== "run" &&
            p !== "setup" &&
            p !== "teardown" &&
            (caseName === undefined || caseName === p))
        {
            result = true;
            if (catchError)
            {
                try
                {
                    runTestCase(this, p);
                } 
                catch (e)
                {
                    exception = e;
                    result = false; 
                    failedNb++;
                }
            } 
            else
            {
                // Useful for debugging failing tests
                runTestCase(this, p);
            }
            if (!result)
            {
                print(suiteName +"." + p + " FAILED with exception:");
                
                if (exception.stack)
                    print(indentText(exception.stack.toString(), "\t"));
                else
                    print(indentText(exception.toString(), "\t"));
            } 
            else if (verbose)
            {
                print(suiteName +"." + p + " : ok");
            }
        } else if (this.hasOwnProperty(p) &&
                   tests.testSuite.prototype.isPrototypeOf(this[p]))
        {
            failedNb = failedNb + this[p].run(catchError, suiteName + "." + p,
                                              caseName, verbose);
        } 
    }

    return failedNb;
};

