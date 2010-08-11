/**
@fileOverview
Unit testing framework implementation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace for all tests */
var tests = tests || {};

/** Root test function */
tests.run = function (catchError, testName)
{

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
            failedNb += this[p].run(catchError, p, caseName);
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
tests.testSuite.prototype.run = function (catchError, suiteName, caseName)
{
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
                    if (this["setup"] !== undefined)
                    {
                        this.setup();
                    }
                    this[p]();
                    if (this["teardown"] !== undefined)
                    {
                        this.teardown();
                    }
                } catch (e)
                {
                    exception = e;
                    result = false; 
                    failedNb++;
                }
            } else
            {
                // Useful for debugging failing tests
                if (this["setup"] !== undefined)
                {
                    this.setup();
                }
                this[p]();
                if (this["teardown"] !== undefined)
                {
                    this.teardown();
                }
            }
            if (!result)
            {
                print(suiteName +"." + p + " FAILED with exception:");
                print("\t" + exception);
            }
        } else if (this.hasOwnProperty(p) &&
                   tests.testSuite.prototype.isPrototypeOf(this[p]))
        {
            failedNb = failedNb + this[p].run(catchError, suiteName + "." + p,
                                              caseName);
        } 
    }

    return failedNb;
};

