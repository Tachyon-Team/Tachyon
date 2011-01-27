/**
@fileOverview
Low-level utility functions for the run-time.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Print an error string and stop the execution
*/
function error(errorStr)
{
    "tachyon:static";
    "tachyon:noglobal";

    print('*** RUN-TIME ERROR ***');
    print(errorStr);
    exit(0);
}

/**
Perform an assertion test
*/
function assert(testVal, errorStr)
{
    "tachyon:static";
    "tachyon:noglobal";

    if (testVal !== true)
    {
        error(errorStr);
    }
}

/**
Print values to the console
*/
function print(val)
{
    "tachyon:static";
    "tachyon:noglobal";

    // FIXME: until we have int to string conversion in boxToString
    if (boxIsInt(val))
    {
        printInt(val);
        return;
    }

    // Convert the value to a string
    var strVal = boxToString(val);

    // Print the string
    var cstr = makeCString(val);
    printStr(cstr);
    freeCString(cstr);
}

