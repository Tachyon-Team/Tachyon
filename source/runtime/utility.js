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

    if (boxIsInt(val))
    {
        printInt(val);
    }
    else if (boxIsString(val))
    {
        var cstr = makeCString(val);
        printStr(cstr);
        freeCString(cstr);
    }
    else if (val === UNDEFINED)
    {
        print('undefined');
    }
    else if (val === null)
    {
        print('null');
    }
    else if (val === true)
    {
        print('true');
    }
    else if (val === false)
    {
        print('false');
    }
    else
    {
        print('unsupported value type in print');
    }
}

