/**
@fileOverview
Entry point for the testing mode.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

// Flag to enable or disable unit tests
// NOTE: will be useful to enable unit testing through the command-line
var testsEnabled = true;

if (testsEnabled)
{
    tests.run(true, undefined, true);
}
