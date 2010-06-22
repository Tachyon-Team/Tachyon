/**
@fileOverview
Entry point for the testing mode. To be run after tests are registered.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// Flag to enable or disable unit tests
// NOTE: will be useful to enable unit testing through the command-line
var testsEnabled = true;

// If unit testing is enabled
if (testsEnabled)
{
    // Register all test suites
    testManager.addSuite(makeUtilitySuite());

    // Run all the unit tests
    testManager.runTests();
}

