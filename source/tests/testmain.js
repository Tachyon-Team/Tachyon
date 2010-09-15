/**
@fileOverview
Entry point for the testing mode.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

// Initialize Tachyon
initialize();

// Run all unit tests
tests.run(true, undefined, true);

// Uninitialize Tachyon
uninitialize();

