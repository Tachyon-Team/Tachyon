/**
@fileOverview
Entry point for the testing mode.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

var args = parseCmdLine();

// Initialize Tachyon
assert(initialize(false, args.options["x86_64"]), "Initialization failed");

// config.hostParams.print = print;

// Run all unit tests
tests.run(true, undefined, true);
//tests.run(false, undefined, true);

// Uninitialize Tachyon
uninitialize();

