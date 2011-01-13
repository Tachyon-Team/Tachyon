/**
@fileOverview
Basic conditional branching test.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.basic_if = tests.testSuite();

tests.basic_if.main = function ()
{
    var basic_if = compileFileToJSFunc('programs/basic_if/basic_if.js', config.clientParams);
    var x = basic_if();
    basic_if.free();
    assert(x === (2 << 2), "Invalid return value: " + x);

};
