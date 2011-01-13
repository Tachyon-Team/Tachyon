/**
@fileOverview
Test of the return statement.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

tests.basic_ret = tests.testSuite();

tests.basic_ret.main = function ()
{
    var basic_ret = compileFileToJSFunc('programs/basic_ret/basic_ret.js', config.clientParams);
    var x = basic_ret();
    basic_ret.free();
    assert(x === (20 << 2), "Invalid return value: " + x);
};
