/**
@fileOverview
Front-end interface.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

var frontend = frontend || {};

/**
Compile a file to an optimized IRFunction

@filename   String containing path to the source file
@tachyonSrc Flag to indicate whether we are compiling tachyon code
*/
frontend.compileFileToIR = function (filename, tachyonSrc)
{
    tachyonSrc = tachyonSrc || false;

    var ast = parse_src_file(filename);
    var ir = unitToIR(ast, tachyonSrc);
    lowerIRFunc(ir);

    return ir;
};
