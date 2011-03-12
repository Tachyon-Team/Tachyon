function genTest(srcFiles, funcName, inputArgs, expectResult, compParams)
{
    if (typeof srcFiles === 'string')
        srcFiles = [srcFiles];

    return function()
    {
        var result = compileAndRunSrcs(
            srcFiles, 
            funcName,
            inputArgs,
            compParams
        );

        assert (
            result === expectResult,
            'Invalid return value "' + result + 
            '", expected "' + expectResult + '"'
        );
    };
}

function compileAndRunSrcs(srcFiles, funcName, inputArgs, compParams)
{
    var argTypes = [];
    for (var i = 0; i < inputArgs.length; ++i)
    {
        assert (
            isInt(inputArgs[i]),
            'only integer arguments supported for now'
        );
        argTypes.push(new CIntAsBox());
    }

    if (compParams === undefined)
        compParams = 'clientParams';

    var params = config[compParams];

    assert (
        params instanceof CompParams,
        'invalid compilation parameters'
    );

    // For each source file
    for (var i = 0; i < srcFiles.length; ++i)
    {
        var srcFile = srcFiles[i];

        // Compile the unit
        print("Compiling: '" + srcFiles[i] + "'");
        var ir = compileSrcFile(srcFile, params);

        // Create a bridge to execute this unit
        var unitBridge = makeBridge(
            ir,
            config.hostParams,
            [],
            new CIntAsBox()
        );

        // Execute the compilation unit to initialize it
        unitBridge(params.ctxPtr);

        // Try to find the function of the specified name in the unit
        var func = ir.getChild(funcName);

        if (func !== null)
            var funcIR = func;
    }

    assert (
        funcIR !== null,
        'test function not found'
    );

    var funcBridge = makeBridge(
        funcIR,
        config.hostParams,
        argTypes,
        new CIntAsBox()
    );

    // Call the function with the given arguments
    print("Executing test");
    var result = funcBridge.apply(undefined, [params.ctxPtr].concat(inputArgs));

    return result;
}

function btparser(is64bit)
{
    initialize(false, is64bit);

    print("Creating parser test");
    var tachyon_parser_test = genTest(
        [
            'utility/debug.js',
            'utility/hashmap.js',
            'utility/num.js',
            'parser/misc.js',
            'parser/scanner.js',
            'parser/parser.js',
            'parser/pp.js',
            'parser/ast-passes.js',
            'programs/tachyon_parser/tachyon_parser.js'
        ],
        'test',
        [],
        0
    );

    tachyon_parser_test();

    print("Test succeeded");

    uninitialize();
}
