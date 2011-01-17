/**
@fileOverview
Code specific to the JavaScript VM's entry point, handling of
command-line arguments, etc.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

function testIR()
{

    var memBlock = allocMachineCodeBlock(4096);
    
    var blockAddr = getBlockAddr(memBlock, 0);
    var b0 = blockAddr[0];
    var b1 = blockAddr[1];
    var b2 = blockAddr[2];
    var b3 = blockAddr[3];

    shellCommand("cp test_objs.js test_repl.js");
    shellCommand("sed -i '' -e 's/b0/" + b0 + "/g' test_repl.js");
    shellCommand("sed -i '' -e 's/b1/" + b1 + "/g' test_repl.js");
    shellCommand("sed -i '' -e 's/b2/" + b2 + "/g' test_repl.js");
    shellCommand("sed -i '' -e 's/b3/" + b3 + "/g' test_repl.js");

    var func = compileFileToJSFunc('test_repl.js', config.hostParams);
    var result = func();
    func.free();

    //print('result: ' + result);
    print('result: ' + (result >> 2));
    print('');

    //print('context size: ' + config.hostParams.memLayouts.ctx.getSize());

    for (var i = 0; i < 4096; ++i)
    {
        if (memBlock[i] != 0)
            print(i + ': ' + memBlock[i]);
    }


    /*
    var ast = parse_src_file('test_ffi.js');
    var ir = unitToIR(ast, config.hostParams);
    lowerIRFunc(ir, config.hostParams);
    ir.validate();    
    print(ir);
    
    //config.hostParams.print = print;
    var func = compileFileToJSFunc('test_ffi.js', config.hostParams);
    var result = func();
    func.free();
    //print(result >> 2);
    */

    /*
    var ast = parse_src_str(
        //'function foo() { return 6-3; }'
        //'function foo() { return 3+3; }'
        //'function foo() { return 6/3; }'
        //'function foo() { return 6*3; }'
        //'function foo() { return (6/3)*3; }'
        //'function foo(a) { return iir.add(a, 0); }'
        'function foo(a) { return iir.add(0, iir.sub(a,0)); }'
    );
    var ir = unitToIR(ast, config.hostParams);
    lowerIRFunc(ir, config.hostParams);
    ir.validate();    
    print(ir);
    */



    /*
    var ast = parse_src_file('loop_loop.js');
    var ir = unitToIR(ast, config.hostParams);
    lowerIRFunc(ir, config.hostParams);
    ir.validate();    
    print(ir);
    */

    /*
    config.hostParams.print = print;    
    
    var func = compileFileToJSFunc('loop_loop.js', config.hostParams);
    var result = func();
    func.free();
    print(result >> 2);
    */


    /*
    var codeblock = backend.compileIRToCB(ir);    
    //print(backend.listing(codeblock));
    var result = backend.executeCB(codeblock);

    //print('result: ' + (result >> 2));    
    print('result: ' + result);
    */
    
    /*
    var func = config.hostParams.staticEnv.getBinding('lt');
    print(func);
    */
};

function printInstrNames(ir)
{
    var workList = ir.getChildrenList();
    
    var visited = [];

    var mnemList = [];

    while (workList.length > 0)
    {
        var func = workList.pop();

        if (arraySetHas(visited, func))
            continue;

        /*
        if (func.funcName == 'newObject' ||
            func.funcName == 'getPropVal' || 
            func.funcName == 'putPropVal')
            continue;
        */

        for (var itr = func.virginCFG.getInstrItr(); itr.valid(); itr.next())
        {
            var instr = itr.get();

            arraySetAdd(mnemList, instr.mnemonic);

            for (var useItr = instr.getUseItr(); useItr.valid(); useItr.next())
            {
                var use = useItr.get();

                if (use instanceof IRFunction)
                    workList = workList.concat(use.getChildrenList());
            }
        }

        arraySetAdd(visited, func);
    }

    mnemList.sort();

    print('Total instruction variants: ' + mnemList.length);
    for (var i = 0; i < mnemList.length; ++i)
    {
        print(mnemList[i]);
    }
}

try 
{
    // Initialize Tachyon
    initialize();

    testIR();

    // Uninitialize Tachyon
    uninitialize();
}

catch (e)
{
    if (e.stack)
        print(e.stack);
    else
        print(e);
}

