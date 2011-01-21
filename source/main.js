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
    bootstrap();

    /*
    var memBlock = allocMachineCodeBlock(16384);    
    var blockAddr = getBlockAddr(memBlock, 0);

    var ir = compileSrcFile('test_objs.js', config.hostParams);

    var bridge = makeBridge(
        ir.getChild('proxy'),
        ['void*'],
        'int'
    );

    var result = bridge(blockAddr);

    print('result: ' + result);
    print('');

    for (var i = 0; i < 4096; ++i)
    {
        if (memBlock[i] != 0)
            print(i + ': ' + memBlock[i]);
    }
    */
    
    /*    
    var ir = compileSrcFile('programs/fib/fib.js', config.hostParams);

    var bridge = makeBridge(
        ir.getChild('fib'),
        ['int'],
        'int'
    );

    var result = bridge(10);

    print(result);
    */

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
    var func = compileSrcFile('programs/fib/fib.js', config.hostParams);
    print(func);
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

