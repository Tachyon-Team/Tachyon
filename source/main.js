/**
@fileOverview
Code specific to the JavaScript VM's entry point, handling of
command-line arguments, etc.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

function testIR()
{
    /* TODO: test object code using this method
    var memBlock = allocMemoryBlock(128);
    
    function getAddrInt(block, idx)
    {
        var blockAddr = getBlockAddress(memBlock, idx);

        var addr = 0;
        for (var i = blockAddr.length - 1; i >= 0; --i)
            addr = addr * 256 + blockAddr[i];

        return addr;
    }

    var blockAddr = getAddrInt(memBlock, 0);

    var ast = parse_src_str(
        'function foo() { ' +
        '"tachyon:ret i8";' +
        'iir.set_ctx(iir.icast(IRType.rptr,' + blockAddr + '));' +
        'iir.store(IRType.i8, iir.get_ctx(), iir.icast(IRType.i32, 0), iir.icast(IRType.i8, 7));' +
        'return iir.load(IRType.i8, iir.get_ctx(), iir.icast(IRType.i32, 0));' +
        '}' + 
        'return foo();'
    );
    */

    //var ast = parse_src_file('test.js');
    var ast = parse_src_file('programs/loop_sum/loop_sum.js');

    //pp(ast);

    var ir = unitToIR(ast, config.hostParams);
    
    //print(ir);

    lowerIRFunc(ir, config.hostParams);

    print(ir);

    ir.validate();    
     
    /*
    var codeblock = backend.compileIRToCB(ir);    
    //print(backend.listing(codeblock));
    var result = backend.executeCB(codeblock);

    //print('result: ' + (result >> 2));    
    print('result: ' + result);
    */
    
    /*
    var func = config.hostParams.staticEnv.getBinding('extStrTable');
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

