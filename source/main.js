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
    var ast = parse_src_file('programs/fib.js');
    //var ast = parse_src_str('function foo(a) { return a + "foo"; }');

    pp(ast);

    var ir = unitToIR(ast, true);
    
    print(ir);

    lowerIRFunc(ir);

    print(ir);

    ir.validate();    
    
    printInstrNames(ir);

    /*
    var codeblock = backend.compile(ir, print);
    print(backend.listing(codeblock));
    backend.execute(codeblock);
    */

    
    var func = staticEnv.getBinding('initStrTable');
    print(func);
    
};

function printInstrNames(ir)
{
    var workList = ir.getChildrenList();
    
    var visited = []

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

