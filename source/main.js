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
    var ast = parse_src_file('parser/tests/test4.js');
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

        if (func.funcName == 'get_prop_val' || 
            func.funcName == 'put_prop_val')
            continue;

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

// Initialize Tachyon
initialize();

testIR();

// Uninitialize Tachyon
uninitialize();

