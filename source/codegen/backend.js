/**
@fileOverview
Entry point for the backend of the Javascript VM. This code should be platform agnostic.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace */
var backend = backend || {};

/**
    Returns a code block representing the compiled code.
*/
backend.compile = function (ir, print, primitives)
{
    if (print === undefined)
    {
        print = function () {};
    }

    const mem = x86.Assembler.prototype.memory;
    const reg = x86.Assembler.prototype.register;
    const translator = irToAsm.translator();

    var cfg, order, liveIntervals, mems;
    var i, k, next, tab;
    var fixedIntervals;
    var fcts = ir.getChildrenList();

    if (primitives !== undefined)
    {
        fcts = primitives.concat(fcts);
    }


    translator.init(ir);

    // For each function, order blocks, allocate registers, translate to 
    // assembly and generate code 

    for (k=0; k<fcts.length; ++k)
    {
        print("Translation of function: '" + fcts[k].funcName + "'");

        // Add register allocation information on the function
        fcts[k].regAlloc = fcts[k].regAlloc || {};
        fcts[k].regAlloc.spillNb = 0;

        cfg = fcts[k].virginCFG;

        cfg.simplify();

        order = allocator.orderBlocks(cfg);
        allocator.numberInstrs(cfg, order);

    
        print("******* Before register allocation *******");
        var block;
        var tab = "\t";
        var instr;
        for (i=0; i < order.length; ++i)
        {
            block = order[i]; 
            print(block.regAlloc.from + ": label " + block.label);
            for (var instrIt = block.getInstrItr(); instrIt.valid(); instrIt.next())
            {
                instr = instrIt.get();
                print(instr.regAlloc.id + ":" + tab + " (" + instr.instrId + ") " + instr );
            }
        }

        liveIntervals = allocator.liveIntervals(cfg, order, irToAsm.config);
        fixedIntervals = allocator.fixedIntervals(cfg, irToAsm.config);

        /*
        // Print intervals before allocation
        for (i=0; i<liveIntervals.length; ++i)
        {
            print(i + ": " + liveIntervals[i] + ",");
        }
        print();
        */

        mems = irToAsm.spillAllocator();

        allocator.linearScan(irToAsm.config, 
                             liveIntervals, 
                             mems, 
                             fixedIntervals);

        /*
        // Print intervals after allocation
        for (i=0; i<liveIntervals.length; ++i)
        {
            print(i + ": " + liveIntervals[i]);
            tab = "\t";
            next = liveIntervals[i].next;
            while (next)
            {
                print( tab + next);
                tab = tab + "\t";
                next = next.next;
            }
        }*/

        // Add physical registers and memory location to operands
        // of every instruction
        allocator.assign(cfg); 
    
        // SSA form deconstruction and linear scan resolution 
        order = allocator.resolve(cfg, liveIntervals, order);


        print("******* After register allocation *******");

        for (i=0; i < order.length; ++i)
        {
            block = order[i]; 
            print(block.regAlloc.from + ": label " + block.label);
            for (var instrIt = block.getInstrItr(); instrIt.valid(); instrIt.next())
            {
                instr = instrIt.get();
                if (instr instanceof MoveInstr)
                {
                    print("  " + tab + instr);
                } else if (instr instanceof ArgValInstr)
                {
                    if (instr.regAlloc.dest === null)
                    {
                        continue;
                    }
                    print(instr.regAlloc.id + ": " + tab + instr.regAlloc.dest 
                          + " = " + instr.mnemonic + " " + instr.argIndex );
                } else if (instr instanceof MakeClosInstr)
                {
                     
                    print(instr.regAlloc.id + ": " + tab 
                          + instr.regAlloc.dest + " = " 
                          + instr.mnemonic + " " 
                          + instr.regAlloc.opnds[0].funcName 
                          + " " + instr.regAlloc.opnds.slice(1));
                } else if (instr instanceof CallInstr && instr.regAlloc.opnds[0] instanceof IRFunction)
                {
                    print(instr.regAlloc.id + ": " + tab + 
                          (instr.regAlloc.dest !== null ? 
                           instr.regAlloc.dest + " = " : "") 
                          +  instr.mnemonic + " <" + instr.regAlloc.opnds[0].funcName + ">, " +
                          instr.regAlloc.opnds.slice(1));
                } else 
                {
                    print(instr.regAlloc.id + ": " + tab + 
                          (instr.regAlloc.dest !== null ? 
                           instr.regAlloc.dest + " = " : "") 
                          +  instr.mnemonic + " " + instr.regAlloc.opnds );
                }
            }
        }
        print("*****************************************");

        fcts[k].regAlloc.spillNb = mems.slots.length;

        print("Allocation info:");
        print("Number of spills: " + fcts[k].regAlloc.spillNb);
        print();

        // Translate from IR to ASM
        //translator.genFunc(fcts[k], order);
    }

    //translator.asm.codeBlock.assemble();
    return translator.asm.codeBlock;
};

/**
    Returns a string representation of the listing for the given
    code block.
*/
backend.listing = function (codeBlock)
{
    return codeBlock.listingString();
};

/**
    Allocate an executable memory zone, write the given code block in
    that zone, execute it, free the memory zone and return the result.
*/
backend.execute = function (codeBlock)
{
    var block = codeBlock.assembleToMachineCodeBlock(); // assemble it
    var x = execMachineCodeBlock(block); // execute the code generated
    freeMachineCodeBlock(block);
    return x;
};

/**
    Returns the list of primitive functions used in a given IR.
*/
backend.usedPrimitives = function (ir)
{
    var workList = ir.getChildrenList();
    var visited = [];
    var primitives = [];


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


            for (var useItr = instr.getUseItr(); useItr.valid(); useItr.next())
            {
                var use = useItr.get();

                if (use instanceof IRFunction)
                {
                    workList = workList.concat(use.getChildrenList());
                    if (use.funcName in primitiveMap)
                    {
                        primitives.push(use);
                    }
                }
            }
        }

        arraySetAdd(visited, func);
    }

    return primitives;
};



