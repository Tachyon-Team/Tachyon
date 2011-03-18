function measurePerformance(msg, fct)
{
    //print(msg);
    fct();
}

function btfib(is64bit)
{
    printTachyonState();

    print("Running after initialization of boostrap code");
    //print("x86: " + x86);
    //print("asm: " + asm);
    //print("IRType: " + IRType);
    //print("IRType.box: " + IRType.box);

    print('Initializing Tachyon');

    // Initialize Tachyon in minimal mode
    initialize(false, is64bit);

    printTachyonState();

    print("Fib: compiling source code to a code block");
    var fibIR = compileSrcString("function fib(n) { if (n < 2) { return n; } else { return fib(n-1) + fib(n-2); } }", config.hostParams);
    var fibCB = backend.compileIRToCB(fibIR, config.hostParams); 

    print("Fib listing:");
    print(backend.listing(fibCB));

    print("Fib: compiling source code to a machine code block");
    var bridge = makeBridge(
        fibIR,
        config.hostParams,
        [new CIntAsBox()],
        new CIntAsBox()
    );

    bridge(config.hostParams.ctxPtr);
}
