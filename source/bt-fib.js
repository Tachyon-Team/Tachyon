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

    print("Fib: compiling source code");
    //config.hostParams.printRegAlloc = true;
    //config.hostParams.printASM = true;
    //var ir = compileSrcString("function fib(n) { if (n < 2) { return n; } else { return fib(n-1) + fib(n-2); } }; printInt(unboxInt(fib(10)));", config.hostParams);
    //var ir = compileSrcString("puts('hello world'); printInt(unboxInt((function (n) { return n; })(10)));", config.hostParams);
    //var ir = compileSrcString("(function () { function fib(n) { if (n < 2) return n; else return fib(n-1) + fib(n-2); } printInt(unboxInt(fib(10))); })();", config.hostParams);

    var ir = compileSrcFile('bt-test.js', config.hostParams);

    //config.hostParams.printRegAlloc = false;
    //config.hostParams.printASM = false;
    //var ir = compileSrcString("printInt(unboxInt(1));", config.hostParams);
    //var fibCB = backend.compileIRToCB(fibIR, config.hostParams); 

    //print("Fib listing:");
    //print(backend.listing(fibCB));
    
    print('Fib: Creating bridge');
    var bridge = makeBridge(
        ir,
        config.hostParams,
        [],
        new CIntAsBox()
    );

    print("Fib: Executing");
    print(config.hostParams.ctxPtr);
    bridge(config.hostParams.ctxPtr);
}
