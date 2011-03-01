function test()
{
    initialize();

    var fibstr = "function fib(n) { if (n < 2) { return n; }" + 
     " else { return fib(n-1) + fib(n-2); } }";

    print("compiling fibonacci");
    var fib = compileSrcString(fibstr, config.hostParams);

    print("clean up");
    uninitialize();
}
test();
