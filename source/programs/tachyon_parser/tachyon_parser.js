function test()
{
    
    var port = new String_input_port(
        "function fib(n) { if (n < 2) { return n; }" +
        " else { return fib(n-1) + fib(n-2); } }"
    );
    var s = new Scanner(port); 
    var p = new Parser(s, false); 
    prog = p.parse(); 

    assert(prog !== null);

    prog = new Program(
        prog.loc,
        new BlockStatement(
            prog.loc,
            prog.block.statements
        )
    );

    var normalized_prog = ast_normalize(prog, false);

    //var pp_str = js_to_string(normalized_prog);
    var pp_str = "";

    var exp_str =   "var fib;\n" +
                "fib = (function (n)\n" +
                "{\n" +
                "    if ((n < 2))\n" + 
                "    {\n" + 
                "        return n;\n" +
                "    }\n" + 
                "    else\n" + 
                "    {\n" + 
                "        return (fib((n - 1)) + fib((n - 2)));\n" + 
                "    }\n" + 
                "});\n";

    assert(pp_str === exp_str, "Invalid string from parsing " + pp_str + " expected " + exp_str);
}
