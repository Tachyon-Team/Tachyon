function test()
{
    print("Opening string port");
    
    var port = new String_input_port(
        "function fib(n) { if (n < 2) { return n; }" +
        " else { return fib(n-1) + fib(n-2); } }"
    );
    var s = new Scanner(port); 
    var p = new Parser(s, false); 

    print("Parsing");
    prog = p.parse(); 

    assert(prog !== null, "Invalid parse tree");

    print("Creating program");
    prog = new Program(
        prog.loc,
        new BlockStatement(
            prog.loc,
            prog.block.statements
        )
    );

    print("Normalizing program");
    var normalized_prog = ast_normalize(prog, false);

    print("Pretty printing");
    var pp_str = js_to_string(normalized_prog);

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

    print("Comparing");
    print(pp_str);
    print(exp_str);
    //assert(pp_str === exp_str, "Invalid string from parsing " + pp_str + " expected " + exp_str);

    if (pp_str === exp_str)
    {
        return 0;
    } else
    {
        return 1;
    }
}
