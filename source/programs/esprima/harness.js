function test()
{    
    //
    // Basic test
    //

    var ast = esprima.parse('var answer = 42');
    //print(JSON.stringify(ast));

    if (typeof ast !== 'object')
        return 1;

    if (ast.type !== 'Program')
        return 2;

    //print(ast.body[0].declarations[0].init.hasOwnProperty('value'));
    if (ast.body[0].declarations[0].init.value !== 42)
        return 3;

    //
    // Test 1
    // - source locations, 
    // - function declaration
    // - function arguments ******* bugz
    // - if/else
    // - unary/binary operators
    // - conditional operator
    // - for loop
    // - function calls
    // - integer, string, boolean literals
    // - object literal ****** bugz
    // - array literal
    //

    var srcFile = readFile('programs/esprima/test1.js');
    var ast = esprima.parse(srcFile/*, { loc:true }*/);

    //print(JSON.stringify(ast));

    // TODO: recursively search for specific nodes?

    //
    // Self-parse test, esprima parsing esprima
    //

    /*
    var srcFile = readFile('programs/esprima/esprima.js');
    print('start...');
    var ast = esprima.parse(srcFile);
    print('...end');
    */

    return 0;
}

