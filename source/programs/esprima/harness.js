function test()
{    
    var ast = esprima.parse('var answer = 42');

    //print(JSON.stringify(ast));

    if (typeof ast !== 'object')
        return 1;

    if (ast.type !== 'Program')
        return 2;

    //print(ast.body[0].declarations[0].init.hasOwnProperty('value'));
    if (ast.body[0].declarations[0].init.value !== 42)
        return 3;

    return 0;
}

