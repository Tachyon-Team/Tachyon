var o = { k:3 };

function foo()
{
    typeAssert(o, '"object"');

    typeAssert(o.p, '["and", "undef", ["not", "missing"]]');

    typeAssert(o.k, '"int"');    
}

foo();

