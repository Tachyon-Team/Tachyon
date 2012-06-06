var o = {};

if (Math.random() > 0)
{
    o.k = 1;
}
else if (Math.random() > 0)
{
    o.k = 's';
}

typeAssert(o.k, '["and", "int", "string", "undef", ["not", "false"]]');

