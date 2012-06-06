var o = { k:false };

if (Math.random() > 0)
{
    o.k = 1;
}
else
{
    o.k = 's';
}

typeAssert(o.k, '["and", "int", "string", ["not", "undef"]]');
