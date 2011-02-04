function foo()
{
    //return arguments[0].length;
    return boxInt(iir.get_num_args());
}

function foo_proxy(x)
{
    return foo(x);
}

