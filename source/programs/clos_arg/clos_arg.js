function callee()
{
}

function caller(params)
{
    callee('In caller');

    function nested()
    {
        params = true;
    }

    nested(params);
}

function test()
{
    callee('calling caller');

    caller(3);

    return 0;
}

