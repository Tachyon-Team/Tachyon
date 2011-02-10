function test()
{
    var o = [3,4,5];

    o.x = 1;
    o.y = 2;

    var propNames = [];

    for (k in o)
        propNames[propNames.length] = k;

    var valid = [0,1,2,'x','y'];

    if (valid.length !== propNames.length)
        return 1;

    var i = 0;

    VALID_LOOP:
    for (var i = 0; i < valid.length; ++i)
    {
        var v = valid[i];

        for (var j = 0; j < propNames.length; ++j)
        {
            var p = propNames[j];

            if (v === p)
                continue VALID_LOOP;
        }

        // Property not found
        return 2;
    }

    return 0;
}
