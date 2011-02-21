function test()
{
    var map = new HashMap();

    var keyList = [];
    var valList = [];

    for (var i = 0; i < 50; ++i)
    {
        keyList.push('k' + i);
        valList.push(i);
    }

    for (var i = 0; i < 50; ++i)
    {
        keyList.push(i);
        valList.push('v' + i);
    }

    /*
    print('num items: ' + map.numItems);
    print('num slots: ' + map.numSlots);
    print('array length: ' + map.array.length);
    */

    for (var i = 0; i < keyList.length; ++i)
    {
        /*
        print('key: ' + keyList[i]);
        print('val: ' + valList[i]);
        print('hash: ' + defHashFunc(keyList[i]));
        */

        map.addItem(keyList[i], valList[i]);
    }

    /*
    print('getting items');
    */

    for (var i = 0; i < keyList.length; ++i)
    {
        if (!map.hasItem(keyList[i]))
            return 1;

        var val = map.getItem(keyList[i]);

        /*
        print('key: ' + keyList[i]);
        print('val: ' + valList[i]);
        print('got: ' + val);
        */

        if (val !== valList[i])
            return 2;
    }

    ITR_LOOP:
    for (var itr = map.getItr(); itr.valid(); itr.next())
    {
        var cur = itr.get();

        for (var i = 0; i < keyList.length; ++i)
        {
            if (keyList[i] === cur.key)
            {
                if (valList[i] !== cur.value)
                    return 3;

                continue ITR_LOOP;
            }
        }

        return 4;
    }

    for (var i = 0, c = 0; i < keyList.length; ++i, ++c)
    {
        if (c % 3 === 0)
        {
            map.remItem(keyList[i]);

            keyList.splice(i, 1);
            valList.splice(i, 1);

            --i;
        }
    }

    for (var i = 0; i < keyList.length; ++i)
    {
        if (!map.hasItem(keyList[i]))
            return 5;

        var val = map.getItem(keyList[i]);

        if (val !== valList[i])
            return 6;
    }

    return 0;
}

