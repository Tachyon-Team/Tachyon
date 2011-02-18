function test()
{
    var map = new HashMap();

    var keyList = ['a', 'b', 'c', 1, 2, 3];
    var valList = [1, 2, 3, 'a', 'b', 'c'];

    print('adding items');

    for (var i = 0; i < keyList.length; ++i)
    {
        print(keyList[i]);
        print(valList[i]);

        map.addItem(keyList[i], valList[i]);
    }

    print('getting items');

    for (var i = 0; i < keyList.length; ++i)
    {
        var val = map.getItem(keyList[i]);

        print(val);
        print(valList[i]);

        if (val !== valList[i])
            return 1;
    }

    return 0;
}

