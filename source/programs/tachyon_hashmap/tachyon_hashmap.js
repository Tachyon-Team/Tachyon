function test()
{
    var map = new HashMap();

    var keyList = ['a', 'b', 'c', 1, 2, 3];
    var valList = [1, 2, 3, 'a', 'b', 'c'];

    /*
    print('num items: ' + map.numItems);
    print('num slots: ' + map.numSlots);
    print('array length: ' + map.array.length);

    print('adding items');
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
        var val = map.getItem(keyList[i]);

        /*
        print('key: ' + keyList[i]);
        print('val: ' + valList[i]);
        print('got val: ' + val);
        */

        if (val !== valList[i])
            return 1;
    }

    //
    // TODO: **** add more tests here ****
    // - hasItem
    // - remItem
    // - iterator
    //

    return 0;
}

