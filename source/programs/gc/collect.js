function test()
{
    var liveObj1 = { v:3 };


    //print('before gcCollect');


    // Trigger a collection
    gcCollect();


    //print('after gcCollect');


    if (boxIsObj(liveObj1) === false)
        return 1;

    // FIXME: object property lookups won't work until string
    // references in code blocks are updated
    if (liveObj1.v !== 3)
        return 2;

    return 0;
}

