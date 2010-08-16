/**
@fileOverview
Unit tests for utility code.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Test suite for utility code
*/
tests.utility = tests.testSuite();

/**
Test of JS string escaping code
*/
tests.utility.strEscape = function()
{
    str1 = 'foo\f\v\b\r\nbar\"\'\ubbbb\\bif\u0aaabuz\tfoo'
    str2 = escapeJSString(str1);
    str3 = 'foo\\f\\v\\b\\r\\nbar\\"\\\'\\ubbbb\\\\bif\\u0aaabuz\\tfoo';

    assert (str2 == str3);
}

/**
Test of text indenting code
*/
tests.utility.strIndent = function()
{
    str1 = 'foo\nbar\n'
    str2 = indentText(str1, '\t');
    str3 = '\tfoo\n\tbar\n';

    assert (str2 == str3);
}

/**
Test of the hash map functionality
@field
*/
tests.utility.hashMap = function ()
{
    var NUM_ELEMS = 400;
    var REM_ELEMS = 370;

    var map = new HashMap();

    var itemList = [];
    var keyList = [];

    for (var i = 0; i < NUM_ELEMS; ++i)
    {
        keyList.push(i / 2);
        itemList.push(i);

        map.addItem(keyList[i], itemList[i]);
    }

    for (var repeat = 0; repeat < 10; ++repeat)
    {
        var remList = [];

        for (var i = 0; i < REM_ELEMS; ++i)
        {
            var idx = Math.floor(Math.random() * keyList.length);

            map.remItem(keyList[idx]);

            if (map.hasItem(keyList[idx]))
                throw 'removed item still in table';

            remList.push(itemList[idx]);

            keyList.splice(idx, 1);
            itemList.splice(idx, 1);
        }

        for (var i = 0; i < keyList.length; ++i)
        {
            if (!map.hasItem(keyList[i]))
                throw 'item not found in table';

            if (map.getItem(keyList[i]) != itemList[i])
                throw 'item extracted does not match item inserted';
        }

        for (var i = 0; i < remList.length; ++i)
        {
            keyList.push(remList[i] / 2);
            itemList.push(remList[i]);

            map.addItem(keyList[keyList.length - 1], itemList[keyList.length - 1]);
        }
    }
}

