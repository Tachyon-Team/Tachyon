/**
@fileOverview
Unit tests for utility code.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Test of JS string escaping code
*/
function strEscapeTest()
{
    str1 = 'foo\f\v\b\r\nbar\"\'\ubbbb\\bif\u0aaabuz\tfoo'
    str2 = escapeJSString(str1);
    str3 = 'foo\\f\\v\\b\\r\\nbar\\"\\\'\\ubbbb\\\\bif\\u0aaabuz\\tfoo';

    return str2 == str3;
}

/**
Test of text indenting code
*/
function strIndentTest()
{
    str1 = 'foo\nbar\n'
    str2 = indentText(str1, '\t');
    str3 = '\tfoo\n\tbar\n';

    return str2 == str3;
}

/**
Test of the hash map functionality
@field
*/
function hashMapTest()
{
    map = new HashMap();

    var itemList = [];
    var keyList = [];

    for (var i = 0; i < 200; ++i)
    {
        keyList.push({x:i});
        itemList.push(String(i));

        map.addItem(keyList[i], itemList[i]);
    }

    map.expand();

    for (var i = 0; i < 50; ++i)
    {
        var idx = (1337 + 3 * i) % keyList.length;

        map.remItem(keyList[idx]);

        if (map.hasItem(keyList[idx]))
            return 'removed item still in table';

        keyList.splice(idx, 1);
        itemList.splice(idx, 1);
    }    
    
    for (var i = 0; i < keyList.length; ++i)
    {
        if (!map.hasItem(keyList[i]))
            return 'item not found in table';

        if (map.getItem(keyList[i]) != itemList[i])
            return 'item extracted does not match item inserted';
    }

    return true;
}

/**
Test suite for utility code
*/
function makeUtilitySuite()
{
    suite = new TestSuite('utility code');

    suite.addTest(new TestCase('JS string escaping', strEscapeTest));
    suite.addTest(new TestCase('Text indenting', strIndentTest));
    suite.addTest(new TestCase('Hash map functionality', hashMapTest));

    return suite;
}

