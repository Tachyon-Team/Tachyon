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
Test suite for utility code
*/
function makeUtilitySuite()
{
    suite = new TestSuite('utility code');

    suite.addTest(new TestCase('JS string escaping', strEscapeTest));
    suite.addTest(new TestCase('Text indenting', strIndentTest));

    return suite;
}

