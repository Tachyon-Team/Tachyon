/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

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
tests.utility = tests.utility || tests.testSuite();

/**
Test of JS string escaping code
*/
tests.utility.strEscape = function()
{
    str1 = 'foo\f\v\b\r\nbar\"\'\ubbbb\\bif\u0aaabuz\tfoo';
    str2 = escapeJSString(str1);
    str3 = 'foo\\f\\v\\b\\r\\nbar\\"\\\'\\ubbbb\\\\bif\\u0aaabuz\\tfoo';

    assert (str2 == str3);
};

/**
Test of text indenting code
*/
tests.utility.strIndent = function()
{
    str1 = 'foo\nbar\n';
    str2 = indentText(str1, '\t');
    str3 = '\tfoo\n\tbar\n';

    assert (str2 == str3);
};

/**
Test suite for the hash map code
*/
tests.utility.hashMap = tests.testSuite();

/**
Test of hash map consistency
*/
tests.utility.hashMap.consistency = function ()
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

        map.set(keyList[i], itemList[i]);
    }

    for (var repeat = 0; repeat < 10; ++repeat)
    {
        var remList = [];

        for (var i = 0; i < REM_ELEMS; ++i)
        {
            var idx = Math.floor(Math.random() * keyList.length);

            map.rem(keyList[idx]);

            if (map.has(keyList[idx]) === true)
                error('removed item still in table');

            remList.push(itemList[idx]);

            keyList.splice(idx, 1);
            itemList.splice(idx, 1);
        }

        for (var i = 0; i < keyList.length; ++i)
        {
            if (map.has(keyList[i]) === false)
                error('item not found in table');

            if (map.get(keyList[i]) != itemList[i])
                error('item extracted does not match item inserted');
        }

        for (var i = 0; i < remList.length; ++i)
        {
            keyList.push(remList[i] / 2);
            itemList.push(remList[i]);

            map.set(keyList[keyList.length - 1], itemList[keyList.length - 1]);
        }
    }
};

/**
Test of hash map iterators
*/
tests.utility.hashMap.iterator = function ()
{
    var input = [1,2,3,4,55];

    var map = new HashMap();

    for (var i = 0; i < input.length; ++i)
        map.set(input[i], input[i]);

    var output = [];

    for (var itr = map.getItr(); itr.valid(); itr.next())
    {
        var cur = itr.get();

        assert(cur.key === cur.value);

        output.push(cur.key);
    }

    assert (
        input.toString() == output.sort().toString(),
        'did not get all heap values during iteration'
    );
};

/**
Test suite for the linked list code
*/
tests.utility.linkedList = tests.testSuite();

/**
Test of linked list insertion
*/
tests.utility.linkedList.insert = function ()
{
    var list = new LinkedList();

    list.addFirst(1);
    assert (list.getFirst().item === 1);
    list.addLast(2);
    assert (list.getLast().item === 2);
    list.addFirst(0);
    list.addLast(3);

    list.addSorted(0.5, function (i1, i2) { return i1 < i2; });

    assert (list == '(0,0.5,1,2,3)');
};

/**
Test of linked list iteration
*/
tests.utility.linkedList.iterator = function ()
{
    var array = [0,1,2,3,4];
    var list = LinkedList.fromArray(array);

    var idx = 0;
    var itr = list.getItr();
    for (; itr.valid(); itr.next(), idx++)
    {
        assert (itr.get() === array[idx]);
    }
};

/**
Test of number formatting with decimals
*/
tests.utility.fmtNumDecimals = function ()
{
    assert (
        fmtNumDecimals(10.35, 1) == '10.4'
    );

    assert (
        fmtNumDecimals(10, 0) == '10'
    );

    assert (
        fmtNumDecimals(-35.2466, 2) == '-35.25'
    );

    assert (
        fmtNumDecimals(Number('-1.349338e+30'), 2) == '-1.35e+30'
    );

    assert (
        fmtNumDecimals(Number('-9.99e+30'), 1) == '-1.0e+31'
    );

    assert (
        fmtNumDecimals(Number('9.98e-30'), 1) == '1.0e-29'
    );

    assert (    
        fmtNumDecimals(89.999, 2) == '90.00'
    );

    assert (    
        fmtNumDecimals(99.999, 2) == '100.00'
    );
};

/**
Test suite for XML/HTML code
*/
tests.utility.xml = tests.testSuite();

/**
Test of XML string escaping
*/
tests.utility.xml.strEscape = function ()
{
    var origStr = 'foo & bar <3 >3 "lol" \'lol\'';
    var validStr = 'foo&nbsp;&amp;&nbsp;bar&nbsp;&lt;3&nbsp;&gt;3&nbsp;&quot;lol&quot;&nbsp;&apos;lol&apos;';

    assert (escapeXMLString(origStr) === validStr);
};

/**
Test of XML code generation
*/
tests.utility.xml.gendoc = function ()
{
    var xmlRoot = new XMLElement('doc');
    var xmlDoc = new XMLDocument(xmlRoot);

    var listElem = new XMLElement('list');
    listElem.attribs.length = 3;
    xmlRoot.addChild(listElem);

    listElem.addChild(new XMLElement('foo', { val:1 }, true));
    listElem.addChild(new XMLElement('foo', { val:2 }, true));
    listElem.addChild(new XMLElement('foo', { val:3 }, true));

    xmlRoot.addChild(new XMLText('text contents yo!'));

    xmlRoot.addChild(new XMLElement('br', {}, true));

    var str = xmlDoc.toString();

    //print(str);
};

/**
Test of HTML code generation
*/
tests.utility.xml.genhtml = function ()
{
    var page = new HTMLPage('The Test Page');

    page.addContents(new HTMLHeader(1, 'foo'));
    page.addContents(new HTMLHeader(2, 'bar'));
    page.addContents(new HTMLSep());
    page.addContents(new HTMLHeader(3, 'bif'));
    page.addContents(new HTMLSep());

    var str = page.toString();

    //print(str);

    //page.toFile('test.html');
};

