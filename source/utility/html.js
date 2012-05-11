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
HTML code generation utilities.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Represents an HTML page
*/
function HTMLPage(titleStr)
{
    /**
    Root page element
    @field
    */
    this.root = new XMLElement('html');

    /**
    Element representing the page head
    @field
    */
    this.head = new XMLElement('head');

    /**
    Element representing the page body
    @field
    */
    this.body = new XMLElement('body');

    // Add the head and body elements to the root's children
    this.root.addChild(this.head);
    this.root.addChild(this.body);

    // Add an element for the title to the page head
    var title = new XMLElement('title');
    title.addChild(new XMLText(titleStr || ''));
    this.head.addChild(title);
}
HTMLPage.prototype = new XMLDocument();

/**
Save the code of an HTML page to a text file
*/
HTMLPage.prototype.toFile = function (fileName)
{
    writeFile(fileName, this.toString());
};

/**
Add contents to the page body
*/
HTMLPage.prototype.addContents = function (elem)
{
    this.body.addChild(elem);
};

/**
Create an HTML header
*/
function HTMLHeader(level, titleStr, center)
{
    assert (
        typeof level === 'number' && level >= 1 && level <= 6,
        'invalid header level'
    );

    var elem = new XMLElement('h' + level.toString());

    elem.title = new XMLText(titleStr);
    elem.addChild(elem.title);

    if (center)
    {
        ctrElem = new XMLElement('center');
        ctrElem.addChild(elem);
        elem = ctrElem;
    }

    return elem;
}

/**
Create an HTML horizontal separator
*/
function HTMLSep()
{
    return new XMLElement('hr', {}, true);
}

/**
Create an HTML paragraph
*/
function HTMLPar(textStr)
{
    var parElem = new XMLElement('p');

    var textElem = new XMLText(textStr);

    textElem.toString = function (document, indent)
    {
        // Escape and indent the string
        return indentText(escapeXMLString(this.text, true, true), indent);    
    };

    parElem.addChild(textElem);

    return parElem;
}

/**
Create an XML pre tag
*/
function HTMLPre()
{
    var pre = new XMLElement('pre', {}, false, true);

    return pre;
}

/**
Create an HTML table
*/
function HTMLTable()
{
    var tblElem = new XMLElement('table', { border:1 });

    tblElem.addRow = function (row)
    {
        var rowElem = new XMLElement('tr');
        tblElem.addChild(rowElem);
    };

    tblElem.addCell = function (contents, attribs)
    {
        var cellElem = new XMLElement('td', attribs);
        tblElem.children[tblElem.children.length-1].addChild(cellElem);

        if (contents instanceof Array)
        {
            for (var i = 0; i < contents.length; ++i)
                cellElem.addChild(contents[i]);
        }
        else
        {
            if (typeof contents === 'string')
                contents = HTMLPar(contents);

            cellElem.addChild(contents);
        }
    };

    return tblElem;
}

// TODO: colored bar graph generation

