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

    parElem.addChild(new XMLText(textStr));

    return parElem;
}

/**
Create an HTML table
*/
function HTMLTable(rowArray)
{
    var tblElem = new XMLElement('table', { border:1 });

    for (var rowIdx = 0; rowIdx < rowArray.length; ++rowIdx)
    {
        var rowElem = new XMLElement('tr');
        tblElem.addChild(rowElem);

        for (var colIdx = 0; colIdx < rowArray[rowIdx].length; ++colIdx)
        {
            var cellElem = new XMLElement('td');
            rowElem.addChild(cellElem);

            cellElem.addChild(new XMLText(rowArray[rowIdx][colIdx]));
        }
    }

    return tblElem;
}


// TODO: colored bar graph generation














