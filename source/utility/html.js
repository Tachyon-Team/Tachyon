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
}

/**
Create an HTML header
*/
function HTMLHeader(level, titleStr)
{
    assert (
        typeof level === 'number' && level >= 1 && level <= 6,
        'invalid header level'
    );

    var elem = new XMLElement('h' + level.toString());

    elem.title = new XMLText(titleStr);
    elem.addChild(elem.title);

    return elem;
}

/**
Create an HTML horizontal separator
*/
function HTMLSep()
{
    return new XMLElement('hr', {}, true);
}


// TODO: table generation

// TODO: colored bar graph generation














