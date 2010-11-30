/**
@fileOverview
XML tree generation/manipulation code.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Base class for XML nodes
*/
function XMLNode()
{
}

/**
@class Represent an XML document
*/
function XMLDocument(rootElem, isHTML)
{
    /**
    Root XML element
    @field
    */
    this.root = rootElem;

    /**
    Flag to indicate that this is an HTML document
    @field
    */
    this.isHTML = isHTML;
}
XMLDocument.prototype = new XMLNode();

/**
Produce a string representation of an XML document
*/
XMLDocument.prototype.toString = function ()
{
    return this.root.toString(this, '');
}

/**
@class Represent an XML/HTML element
*/
function XMLElement(name, attribs, isLeaf)
{
    assert (
        !attribs || attribs instanceof Object,
        'the attributes must be provided in an object'
    );

    /**
    Element name
    @field
    */
    this.name = name;

    /**
    Dictionary of attributes for this element
    @field
    */
    this.attribs = attribs || {};

    /**
    List of children elements
    @field
    */
    this.children = [];

    /**
    Flag to indicate that this is a leaf
    @field
    */
    this.isLeaf = isLeaf;
}
XMLElement.prototype = new XMLNode();

/**
Produce a string representation of this node
*/
XMLElement.prototype.toString = function (document, indent)
{
    var escName = escapeXMLString(this.name);

    // Declare a string for the output
    var output = indent + '<' + escName;

    // For each attribute
    for (var attrName in this.attribs)
    {
        output += ' ' + escapeXMLString(attrName) + '="';
        output += escapeXMLString(this.attribs[attrName].toString()) + '"';
    }

    output += (this.isLeaf? ' />':'>\n');

    // For each child node
    for (var i = 0; i < this.children.length; ++i)
    {
        output += this.children[i].toString(document, indent + '  ') + '\n';
    }

    if (!this.isLeaf)
        output += indent + '</' + escName + '>'

    // Return the output string
    return output;
}

/**
Add a new child to an XML element
*/
XMLElement.prototype.addChild = function (childNode)
{
    assert (
        childNode instanceof XMLNode,
        'new child should be XML node'
    );

    this.children.push(childNode);
}

/**
@class XML text string node
*/
function XMLText(text)
{
    /**
    Text contents of this node
    @field
    */
    this.text = text;
}
XMLText.prototype = new XMLNode();

/**
Produce a string representation of this node
*/
XMLText.prototype.toString = function (document, indent)
{
    // Escape and indent the string
    return indentText(escapeXMLString(this.text), indent);
}

