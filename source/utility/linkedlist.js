/**
@fileOverview
Implementation of a linked list data structure.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Linked list implementation
*/
function LinkedList()
{
    /**
    First list node
    @field
    */
    this.first = null;

    /**
    Last list node
    @field
    */
    this.last = null;
}
LinkedList.prototype = {};

/**
Get a string representation of the linked list
*/
LinkedList.prototype.toString = function ()
{
    var output = '(';

    for (var node = this.first; node !== null; node = node.next)
    {
        output += node.item.toString();

        if (node.next !== null)
            output += ',';
    }

    return output + ')';
};

/**
Get an iterator to the list
*/
LinkedList.prototype.getItr = function ()
{
    return new LinkedList.Iterator(this.first);
};

/**
Test if the list is empty
*/
LinkedList.prototype.isEmpty = function ()
{
    return (this.first === null);
};

/**
Clear all the list contents
*/
LinkedList.prototype.clear = function ()
{
    this.first = null;
    this.last = null;
};

/**
Get the first list element
*/
LinkedList.prototype.getFirst = function ()
{
    assert(
        !this.isEmpty(),
        'cannot get first list item, list empty'
    );

    return this.first;
};

/**
Get the last list element
*/
LinkedList.prototype.getLast = function ()
{
    assert(
        !this.isEmpty(),
        'cannot get last list item, list empty'
    );

    return this.last;
};

/**
Add an element to the beginning of the list
*/
LinkedList.prototype.addFirst = function (item)
{
    this.first = new LinkedList.Node(item, this.first);

    if (this.last === null)
        this.last = this.first;
};

/**
Add an element to the end of the list
*/
LinkedList.prototype.addLast = function (item)
{
    var newNode = new LinkedList.Node(item, null);

    if (this.first === null)
    {
        this.first = newNode;
        this.last = newNode; 
    }
    else
    {
        this.last.next = newNode;
        this.last = newNode;
    }

    assert (
        this.last === newNode,
        'error adding new node to linked list'
    );
};

/**
Add an element before an iterator's current position
*/
LinkedList.prototype.addBefore = function (item, itr)
{
    if (itr.prev === null)
        this.addFirst(item);
    else
        itr.prev.next = new LinkedList.Node(item, itr.current);
};

/**
Add an element after an iterator's current position
*/
LinkedList.prototype.addAfter = function (item, itr)
{
    if (itr.current === null)
        this.addLast(item);
    else
        itr.current.next = new LinkedList.Node(item, itr.current.next);
};

/**
Add an element to the end of the list
*/
LinkedList.prototype.addSorted = function (item, compFunc)
{
    // Find the node this item should be placed before
    var itr = this.find(item, compFunc);

    // Add the new item before the current node
    this.addBefore(item, itr);
};

/**
Remove an elemement from the beginning of the list
*/
LinkedList.prototype.remFirst = function ()
{
    assert (
        this.first !== null,
        'cannot remove first, list empty'
    );

    var item = this.first.item;

    this.first = this.first.next;

    if (this.first === null)
        this.last = null;

    return item;
};

/**
Remove an elemement at an iterator's position
*/
LinkedList.prototype.remItr = function (itr)
{
    assert (
        itr.valid(),
        'cannot remove item at iterator, iterator not valid'
    );

    if (itr.prev)
    {
        itr.prev.next = itr.current.next;
    }
    else
    {
        this.remFirst();
    }
};

/**
Find an element matching a comparison criteria
*/
LinkedList.prototype.find = function (item, compFunc)
{
    for (var itr = this.getItr(); itr.valid(); itr.next())
    {
        if (compFunc(item, itr.get()))
            break;
    }

    return itr;
};

/**
Obtain the contents of the list as an array
*/
LinkedList.prototype.toArray = function ()
{
    var output = [];

    for (var node = this.first; node !== null; node = node.next)
        output.push(node.item);

    return output;
};

/**
Create a linked list form an array's content
*/
LinkedList.fromArray = function (array)
{
    var list = new LinkedList();

    for (var i = 0; i < array.length; ++i)
        list.addLast(array[i]);

    return list;
};

/**
@class Linked list node
*/
LinkedList.Node = function (item, next)
{
    /**
    Internal item value
    @field
    */
    this.item = item;

    /**
    Reference to next node
    @field
    */
    this.next = next;
};

/**
@class Linked list iterator
*/
LinkedList.Iterator = function (node)
{
    /**
    Previous node
    @field
    */
    this.prev = null;

    /**
    Current node
    @field
    */
    this.current = node;
};
LinkedList.Iterator.prototype = {};

/**
Test if the iterator is at a valid position
*/
LinkedList.Iterator.prototype.valid = function ()
{
    return (this.current !== null);
};

/**
Move to the next list item
*/
LinkedList.Iterator.prototype.next = function ()
{
    assert (
        this.valid(),
        'cannot move to next list item, iterator not valid'
    );

    this.prev = this.current;
    this.current = this.current.next;
};

/**
Get the current list item
*/
LinkedList.Iterator.prototype.get = function ()
{
    assert (
        this.valid(),
        'cannot get current list item, iterator not valid'
    );

    return this.current.item;
};

