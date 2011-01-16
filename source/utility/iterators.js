/**
@fileOverview
Useful iterator objects and functions.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** 
    @class
    Base class for all iterators.
*/
function Iterator()
{
    assertNew(this);
    return this;
};
/** Move iterator to the next item */
Iterator.prototype.next  = function () { error("unimplemented"); };
/** Ensure iterator is still on a valid item.  Ex: Not at the end */
Iterator.prototype.valid = function () { error("unimplemented"); };
/** Returns the current item */
Iterator.prototype.get   = function () { error("unimplemented"); };
/** Insert an item at the current position */
Iterator.prototype.insert = function (item) { error("unimplemented"); };
/** Update the item at the current position with the new value */
Iterator.prototype.update = function (value) { error("unimplemented"); };


/**
    @class
    Filter results of an iterator such that only results for which func
    returns true are returned.

    @param {Iterator} it underlying iterator
    @param {Function} func filtering function. Receives an item an returns true
                      if the item should be returned, false otherwise.
    @augments Iterator
*/
function FilterIterator(it, func)
{
    assertNew(this);

    /** @private */
    this.func = func;

    /** @private */
    this.it = it;

    // Move to the first valid item
    while(this.it.valid() && !this.func(this.it.get()))
    {
        this.it.next(); 
    }
   
    return this;
};

FilterIterator.prototype = new Iterator();

/** Move iterator to the next item */
FilterIterator.prototype.next = function ()
{
    this.it.next();
    while(this.it.valid() && !this.func(this.it.get()))
    {
        this.it.next(); 
    }
};

/** Ensure iterator is still on a valid item.  Ex: Not at the end */
FilterIterator.prototype.valid = function ()
{
    return this.it.valid();     
};

/** Returns the current item */
FilterIterator.prototype.get = function ()
{
    return this.it.get();
};

/** Returns the index of the underlying iterator */
FilterIterator.prototype.getIndex = function ()
{
    return this.it.getIndex();
};

