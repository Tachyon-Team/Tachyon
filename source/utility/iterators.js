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
/** Apply the given function to the remaining items in succession */
Iterator.prototype.forEach = function (fct) 
{ 
    for (; this.valid(); this.next())
    {
       if(fct(this.get()) === false)
       {
            break;
       }
    }
};


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

