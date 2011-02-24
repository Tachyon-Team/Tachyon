/**
@fileOverview
Set implementation

@author
Erick Lavoie

@copyright
Copyright (c) 2010 Erick Lavoie, All Rights Reserved
*/

/**
@class Set implementation using a hash map
*/
function HashSet(hashFunc, equalFunc)
{
    /**
    Internal hash map
    @private
    */
    this.hashMap = new HashMap(hashFunc, equalFunc);

    /**
    Number of items
    @field
    */
    this.length = 0;
}

HashSet.prototype = {};

/**
Test if a set contains a value
*/
HashSet.prototype.has = function (val)
{
    return this.hashMap.hasItem(val);
};

/**
Add a value to the set
*/
HashSet.prototype.add = function (val)
{
    this.hashMap.addItem(val, val);
    this.length = this.hashMap.numItems;
    return this;
};

/**
Add all values from array to the set
*/
HashSet.prototype.addArray = function (arr)
{
    const that = this;

    arr.forEach(function (val)
    {
        that.add(val);    
    });
    return this;
};

/**
Remove a value from the set
*/
HashSet.prototype.rem = function (val)
{
    if (this.hashMap.hasItem(val))
    {
        this.hashMap.remItem(val);
        this.length = this.hashMap.numItems;
    }
    return this;
};

/**
Remove all values in array from the set
*/
HashSet.prototype.remArray = function (arr)
{
    const that = this;
    arr.forEach(function (val)
    {
        that.rem(val);
    });
    return this;
};

/**
In-place set difference (relative complement)
*/
HashSet.prototype.diff = function (set)
{
    const that = this;

    for (var it = set.hashMap.getItr(); it.valid(); it.next())
    {
        var item = it.get();
        if (that.hashMap.hasItem(item.key))
        {
            that.rem(item.key);
        }
    };
    
    return this;
};

/**
In-place union
*/
HashSet.prototype.union = function (set)
{
    const that = this;

    for (var it = set.hashMap.getItr(); it.valid(); it.next())
    {
        that.add(it.get().key);    
    };

    return this;
};

/*
In-place intersection
*/
HashSet.prototype.intr = function (set)
{
    const that = this;

    this.hashMap.getKeys().forEach(function (key)
    {
        if (!set.has(key))
        {
            that.rem(key);
        }
    });
    return this;
};


/**
Test set for equality
*/
HashSet.prototype.equal = function (set)
{
    if (set.length !== this.length)
    {
        return false;
    }

    for (var it = set.hashMap.getItr(); it.valid(); it.next())
    {
        if(!set.has(it.get().key))
        {
            return false;    
        }
    };

    return true;
};

/**
Clear the set content
*/
HashSet.prototype.clear = function ()
{
    this.hashMap.clear();
    this.length = 0;
    return this;
};

/**
Copy the set
*/
HashSet.prototype.copy = function ()
{
    var h = Object.create(this);
    h.hashMap = this.hashMap.copy(); 
    return h;
};

