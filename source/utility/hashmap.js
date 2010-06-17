/**
@fileOverview
Implementation of a hash map

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// Initial hash map size
HASH_MAP_INIT_SIZE = 89;

// Hash map expansion threshold
HASH_MAP_EXPAND_THRESHOLD = 0.7;

// Next object serial number to be assigned
nextObjectSerial = 1;

/**
Default hash function implementation
*/
function defHashFunc(val)
{
    if (typeof val == 'number')
    {
        return parseInt(val);
    }
    else if (typeof val == 'string')
    {
        var hashCode = 0;

        for (var i = 0; i < val.length; ++i)
            hashCode = hashCode << 1 + val[i];

        return hashCode;
    }
    else
    {
        if (!val.hasOwnProperty('__hashCode__'))
        {
            val.__hashCode__ = nextObjectSerial++;
        }

        return val.__hashCode__;
    }
}

/**
Default equality function
*/
function defEqualFunc(key1, key2)
{
    return key1 === key2;
}

/**
@class Hash map implementation
*/
function HashMap(hashFunc, equalFunc)
{
    /**
    Add an item to the map
    */
    this.addItem = function (key, value)
    {
        if (this.numElems + 1 > this.array.length * HASH_MAP_EXPAND_THRESHOLD)
            this.expand();

        var index = this.hashFunc(key) % this.array.length;

        this.array[index].push(key);
        this.array[index].push(value);
    }

    /**
    Remove an item from the map
    */
    this.remItem = function (key)
    {
        var index = this.hashFunc(key) % this.array.length;
        var itemList = this.array[index];
        var numItems = itemList.length / 2;

        for (var i = 0; i < numItems; ++i)
        {
            if (this.equalFunc(itemList[2 * i], key))
                itemList.splice(2 * i, 2);
        }      
    }

    /**
    Test if the map contains an item
    */    
    this.hasItem = function (key)
    {
        var index = this.hashFunc(key) % this.array.length;
        var itemList = this.array[index];
        var numItems = itemList.length / 2;

        for (var i = 0; i < numItems; ++i)
        {
            if (this.equalFunc(itemList[2 * i], key))
                return true;
        }

        return false;
    }

    /**
    Get an item in the map
    */
    this.getItem = function (key)
    {
        var index = this.hashFunc(key) % this.array.length;
        var itemList = this.array[index];
        var numItems = itemList.length / 2;

        for (var i = 0; i < numItems; ++i)
        {
            if (this.equalFunc(itemList[2 * i], key))
                return itemList[2 * i + 1];
        }

        throw 'Item not found in hash map';
    }

    /**
    Expand the hash map's internal storage
    */
    this.expand = function (key)
    {
        var oldArray = this.array;

        this.array = [];
        this.array.length = 2 * oldArray.length + 1;
        for (var i = 0; i < this.array.length; ++i)
            this.array[i] = [];

        for (var i = 0; i < oldArray.length; ++i)
        {
            var itemList = oldArray[i];

            for (var j = 0; j < itemList.length; ++j)
                this.addItem(itemList[2 * j], itemList[2 * j + 1]);
        }
    }

    /**
    Internal storage array
    @field
    */
    this.array = []

    // Set the initial array size
    this.array.length = HASH_MAP_INIT_SIZE;

    // Initialize each array element
    for (var i = 0; i < this.array.length; ++i)
        this.array[i] = [];

    /**
    Number of elements stored
    @field
    */
    this.numElems = 0;

    // If no hash function was specified, use the default function
    if (hashFunc == undefined || hashFunc == null)
        hashFunc = defHashFunc;

    /**
    Hash function
    @field
    */
    this.hashFunc = hashFunc;

    // If no hash function was specified, use the default function
    if (equalFunc == undefined || equalFunc == null)
        equalFunc = defEqualFunc;

    /**
    Key equality function
    @field
    */
    this.equalFunc = equalFunc;
}

/*
map = new HashMap();

a = {x:1};
b = {x:2};
c = {x:3};

map.addItem(a, '1');
map.addItem(b, '2');
map.addItem(c, '3');

print(map.getItem(a));
print(map.getItem(b));
print(map.getItem(c));

map.remItem(b);

print(map.hasItem(a));
print(map.hasItem(b));
print(map.hasItem(c));
*/

