/**
@fileOverview
Implementation of a hash map

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// Initial hash map size
var HASH_MAP_INIT_SIZE = 89;

// Hash map min and max load factors
var HASH_MAP_MIN_LOAD = 0.1;
var HASH_MAP_MAX_LOAD = 0.6;

// Next object serial number to be assigned
var nextObjectSerial = 1;

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
            hashCode = (((hashCode << 8) + val.charCodeAt(i)) * 331804471) & 536870911;

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

// Key value for free hash table slots
var freeHashKey = [];

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
        // Ensure that the item is not already in the table
        assert (
            !this.hasItem(key),
            'cannot add item, key already in hash map'
        );

        var index = 2 * (this.hashFunc(key) % this.numSlots);

        // Until a free item slot is found
        while (this.array[index] !== freeHashKey)
            index = (index + 2) % this.array.length;
    
        // Insert the new item at the free slot
        this.array[index] = key;
        this.array[index + 1] = value;

        // Increment the number of items stored
        this.numItems++;

        // If we are above the max load factor, expand the internal array
        if (this.numItems > this.numSlots * HASH_MAP_MAX_LOAD - 1)
            this.resize(2 * this.numSlots + 1);
    };

    /**
    Add or change a key-value binding in the map
    */
    this.setItem = function (key, value)
    {
        var index = 2 * (this.hashFunc(key) % this.numSlots);

        // Until a free cell is found
        while (this.array[index] !== freeHashKey)
        {
            // If this slot has the item we want
            if (this.equalFunc(this.array[index], key))
            {
                // Set the item's value
                this.array[index + 1] = value;
            }

            index = (index + 2) % this.array.length;
        }
    
        // Insert the new item at the free slot
        this.array[index] = key;
        this.array[index + 1] = value;

        // Increment the number of items stored
        this.numItems++;

        // If we are above the max load factor, expand the internal array
        if (this.numItems > this.numSlots * HASH_MAP_MAX_LOAD - 1)
            this.resize(2 * this.numSlots + 1);
    };

    /**
    Remove an item from the map
    */
    this.remItem = function (key)
    {    
        var index = 2 * (this.hashFunc(key) % this.numSlots);

        // Until a free cell is found
        while (this.array[index] !== freeHashKey)
        {
            // If this slot has the item we want
            if (this.equalFunc(this.array[index], key))
            {
                // Initialize the current free index to the removed item index
                var curFreeIndex = index;

                // For every subsequent item, until we encounter a free slot
                for (var shiftIndex = (index + 2) % this.array.length;
                    this.array[shiftIndex] !== freeHashKey;
                    shiftIndex = (shiftIndex + 2) % this.array.length)
                {
                    // Calculate the index at which this item's hash key maps
                    var origIndex = 2 * (this.hashFunc(this.array[shiftIndex]) % this.numSlots);

                    // Compute the distance from the element to its origin mapping
                    var distToOrig =
                        (shiftIndex < origIndex)? 
                        (shiftIndex + this.array.length - origIndex):
                        (shiftIndex - origIndex);

                    // Compute the distance from the element to the current free index
                    var distToFree =
                        (shiftIndex < curFreeIndex)?
                        (shiftIndex + this.array.length - curFreeIndex):
                        (shiftIndex - curFreeIndex);                    

                    // If the free slot is between the element and its origin
                    if (distToFree <= distToOrig)
                    {
                        // Move the item into the free slot
                        this.array[curFreeIndex] = this.array[shiftIndex];
                        this.array[curFreeIndex + 1] = this.array[shiftIndex + 1];

                        // Update the current free index
                        curFreeIndex = shiftIndex;
                    }
                }

                // Clear the hash key at the current free position
                this.array[curFreeIndex] = freeHashKey;

                // Decrement the number of items stored
                this.numItems--;

                // If we are under the minimum load factor, shrink the internal array
                if (this.numItems < this.numSlots * HASH_MAP_MIN_LOAD && this.numSlots > HASH_MAP_INIT_SIZE)
                    this.resize((this.numSlots - 1) >> 1);

                // Item removed
                return;
            }

            index = (index + 2) % this.array.length;
        }
    
        assert (false, 'cannot remove item, key not found');        
    };

    /**
    Test if the map contains an item
    */    
    this.hasItem = function (key)
    {
        var index = 2 * (this.hashFunc(key) % this.numSlots);

        // Until a free cell is found
        while (this.array[index] !== freeHashKey)
        {
            // If this slot has the item we want
            if (this.equalFunc(this.array[index], key))
            {
                // Item found
                return true;
            }

            index = (index + 2) % this.array.length;
        }
    
        // Item not found
        return false;
    };

    /**
    Get an item in the map
    */
    this.getItem = function (key)
    {
        var index = 2 * (this.hashFunc(key) % this.numSlots);

        // Until a free cell is found
        while (this.array[index] !== freeHashKey)
        {
            // If this slot has the item we want
            if (this.equalFunc(this.array[index], key))
            {
                // Return the item's value
                return this.array[index + 1];
            }

            index = (index + 2) % this.array.length;
        }
    
        assert (false, 'cannot get item, key not found');
    };

    /**
    Get the keys present in the hash map
    */
    this.getKeys = function ()
    {
        var keys = [];

        for (var i = 0; i < this.numSlots; ++i)
        {
            var index = 2 * i;

            if (this.array[index] !== freeHashKey)
                keys.push(this.array[index]);
        }

        return keys;
    };

    /**
    Erase all contained items
    */
    this.clear = function ()
    {
        // Set the initial number of slots
        this.numSlots = HASH_MAP_INIT_SIZE;

        // Set the initial array size
        this.array.length = 2 * this.numSlots;

        // Reset each array key element
        for (var i = 0; i < this.numSlots; ++i)
            this.array[2 * i] = freeHashKey;

        // Reset the number of items stored
        this.numItems = 0;
    };

    /**
    Copy the map
    */
    this.copy = function ()
    {
        var newMap = new HashMap(this.hashFunc, this.equalFunc);

        newMap.numSlots = this.numSlots;
        newMap.array = this.array.slice(0);
        newMap.numItems = this.numItems;

        return newMap;
    };

    /**
    Resize the hash map's internal storage
    */
    this.resize = function (newSize)
    {
        // Ensure that the new size is valid
        assert (
            this.numItems <= newSize && Math.round(newSize) - newSize == 0,
            'cannot resize, more items than new size allows'
        );

        var oldNumSlots = this.numSlots;
        var oldArray = this.array;

        // Initialize a new internal array
        this.array = [];
        this.numSlots = newSize;
        this.array.length = 2 * this.numSlots;
        for (var i = 0; i < this.numSlots; ++i)
            this.array[2 * i] = freeHashKey;

        // Reset the number of elements stored
        this.numItems = 0;

        // Re-insert the elements from the old array
        for (var i = 0; i < oldNumSlots; ++i)
            if (oldArray[2 * i] !== freeHashKey)
                this.addItem(oldArray[2 * i], oldArray[2 * i + 1]);     
    };

    /**
    Number of internal array slots
    @field
    */
    this.numSlots = HASH_MAP_INIT_SIZE;

    /**
    Internal storage array
    @field
    */
    this.array = []

    // Set the initial array size
    this.array.length = 2 * this.numSlots;

    // Initialize each array element
    for (var i = 0; i < this.numSlots; ++i)
        this.array[2 * i] = freeHashKey;

    /**
    Number of items stored
    @field
    */
    this.numItems = 0;

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

