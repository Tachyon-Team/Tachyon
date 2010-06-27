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
HASH_MAP_EXPAND_THRESHOLD = 0.5;

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

// Key value for free hash table slots
freeHashKey = [];

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
        // If we are past the expansion threshold, expand the internal array
        if (this.numItems + 1 > this.numSlots * HASH_MAP_EXPAND_THRESHOLD)
            this.expand();

        var index = 2 * (this.hashFunc(key) % this.numSlots);

        // Until a free item slot is found
        while (this.array[index] !== freeHashKey)
            index = (index + 2) % this.array.length;
    
        // Inser the new item at the free slot
        this.array[index] = key;
        this.array[index + 1] = value;

        // Increment the number of items stored
        this.numItems++;
    }

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
                // Shift the following items left to keep clusters contiguous
                for (;;)
                {
                    // Compute the index of the following item
                    var nextIndex = (index + 2) % this.array.length;

                    // If this is a free item, stop
                    if (this.array[nextIndex] === freeHashKey)
                        break;

                    // Calculate the index at which this item's hash key maps
                    var origIndex = 2 * (this.hashFunc(this.array[nextIndex]) % this.numSlots);
       
                    // If the item was mapped directly at its intended location, stop
                    if (origIndex == nextIndex)
                        break;

                    // Shift the item left, erasing the previous item
                    this.array[index] = this.array[nextIndex];
                    this.array[index + 1] = this.array[nextIndex + 1];
            
                    // Move to the next item
                    index = nextIndex;
                }

                // Clear the hash key at the current position
                this.array[index] = freeHashKey;

                // Decrement the number of items stored
                this.numItems--;

                // Item removed
                return;
            }

            index = (index + 2) % this.array.length;
        }
    
        assert (false, 'cannot remove item, key not found');        
    }

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
    }

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
    }

    /**
    Expand the hash map's internal storage
    */
    this.expand = function ()
    {
        var oldNumSlots = this.numSlots;
        var oldArray = this.array;

        // Initialize a new internal array
        this.array = [];
        this.numSlots = 2 * oldNumSlots + 1;
        this.array.length = 2 * this.numSlots;
        for (var i = 0; i < this.numSlots; ++i)
            this.array[2 * i] = freeHashKey;

        // Reset the number of elements stored
        this.numItems = 0;

        // Re-insert the elements from the old array
        for (var i = 0; i < oldNumSlots; ++i)
            if (oldArray[2 * i] !== freeHashKey)
                this.addItem(oldArray[2 * i], oldArray[2 * i + 1]);     
    }

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

