/**
@fileOverview
Implementation of string operations.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// TODO: stare at IR, look for errors...

/**
Allocate and initialize the string table, used for hash consing
*/
function initStrTable()
{
    "tachyon:static";


    // TODO
}

/**
Compare two raw UTF-16 strings by iterating over 16 bit code units
This conforms to section 11.8.5 of the ECMAScript 262 specification
NOTE: this is also used to find strings in the hash consing table
*/
function strcmp(str1, str2)
{
    // TODO

    // TODO: define constant for raw string data offset?
}

/**
Allocate/get a reference to a string object containing the given string data
@param strData pointer to raw UTF-16 string data
*/
function getStrObj(strData, strLen)
{
    "tachyon:static";
    "tachyon:arg strData rptr";
    "tachyon:arg strLen pint";

    //
    // TODO: maintain a hash set of allocated strings
    // this is needed for equality comparison by direct reference comparison
    //
    // Need:
    // mem layout of str table
    // ref to table in context object (tag other)
    // str table allocation in heapInit()
    // - initial size ~101 (prime)
    // search algorithm
    // - involves string comparison
    //
    // For now, no resizing of table

    // Allocate a string object
    var strObj = alloc_str(strLen);

    // Set the string length in the string object
    set_str_len(strObj, strLen);

    // Initialize the hash code to 0
    var hashCode = iir.constant(IRType.pint, 0);

    // For each character, update the hash code
    for (
        var index = iir.constant(IRType.pint, 0); 
        true;
        index = index + iir.constant(IRType.pint, 1)
    )
    {
        // Get the current character
        var ch = iir.load(IRType.u16, strData, index);

        // Copy the character into the string object
        set_str_data(strObj, index, ch);

        // Convert the character value to the pint type
        var ch = iir.icast(IRType.pint, ch);

        // If this is the null terminator, break out of the loop
        if (ch == iir.constant(IRType.pint, 0))
            break;

        // Update 
        hashCode =
            (hashCode * iir.constant(IRType.pint, 256) + ch) %
            iir.constant(IRType.pint, 426870919);
    }

    // Set the hash code in the string object
    set_str_hash(strObj, iir.icast(IRType.i32, hashCode));

    // Return a reference to the string object
    return strObj;
}

