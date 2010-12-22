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

    // Allocate the string table object
    var strtbl = alloc_strtbl(STR_TBL_INIT_SIZE);

    // Initialize the hash table size and number of properties
    set_strtbl_tblsize(strtbl, STR_TBL_INIT_SIZE);
    set_strtbl_numstrs(strtbl, i32(0));

    // Initialize the string table
    for (
        var i = pint(0); 
        i < STR_TBL_INIT_SIZE; 
        i += pint(1)
    )
    {
        set_strtbl_tbl(strtbl, i, UNDEFINED);
    }


    // Get a pointer to the context
    var ctx = iir.get_ctx();

    // Set the string table reference in the context
    set_ctx_strtbl(ctx, strtbl);
}

/**
Compare two raw UTF-16 strings by iterating over 16 bit code units
This conforms to section 11.8.5 of the ECMAScript 262 specification
NOTE: this is also used to find strings in the hash consing table
*/
function strcmp(str1, str2)
{
    "tachyon:arg str1 rptr";
    "tachyon:arg str2 rptr";
    "tachyon:ret pint";

    // For each character to be compared
    for (;;)
    {
        var ch1 = iir.load(IRType.u16, str1, pint(0));
        var ch2 = iir.load(IRType.u16, str1, pint(0));

        if (ch1 < ch2)
            return pint(-1);
        else if (ch1 > ch2)
            return pint(1);
        
        if (ch1 == u16(0))
            break;

        str1 += pint(2);
        str2 += pint(2);
    }

    // The strings are equal
    return pint(0);
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
    // Hash code computation
    //

    // Initialize the hash code to 0
    var hashCode = pint(0);

    // For each character, update the hash code
    for (var index = pint(0); true; index = index + pint(1))
    {
        // Get the current character
        var ch = iir.load(IRType.u16, strData, index);

        // Convert the character value to the pint type
        var ch = iir.icast(IRType.pint, ch);

        // If this is the null terminator, break out of the loop
        if (ch == pint(0))
            break;

        // Update 
        hashCode = (hashCode * pint(256) + ch) % pint(426870919);
    }

    //
    // Hash table lookup
    //

    // Get a pointer to the context
    var ctx = iir.get_ctx();

    // Get a pointer to the string table
    var strtbl = get_ctx_strtbl(ctx);

    // Get the size of the string table
    var tblSize = iir.icast(
        IRType.pint,
        get_strtbl_tblsize(strtbl)
    );

    // Get the hash table index for this hash value
    var hashIndex = hashCode % tblSize;

    // Until the key is found, or a free slot is encountered
    while (true)
    {
        // Get the string value at this hash slot
        var strVal = get_strtbl_tbl(strtbl, hashIndex);

        // If this is the string we want
        // TODO: string comparison
        if (false)
        {
            // Return a pointer to the string we found
            return strVal;
        }

        // Otherwise, if we have reached an empty slot
        else if (strVal === UNDEFINED)
        {
            // Break out of the loop
            break;
        }

        // Move to the next hash table slot
        hashIndex = (hashIndex + pint(1)) % tblSize;
    }

    //
    // String object allocation
    //

    // Allocate a string object
    var strObj = alloc_str(strLen);
    
    // Set the string length in the string object
    set_str_len(strObj, strLen);

    // Set the hash code in the string object
    set_str_hash(strObj, iir.icast(IRType.i32, hashCode));


    /*
    // Copy the character data into the string object
    for (var index = pint(0); index < strLen; index = index + pint(1))
    {
        // Get the current character
        var ch = iir.load(IRType.u16, strData, index);

        // Copy the character into the string object
        set_str_data(strObj, index, ch);
    }
    */






    // TODO

    //
    // Hash table updating
    //

    // Set the corresponding key and value in the slot
    set_strtbl_tbl(strtbl, hashIndex, strObj);

    // Get the number of strings and increment it
    var numStrings = get_strtbl_numstrs(strtbl);
    numStrings += i32(1);
    set_strtbl_numstrs(strtbl, numStrings);
    numStrings = iir.icast(IRType.pint, numStrings);
    


    // TODO: hash table extension







    // Return a reference to the string object
    return strObj;
}

