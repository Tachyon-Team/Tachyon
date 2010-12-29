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

    // Initialize the string table size and number of properties
    set_strtbl_tblsize(strtbl, STR_TBL_INIT_SIZE);
    set_strtbl_numstrs(strtbl, i32(0));

    // Initialize the string table entries
    for (var i = pint(0); i < STR_TBL_INIT_SIZE; i += pint(1))
        set_strtbl_tbl(strtbl, i, UNDEFINED);

    // Get a pointer to the context
    var ctx = iir.get_ctx();

    // Set the string table reference in the context
    set_ctx_strtbl(ctx, strtbl);
}

/**
Compare two raw UTF-16 strings by iterating over 16 bit code units
This conforms to section 11.8.5 of the ECMAScript 262 specification
NOTE: this is used to find strings in the hash consing table
*/
function streq(strObj, rawStr)
{
    "tachyon:arg rawStr rptr";
    "tachyon:ret bool";

    // For each character to be compared
    for (var i = pint(0);; i += pint(1))
    {
        var ch1 = get_str_data(strObj, i);

        var ch2 = iir.load(IRType.u16, rawStr, pint(2) * i);

        if (ch1 != ch2)
            return FALSE_BOOL;
        
        if (ch1 == u16(0))
            break;
    }

    // The strings are equal
    return TRUE_BOOL;
}

/**
Allocate/get a reference to a string object containing the given string data
@param rawStr pointer to raw UTF-16 string data
*/
function getStrObj(rawStr, strLen)
{
    "tachyon:static";
    "tachyon:arg rawStr rptr";
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
        var ch = iir.load(IRType.u16, rawStr, index);

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
        if (streq(strVal, rawStr))
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

    // FIXME: register allocation bug prevents memory-memory copy from working
    /*    
    // Copy the character data into the string object
    for (var index = pint(0); index < strLen; index = index + pint(1))
    {
        // Get the current character
        var ch = iir.load(IRType.u16, rawStr, index);

        // Copy the character into the string object
        set_str_data(strObj, index, ch);
    }
    */

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

    // Test if resizing of the string table is needed
    // numStrings > ratio * tblSize
    // numStrings > num/denom * tblSize
    // numStrings * denom > tblSize * num
    if (numStrings * STR_TBL_MAX_LOAD_DENOM >
        tblSize * STR_TBL_MAX_LOAD_NUM)
    {
        // Extend the string table
        extStrTable(strtbl, tblSize, numStrings);
    }

    // Return a reference to the string object
    return strObj;
}

/**
Extend the string table and rehash its contents
*/
function extStrTable(curTbl, curSize, numStrings)
{
    "tachyon:inline";
    "tachyon:arg curSize pint";
    "tachyon:arg numStrings pint";

    // Compute the new table size
    var newSize = curSize * pint(2) + pint(1);

    // Allocate a new, larger hash table
    var newTbl = alloc_strtbl(newSize);

    // Set the new size and the number of strings stored
    set_strtbl_tblsize(newTbl, newSize);
    set_strtbl_numstrs(newTbl, numStrings);

    // For each entry in the current table
    for (var curIdx = pint(0); 
         curIdx < curSize; 
         curIdx = (curIdx + pint(1)) % curSize
    )
    {
        // Get the value at this hash slot
        var slotVal = get_strtbl_tbl(curTbl, curIdx);

        // Get the hash code for the value
        // Boxed value, may be a string or an int
        var valHash = getHash(slotVal);

        // Get the hash table index for this hash value in the new table
        var startHashIndex = valHash % newSize;
        var hashIndex = startHashIndex;

        // Until a free slot is encountered
        while (true)
        {
            // Get the value at this hash slot
            var slotVal2 = get_strtbl_tbl(newTbl, hashIndex);

            // If we have reached an empty slot
            if (slotVal2 === UNDEFINED)
            {
                // Set the corresponding key and value in the slot
                set_strtbl_tbl(newTbl, hashIndex, slotVal);

                // Break out of the loop
                break;
            }

            // Move to the next hash table slot
            hashIndex = (hashIndex + pint(1)) % newSize;

            // Ensure that a free slot was found for this key
            assert (
                hashIndex != startHashIndex,
                'no free slots found in extended hash table'
            );
        }
    }

    // Get a pointer to the context
    var ctx = iir.get_ctx();

    // Update the string table reference in the context
    set_ctx_strtbl(ctx, newTbl);
}

