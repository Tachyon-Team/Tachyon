/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

//=============================================================================
//
// Primitive functions to operate on boxed values
//
//=============================================================================

/**
Box an integer value
*/
function boxInt(intVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:arg intVal pint";

    // Box the integer
    return iir.icast(IRType.box, intVal << TAG_NUM_BITS_INT);
}

/**
Unbox an integer value
*/
function unboxInt(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret pint";

    // Box the integer
    return iir.icast(IRType.pint, boxVal) >> TAG_NUM_BITS_INT;
}

/**
Box a reference value
*/
function boxRef(rawPtr, tagVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:arg rawPtr rptr";
    "tachyon:arg tagVal pint";

    // Box the raw pointer
    return iir.icast(IRType.box, (rawPtr & ~TAG_REF_MASK) | tagVal);
}

/**
Get the reference tag of a boxed value
*/
function getRefTag(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret pint";

    // Mask out the non-tag part
    return boxVal & TAG_REF_MASK;
}

/**
Test if a boxed value has a specific reference tag
*/
function boxHasTag(boxVal, tagVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:arg tagVal pint";
    "tachyon:ret bool";

    // Compare the reference tag
    return getRefTag(boxVal) == tagVal;
}

/**
Test if a boxed value is integer
*/
function boxIsInt(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret bool";

    // Test if the value has the int tag
    return (boxVal & TAG_INT_MASK) == TAG_INT;
}

/**
Test if a boxed value is an object
*/
function boxIsObj(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret bool";

    // Compare the reference tag
    return getRefTag(boxVal) >= TAG_ARRAY;
}

/**
Test if a boxed value is a function
*/
function boxIsFunc(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret bool";

    /* TODO
    // Compare the reference tag
    return getRefTag(boxVal) == TAG_FUNCTION;
    */

    // FIXME: for now, function pointers not boxed, this will not work
    return iir.constant(IRType.bool, 1);
}

/**
Test if a boxed value is an array
*/
function boxIsArray(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret bool";

    // Compare the reference tag
    return getRefTag(boxVal) == TAG_ARRAY;
}

/**
Test if a boxed value is a floating-point value
*/
function boxIsFloat(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret bool";

    // Compare the reference tag
    return getRefTag(boxVal) == TAG_FLOAT;
}

/**
Test if a boxed value is a string
*/
function boxIsString(boxVal)
{
    "tachyon:inline";
    "tachyon:nothrow";
    "tachyon:ret bool";

    // Compare the reference tag
    return getRefTag(boxVal) == TAG_STRING;
}

/**
Convert a boxed value to a one-byte boolean value
*/
function boxToBool(boxVal)
{
    "tachyon:static";
    "tachyon:nothrow";
    "tachyon:ret bool";

    // Get an integer-typed value for input
    var boxInt = iir.icast(IRType.pint, boxVal);

    if (boxInt == BIT_PATTERN_TRUE)
        return iir.constant(IRType.bool, 1);

    else if (boxInt == BIT_PATTERN_FALSE)
        return iir.constant(IRType.bool, 0);

    else if (boxInt == BIT_PATTERN_UNDEF)
        return iir.constant(IRType.bool, 0);

    else if (boxInt == BIT_PATTERN_NULL)
        return iir.constant(IRType.bool, 0);

    else if (boxIsInt(boxVal))
    { 
        if (boxInt != iir.constant(IRType.pint, 0))
            return iir.constant(IRType.bool, 1);
        else
            return iir.constant(IRType.bool, 0);
    }

    else if (boxIsString(boxVal))
    {
        var len = iir.icast(IRType.pint, get_str_len(boxVal));

        if (len != iir.constant(IRType.pint, 0))
            return iir.constant(IRType.bool, 1);
        else
            return iir.constant(IRType.bool, 0);
    }

    return iir.constant(IRType.bool, 1);
}

//=============================================================================
//
// Utility functions
//
//=============================================================================

/**
Allocate a memory block of a given size on the heap
*/
function heapAlloc(size)
{
    "tachyon:static";
    "tachyon:arg size pint";
    "tachyon:ret rptr";

    // Get a pointer to the context
    var ctx = iir.get_ctx();

    // Get the current allocation pointer
    var allocPtr = get_ctx_allocptr();

    // Increment the allocation pointer by the object size
    var nextPtr = allocPtr + size;

    // Align the next allocation pointer
    var rem = iir.icast(IRType.pint, nextPtr) % HEAP_ALIGN;
    if (rem != iir.constant(IRType.pint, 0))
    {
        var pad = HEAP_ALIGN - rem;
        nextPtr += pad;
    }
    
    // Update the allocation pointer in the context object
    set_ctx_allocptr(nextPtr);

    // Allocate the object at the current position
    return allocPtr;
}

/**
Create an exception with a given constructor
*/
function makeError(errorCtor, message)
{
    "tachyon:static";
    "tachyon:nothrow";

    //return new errorCtor(message);
    return UNDEFINED;
}

//=============================================================================
//
// Implementation of JavaScript primitives (IR instructions)
//
//=============================================================================

// TODO: implement the following primitives
function typeOf(obj) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function instanceOf(obj, ctor) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function delPropVal(obj, propName) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function getPropNames(obj) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function makeClos(funcObj) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function putClos(clos, idx, val) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function getClos(clos, idx) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function makeCell() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function putCell(cell, val) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function getCell(cell) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function makeArgObj(funcObj) { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function newArray() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function div() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function mod() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function not() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function and() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function or() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function xor() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function lsft() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function rsft() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function ursft() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function ne() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }
function nseq() { "tachyon:static"; "tachyon:nothrow"; return UNDEFINED; }

/**
Create a new object with no properties
*/
function newObject(proto)
{
    "tachyon:static";
    "tachyon:nothrow";

    // Allocate space for an object
    var obj = boxRef(alloc_obj(), TAG_OBJECT);

    // Initialize the prototype object
    set_obj_proto(obj, proto);

    // Initialize the hash table size and number of properties
    set_obj_tblsize(obj, HASH_MAP_INIT_SIZE);
    set_obj_numprops(obj, iir.constant(IRType.i32, 0));

    // Initialize the hash table pointer to null to prevent GC errors
    set_obj_tbl(obj, null);

    // Allocate space for a hash table and set the hash table reference
    var hashtbl = boxRef(alloc_hashtbl(HASH_MAP_INIT_SIZE), TAG_OTHER);
    set_obj_tbl(obj, hashtbl);

    // Initialize the hash table
    for (
        var i = iir.constant(IRType.pint, 0); 
        i < HASH_MAP_INIT_SIZE; 
        i += iir.constant(IRType.pint, 1)
    )
    {
        set_hashtbl_tbl_key(hashtbl, i, UNDEFINED);
    }

    // Return the object reference
    return obj;
}

/**
Implementation of HIR less-than instruction
*/
function lt(v1, v2)
{
    "tachyon:inline";
    "tachyon:nothrow";

    return iir.lt(v1, v2)? true:false;

    /*
    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Compare the immediate integers directly without unboxing them
        return iir.lt(v1, v2)? true:false;
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
        return UNDEFINED;
    }
    */
}

/**
Implementation of HIR eq instruction
*/
function eq(v1, v2)
{
    "tachyon:inline";
    "tachyon:nothrow";

    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Compare the immediate integers directly without unboxing them
        return iir.eq(v1, v2)? true:false;
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
        return UNDEFINED;
    }
}

/**
Implementation of HIR strict-equality instruction
*/
function seq(v1, v2)
{
    "tachyon:inline";
    "tachyon:nothrow";

    // If both values are floating-point
    if (boxHasTag(v1, TAG_FLOAT) && boxHasTag(v2, TAG_FLOAT))
    {
        // TODO: implement FP case in separate(non-inlined) function
        return UNDEFINED;
    }
    else
    {
        // Compare the boxed value directly without unboxing them
        // This will compare for equality of reference in the case of
        // references and compare immediate integers directly
        return iir.eq(v1, v2)? true:false;
    }
}

/**
Implementation of the HIR add instruction
*/
function add(v1, v2)
{
    "tachyon:inline";
    "tachyon:nothrow";

    
    var intResult;
    if (intResult = iir.add_ovf(v1, v2))
    {
        return intResult;
    } else
    {
        return UNDEFINED;
    }
    

    // TODO: ensure that boxIsInt is valid

    /*
    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Attempt an add with overflow check
        var intResult;
        if (intResult = iir.add_ovf(v1, v2))
        {
            // If there is no overflow, return the result
            // No normalization necessary
            return intResult;
        }
        else
        {
            // TODO: overflow handling: need to create FP objects
            return UNDEFINED;
        }
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
        return UNDEFINED;
    }
    */
}

/**
Implementation of the HIR sub instruction
*/
function sub(v1, v2)
{
    "tachyon:inline";
    "tachyon:nothrow";

    // Attempt a subtract with overflow check
    var intResult;
    if (intResult = iir.sub_ovf(v1, v2))
    {
        // If there is no overflow, return the result
        // No normalization necessary
        return intResult;
    }
    else
    {
        // TODO: overflow handling: need to create FP objects
        return UNDEFINED;
    }    

    /*
    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Attempt a subtract with overflow check
        var intResult;
        if (intResult = iir.sub_ovf(v1, v2))
        {
            // If there is no overflow, return the result
            // No normalization necessary
            return intResult;
        }
        else
        {
            // TODO: overflow handling: need to create FP objects
            return UNDEFINED;
        }    
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
        return UNDEFINED;
    }*/
}

/**
Implementation of the HIR mul instruction
*/
function mul(v1, v2)
{
    "tachyon:inline";
    "tachyon:nothrow";

    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Attempt a multiply with overflow check
        var intResult;
        if (intResult = iir.mul_ovf(v1, v2))
        {
            // If there is no overflow, return the result
            // Normalize by shifting right by the number of integer tag bits
            return iir.icast(IRType.box, intResult >> TAG_NUM_BITS_INT);
        }
        else
        {
            // TODO: overflow handling: need to create FP objects
            return UNDEFINED;
        }    
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
        return UNDEFINED;
    }
}

/**
Compute a hash value for a given string or index
*/
function computeHash(key)
{
    "tachyon:inline";
    "tachyon:ret pint";

    // TODO: assert int or string
    // toPrimitive...

    // If the property is integer
    if (boxIsInt(key))
    {
        // Unbox the key
        return unboxInt(key);
    }

    // Otherwise, the key is a string
    else
    {
        var hashCode = iir.constant(IRType.pint, 0);

        // Read the string length
        var strLen = get_str_len(key);

        // For each character, update the hash code
        for (
            var i = iir.constant(IRType.pint, 0); 
            i < strLen; 
            i = i + iir.constant(IRType.pint, 1)
        )
        {
            var ch = iir.icast(
                IRType.pint,
                get_str_data(key, i)
            );

            hashCode =
                (hashCode * iir.constant(IRType.pint, 256) + ch) %
                iir.constant(IRType.pint, 426870919);
        }

        // Return the computed hash code
        return hashCode;
    }
}

/**
Set a property on an object
*/
function putPropVal(obj, propName, propVal)
{
    "tachyon:static";

    // TODO: assert object is passed? toObject?

    // TODO: find if getter-setter exists?
    // Requires first looking up the entry in the whole prototype chain...

    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

    // Get a pointer to the hash table
    var tblPtr = get_obj_tbl(obj);

    // Get the size of the hash table
    var tblSize = iir.icast(
        IRType.pint,
        get_obj_tblsize(obj)
    );

    // Get the hash table index for this hash value
    var hashIndex = propHash % tblSize;

    // Until the key is found, or a free slot is encountered
    while (true)
    {
        // Get the key value at this hash slot
        var keyVal = get_hashtbl_tbl_key(tblPtr, hashIndex);

        // If this is the key we want
        if (keyVal === propName)
        {
            // Set the corresponding property value
            set_hashtbl_tbl_val(tblPtr, hashIndex, propVal);

            // Break out of the loop
            break;
        }

        // Otherwise, if we have reached an empty slot
        else if (keyVal === UNDEFINED)
        {
            // Set the corresponding property value
            set_hashtbl_tbl_val(tblPtr, hashIndex, propVal);

            // Get the number of properties and increment it
            var numProps = get_obj_numprops(obj);
            numProps += iir.constant(IRType.i32, 1);
            set_obj_numprops(obj, numProps);
            numProps = iir.icast(IRType.pint, numProps);

            // Test if resizing of the hash map is needed
            // numProps > ratio * tblSize
            // numProps > num/denum * tblSize
            // numProps / num > tblSize / denum
            if (numProps / HASH_MAP_MAX_LOAD_NUM >
                tblSize / HASH_MAP_MAX_LOAD_DENUM)
            {
                // TODO: realloc hash table, rehash all properties
            }

            // Break out of the loop
            break;
        }

        // Move to the next hash table slot
        hashIndex = (hashIndex + iir.constant(IRType.pint, 1)) % tblSize;
    }
}

/**
Get a property from an object
*/
function getProp(obj, propName, propHash)
{
    "tachyon:inline";
    "tachyon:arg propHash pint";

    // Until we reach the end of the prototype chain
    do
    {
        // Get a pointer to the hash table
        var tblPtr = get_obj_tbl(obj);

        // Get the size of the hash table
        var tblSize = iir.icast(
            IRType.pint,
            get_obj_tblsize(obj)
        );

        // Get the hash table index for this hash value
        var hashIndex = propHash % tblSize;

        // Until the key is found, or a free slot is encountered
        while (true)
        {
            // Get the key value at this hash slot
            var keyVal = get_hashtbl_tbl_key(tblPtr, hashIndex);

            // If this is the key we want
            if (keyVal === propName)
            {
                // Load the property value
                var propVal = get_hashtbl_tbl_val(tblPtr, hashIndex);

                /*
                if (isGetterSetter(propVal))
                    return callGetter(obj, propVal);
                else 
                    return propVal;
                */

                // TODO
                return propVal;
            }

            // Otherwise, if we have reached an empty slot
            else if (keyVal === UNDEFINED)
            {
                break;
            }

            // Move to the next hash table slot
            hashIndex = (hashIndex + iir.constant(IRType.pint, 1)) % tblSize;
        }

        // Move up in the prototype chain
        var obj = get_obj_proto(obj);

    } while (obj != null);

    // Property not found, return a special bit pattern
    return iir.icast(IRType.box, BIT_PATTERN_NOT_FOUND);
}

/**
Get a property from an object
*/
function hasPropVal(obj, propName)
{
    "tachyon:static";
    "tachyon:ret bool";

    // TODO: throw error if not object
    // - Maybe not, should never happen in practice... toObject
    // - What we actually want is a debug assertion

    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

    // Attempt to find the property on the object
    var prop = getProp(obj, propName, propHash);

    // Test if the property was found
    return (iir.icast(IRType.pint, prop) != BIT_PATTERN_NOT_FOUND);
}

/**
Get a property from an object
*/
function getPropVal(obj, propName)
{
    "tachyon:static";

    // TODO: throw error if not object
    // - Maybe not, should never happen in practice... toObject
    // - What we actually want is a debug assertion

    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

    // Attempt to find the property on the object
    var prop = getProp(obj, propName, propHash);

    // If the property isn't defined
    if (iir.icast(IRType.pint, prop) == BIT_PATTERN_NOT_FOUND)
    {
        // Return the undefined value
        return UNDEFINED;
    }
}

/**
Get a property value from the global object
*/
function getGlobal(obj, propName, propHash)
{
    "tachyon:static";
    "tachyon:arg propHash pint";

    // Attempt to find the property on the object
    var prop = getProp(obj, propName, propHash);

    // If the property isn't defined
    if (iir.icast(IRType.pint, prop) == BIT_PATTERN_NOT_FOUND)
    {
        // Throw a ReferenceError exception
        throw makeError(ReferenceError, "global property not defined" + propName);
    }

    // Return the property
    return prop;
}

/**
Get a function property from the global object
*/
function getGlobalFunc(obj, propName, propHash)
{
    "tachyon:static";
    "tachyon:arg propHash pint";

    // Attempt to find the property on the object
    var prop = getProp(obj, propName, propHash);

    // If the property is a function
    if (boxIsFunc(prop))
    {
        // Return the function property
        return prop;
    }
    else
    {
        // If the property isn't defined
        if (iir.icast(IRType.pint, prop) == BIT_PATTERN_NOT_FOUND)
        {
            // Throw a ReferenceError exception
            throw makeError(ReferenceError, "global property not defined" + propName);
        }
        else
        {
            // Throw a TypeError exception
            throw makeError(TypeError, "global property is not a function" + propName);
        }
    }
}

