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
Get the reference tag of a boxed value
*/
function getRefTag(boxVal)
{
    "tachyon:inline";
    "tachyon:ret pint";

    // Mask the tag
    return boxVal & TAG_REF_MASK;
}

/**
Test if a boxed value has a specific reference tag
*/
function boxHasTag(boxVal, tagVal)
{
    "tachyon:inline";
    "tachyon:arg tagVal pint";
    "tachyon:ret i8";

    // Compare the tag
    return getRefTag(boxVal) == tagVal;
}

/**
Test if a boxed value is integer
*/
function boxIsInt(boxVal)
{
    "tachyon:inline";
    "tachyon:ret i8";

    // Test if the value has the int tag
    return (boxVal & TAG_INT_MASK) == TAG_INT;
}

/**
Test if a boxed value is an object
*/
function boxIsObj(boxVal)
{
    "tachyon:inline";
    "tachyon:ret i8";

    // Compare the tag
    return getRefTag(boxVal) >= TAG_ARRAY;
}

/**
Test if a boxed value is a function
*/
function boxIsFunc(boxVal)
{
    "tachyon:inline";
    "tachyon:ret i8";

    // Compare the tag
    return getRefTag(boxVal) == TAG_FUNCTION;
}

/**
Test if a boxed value is an array
*/
function boxIsArray(boxVal)
{
    "tachyon:inline";
    "tachyon:ret i8";

    // Compare the tag
    return getRefTag(boxVal) == TAG_ARRAY;
}

/**
Convert a boxed value to a one-byte boolean value
*/
function boxToBool(boxVal)
{
    "tachyon:ret i8";

    // Get an integer-typed value for input
    var boxInt = iir.icast(IRType.pint, boxVal);

    if (boxInt == BIT_PATTERN_TRUE)
        return iir.constant(IRType.i8, 1);

    else if (boxInt == BIT_PATTERN_FALSE)
        return iir.constant(IRType.i8, 0);

    else if (boxInt == BIT_PATTERN_UNDEF)
        return iir.constant(IRType.i8, 0);

    else if (boxInt == BIT_PATTERN_NULL)
        return iir.constant(IRType.i8, 0);

    else if (boxIsInt(boxVal))
    { 
        if (boxInt != iir.constant(IRType.pint, 0))
            return iir.constant(IRType.i8, 1);
        else
            return iir.constant(IRType.i8, 0);
    }

    else if (boxIsString(boxVal))
    {
        var len = iir.icast(IRType.pint, str_get_len(boxVal));

        if (len != iir.constant(IRType.pint, 0))
            return iir.constant(IRType.i8, 1);
        else
            return iir.constant(IRType.i8, 0);
    }

    return iir.constant(IRType.i8, 1);
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
Throw an exception with a given constructor
*/
function throwError(errorCtor, message)
{
    "tachyon:static";

    throw new errorCtor(message);
}

//=============================================================================
//
// Implementation of JavaScript primitives (IR instructions)
//
//=============================================================================

// TODO: implement the following primitives
function new_object() {}
function make_clos() {}
function put_clos() {}
function get_clos() {}
function make_arg_obj() {}
function div() {}
function mod() {}
function neq() {}
function not() {}

/**
Implementation of HIR less-than instruction
*/
function lt(v1, v2)
{
    "tachyon:inline";

    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Get integer-typed values for v1 and v2
        // Note that this does not unbox the values
        var i1 = iir.icast(IRType.pint, v1);
        var i2 = iir.icast(IRType.pint, v2);

        // Compare the immediate integers directly
        return (i1 < i2)? true:false;
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
    }
}

/**
Implementation of HIR eq instruction
*/
function eq(v1, v2)
{
    "tachyon:inline";

    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Get integer-typed values for v1 and v2
        // Note that this does not unbox the values
        var i1 = iir.icast(IRType.pint, v1);
        var i2 = iir.icast(IRType.pint, v2);

        // Compare the immediate integers directly
        return (i1 == i2)? true:false;
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
    }
}

/**
Implementation of HIR strict-equality instruction
*/
function seq(v1, v2)
{
    "tachyon:inline"

    // If both values are floating-point
    if (boxHasTag(v1, TAG_FLOAT) && boxHasTag(v2, TAG_FLOAT))
    {
        // TODO: implement FP case in separate(non-inlined) function
    }
    else
    {
        // Get the raw bit-patterns of the boxed values
        var i1 = iir.icast(IRType.pint, v1);
        var i2 = iir.icast(IRType.pint, v2);

        // Compare the raw bit-patterns directly
        return (i1 == i2)? true:false;
    }
}

/**
Implementation of the HIR add instruction
*/
function add(v1, v2)
{
    "tachyon:inline";

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
        }
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
    }
}

/**
Implementation of the HIR sub instruction
*/
function sub(v1, v2)
{
    "tachyon:inline";

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
        }    
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
    }
}

/**
Implementation of the HIR mul instruction
*/
function mul(v1, v2)
{
    "tachyon:inline";

    // If both values are immediate integers
    if (boxIsInt(v1) && boxIsInt(v2))
    {
        // Attempt a multiply with overflow check
        var intResult;
        if (intResult = iir.mul_ovf(v1, v2))
        {
            // If there is no overflow, return the result
            // Normalize by shifting right by the number of integer tag bits
            return iir.box(IRType.pint, intResult >> TAG_NUM_BITS_INT);
        }
        else
        {
            // TODO: overflow handling: need to create FP objects
        }    
    }
    else
    {
        // TODO: implement general case in separate (non-inlined) function
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
        // Cast the key to an integer value
        return iir.icast(IRType.pint, key);
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
                iir.constant(IRType.pint, 426870919)
            ;
        }

        // Return the computed hash code
        return hashCode;
    }
}

/**
Implementation of the HIR put_prop_val instruction
*/
function put_prop_val(obj, propName, propVal)
{
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
        else if (keyVal === OBJ_HASH_EMPTY_KEY)
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
Implementation of the HIR get_prop_val instruction
*/
function get_prop_val(obj, propName)
{
    // TODO: throw error if not object
    // - Maybe not, should never happen in practice... toObject
    // - What we actually want is a debug assertion

    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

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
                return propVal
            }

            // Otherwise, if we have reached an empty slot
            else if (keyVal === OBJ_HASH_EMPTY_KEY)
            {
                break;
            }

            // Move to the next hash table slot
            hashIndex = (hashIndex + iir.constant(IRType.pint, 1)) % tblSize;
        }

        // Move up in the prototype chain
        var obj = get_obj_proto(obj);

    } while (obj != null);

    // Property not found
    return undefined;
}

