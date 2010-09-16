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

// TODO: possible trick, OR of both values, test if zero int tag
// Test if two values are boxed integers at once

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

    // If the value is false, undefined, null or 0
    if (boxInt == BIT_PATTERN_FALSE ||
        boxInt == BIT_PATTERN_UNDEF ||
        boxInt == BIT_PATTERN_NULL ||
        boxInt == iir.constant(IRType.pint, 0)
    )
    {
        // Return a false boolean
        return iir.constant(IRType.i8, 0);
    }

    // If the value is true or a nonzero integer
    else if (
        boxInt == BIT_PATTERN_TRUE ||
        boxIsInt(boxVal)
    )
    {
        // Return a true boolean
        return iir.constant(IRType.i8, 1);
    }

    // TODO: handle other cases
    return iir.constant(IRType.i8, 0);
}

//=============================================================================
//
// Miscellaneous utility functions
//
//=============================================================================

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
function make_clos() {}
function put_clos() {}
function get_clos() {}
function make_arg_obj() {}
function div() {}
function mod() {}
function neq() {}

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
    //
    // TODO
    //

    // TODO: assert object

    // TODO: find if getter-setter exists?


    /*

    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

    // Get a pointer to the hash table
    var tblPtr = iir.load(IRType.box, obj, OBJ_HASH_PTR_OFFSET);

    // Get the size of the hash table
    var tblSize = iir.icast(
        IRType.pint,
        iir.load(
            IRType.i32,
            obj, 
            OBJ_HASH_SIZE_OFFSET
        )
    );

    // Get the hash table index for this hash value
    var hashIndex = propHash % tblSize;

    // Until the key is found, or a free slot is encountered
    while (true)
    {
        // Get the key value at this hash slot
        var keyVal = iir.load(
            IRType.box,
            tblPtr,
            hashIndex * OBJ_HASH_ENTRY_SIZE
        );

        // If this is the key we want or an empty slot
        if (keyVal === propName)
        {

            // TODO: Set prop val
            
            // Load the property value
            var propVal = load(
                    IRType.box, 
                    tblPointer, 
                    hashIndex * OBJ_HASH_ENTRY_SIZE + OBJ_HASH_KEY_SIZE
            );
            

            break;
        }

        // Otherwise, if we have reached an empty slot
        else if (keyVal === OBJ_HASH_EMPTY_KEY)
        {
            // TODO: set prop val

            // TODO: increment item count

            // TODO: test if resizing is needed


            break;
        }

        // Move to the next hash table slot
        hashIndex = (hashIndex + OBJ_HASH_ENTRY_SIZE) % tblSize;
    }
    */
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

