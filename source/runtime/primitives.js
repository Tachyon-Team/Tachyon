/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

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
Throw an exception with a given constructor
*/
function throwError(errorCtor, message)
{
    "tachyon:static";

    throw new errorCtor(message);
}

// TODO: implement the following primitives
function make_clos() {}
function put_clos() {}
function get_clos() {}
function make_arg_obj() {}
function div() {}
function mod() {}
function neq() {}

/**
Implementation of the HIR tobool instruction
*/
function tobool(boxVal)
{
    "tachyon:ret i8";

    // TODO
    return iir.constant(IRType.i8, 0);
}

/**
Implementation of HIR less-than instruction
*/
function lt(v1, v2)
{
    //"tachyon:inline";

    // TODO
}

/**
Implementation of HIR eq instruction
*/
function eq(v1, v2)
{
    //"tachyon:inline";

    // TODO
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
Compute a hash value for a property name or index
*/
function computeHash(propName)
{
    "tachyon:inline";
    "tachyon:ret i32";

    // TODO
    return iir.constant(IRType.i32, 0);
}

/**
Implementation of the HIR put_prop_val instruction
*/
function put_prop_val(obj, propName)
{
    // TODO
}

/**
Implementation of the HIR get_prop_val instruction
*/
function get_prop_val(obj, propName)
{
    // TODO: throw error if not object

    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = iir.icast(
        IRType.pint, 
        computeHash(propName)
    );

    // Until we reach the end of the prototype chain
    do
    {
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

            // If this is the key we want
            if (keyVal === propName)
            {
                // Load the property value
                var propVal = load(
                    IRType.box, 
                    tblPointer, 
                    hashIndex * OBJ_HASH_ENTRY_SIZE + OBJ_HASH_KEY_SIZE
                );

                if (isGetterSetter(propVal))
                    return callGetter(obj, propVal);
                else 
                    return propVal;
            }

            // Otherwise, if we have reached an empty slot
            else if (keyVal === OBJ_HASH_EMPTY_KEY)
            {
                break;
            }

            // Move to the next hash table slot
            hashIndex = (hashIndex + OBJ_HASH_ENTRY_SIZE) % tblSize;
        }

        // Move up in the prototype chain
        var obj = iir.load(IRType.box, obj, OBJ_PROTO_PTR_OFFSET);

    } while (obj != null);

    // Property not found
    return undefined;
}

