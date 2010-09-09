/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Get the tag of a boxed value
*/
function getBoxTag(boxVal)
{
    "tachyon:inline";
    "tachyon:ret pint";

    // Mask the tag
    return boxVal & BOX_TAG_MASK;
}

/**
Test if a boxed value has a specific type
*/
function boxHasTag(boxVal, tagVal)
{
    "tachyon:inline";
    "tachyon:arg tagVal pint";
    "tachyon:ret i8";

    // Compare the tag
    return getBoxTag(boxVal) == tagVal;
}

/**
Test if a boxed value is integer
*/
function boxIsInt(boxVal)
{
    "tachyon:inline";
    "tachyon:ret i8";

    // Test if the value has the int tag
    return boxHasTag(boxVal, BOX_TAG_INT);
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
function get_global() {}
function make_arg_obj() {}
function sub() {}
function mul() {}
function div() {}
function mod() {}
function neq() {}

/**
Implementation of HIR eq instruction
*/
function eq(v1, v2)
{
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
        // TODO: overflow handling: need to create FP objects

        var i1 = iir.unbox(IRType.pint, v1);
        var i2 = iir.unbox(IRType.pint, v2);

        var r = i1 + i2;

        return iir.box(IRType.pint, r);
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
        // TODO: overflow handling: need to create FP objects

        var i1 = iir.unbox(IRType.pint, v1);
        var i2 = iir.unbox(IRType.pint, v2);

        var r = i1 - i2;

        return iir.box(IRType.pint, r);
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
    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

    // Until we reach the end of the prototype chain
    do
    {
        // Get a pointer to the hash table
        var tblPtr = iir.load(IRType.box, obj, OBJ_HASH_PTR_OFFSET);

        // Get the size of the hash table
        var tblSize = iir.load(IRType.i32, obj, OBJ_HASH_SIZE_OFFSET);

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

