/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

// Make special functions for common utility code:
// TODO: BoxIsInt
// TODO: BoxIsDouble
// TODO: BoxIsString
// TODO: isGetterSetter?

/**
Test if a boxed value is an integer
*/
function BoxIsInt(boxVal)
{
    "tachyon:inline";
    "tachyon:ret i8";

    // TODO: boxVal AND 00000...111 == tag?

    // TODO: allow bitwise arithmetic directly between boxed values and pint
    // - Get rid of raw unbox to pint
    // - Possibly, get rid of raw unbox completely

    return iir.constant(IRType.i8, 0);
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
function eq() {}
function neq() {}

/**
Handler function for the HIR add instruction
*/
function add(v1, v2)
{
    // TODO: implement
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
Handler for the HIR put_prop_val instruction
*/
function put_prop_val(obj, propName)
{
    // TODO
}

/**
Handler for the HIR get_prop_val instruction
*/
function get_prop_val(obj, propName)
{
    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

    // Until we reach the end of the prototype chain
    do
    {
        // Get a pointer to the object
        var objPtr = iir.unbox(IRType.optr, obj);

        // Get a pointer to the hash table
        var tblPtr = iir.load(IRType.optr, objPtr, OBJ_HASH_PTR_OFFSET);

        // Get the size of the hash table
        var tblSize = iir.load(IRType.i32, objPtr, OBJ_HASH_SIZE_OFFSET);

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
        var obj = iir.load(IRType.box, objPtr, OBJ_PROTO_PTR_OFFSET);

    } while (obj != null);

    // Property not found
    return undefined;
}

