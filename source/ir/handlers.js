/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/



// TODO: toboolean/boolval instr/handler?
// - Boxed input, bool output
// - Not all handlers have untyped in/out...


// TODO
function BoxIsInt(v)
{
}


// TODO
function BoxIsDouble(v)
{
}


// TODO: BoxIsString



/**
Handler function for the HIR add instruction
*/
/*
function AddHandler(v1, v2)
{
    "tachyon:handler";

}
*/

/**
Handler for the HIR get_prop_val instruction
*/
function GetPropValHandler(obj, propName)
{
    "tachyon:handler";    

    /*
    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = computeHash(propName);

    // Until we reach the end of the prototype chain
    while (obj != null)
    {
        // Get a pointer to the object
        var objPtr = iir.unbox(IRType.OBJPTR, obj);

        // Get a pointer to the hash table
        var tblPtr = load(objPtr, OBJ_HASH_PTR_OFFSET);

        // Get the size of the hash table
        var tblSize = load(objPtr, OBJ_HASH_SIZE_OFFSET);

        // Get the hash table index for this hash value
        var hashIndex = propHash % tblSize;

        // Until the key is found, or a free slot is encountered
        while (true)
        {
            // Get the key value at this hash slot
            var keyVal = load(tblPtr, hashIndex * OBJ_HASH_ENTRY_SIZE);

            // If this is the key we want
            if (keyVal === propName)
            {
                // Load the property value
                var propVal = load(tblPointer, hashIndex * OBJ_HASH_ENTRY_SIZE + OBJ_HASH_KEY_SIZE);

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
        var obj = load(objPtr, OBJ_PROTO_PTR_OFFSET);
    }

    // Property not found
    return undefined;
    */


    var v1 = iir.unbox(IRType.PLATFORM_INT, obj);
    var v2 = iir.unbox(IRType.PLATFORM_INT, propName);
    var v3 = v1 + v2;


    /*
    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = iir.unbox(IRType.PLATFORM_INT, computeHash(propName));

    // Until we reach the end of the prototype chain
    while (obj != null)
    {
        // Get a pointer to the object
        var objPtr = iir.unbox(IRType.OBJPTR, obj);

        // Get a pointer to the hash table
        var tblPtr = iir.load(IRType.OBJPTR, objPtr, OBJ_HASH_PTR_OFFSET);

        // Get the size of the hash table
        var tblSize = iir.load(IRType.PLATFORM_INT, objPtr, OBJ_HASH_SIZE_OFFSET);

        // Get the hash table index for this hash value
        var hashIndex = iir.imod(propHash, tblSize);

        // Until the key is found, or a free slot is encountered
        while (true)
        {
            // Get the key value at this hash slot
            //var keyVal = iir.obj_load(IRType.BOXED, tblPtr, iir.imul(hashIndex, OBJ_HASH_ENTRY_SIZE));

            
            // If this is the key we want
            if (keyVal === propName)
            {
                // Load the property value
                var propVal = load(tblPointer, hashIndex * OBJ_HASH_ENTRY_SIZE + OBJ_HASH_KEY_SIZE);

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
        var obj = iir.load(IRType.BOXED, objPtr, OBJ_PROTO_PTR_OFFSET);
    }
    */

    // Property not found
    return undefined;
}

