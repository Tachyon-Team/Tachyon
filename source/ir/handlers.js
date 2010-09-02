/**
@fileOverview
Implementation of high-level IR instructions through handler functions

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/


// TODO: handler input/output type annotation
// - Not all handlers have untyped in/out...

/*
// Make special instructions and handlers for common utility code:
// TODO: BoxIsInt
// TODO: BoxIsDouble
// TODO: BoxIsString
// TODO: isGetterSetter?

Perhaps a better option would be to enable direct "cross-linking" of
handler code. Enable handlers to call each other without defining IR
instructions?
- Requires special "cross-linking" pass
- Could be special flag to lowering pass
  - Replace global function calls by other handler calls, if available?
  - Potential problem, if coding GC code, not technically a handler?


Probably want all tachyon code to cross-link together?
- Problem: some functions will have non-boxed return types
  - This info is needed for cross-linking
  - Need to parse/translate the functions to know this


*/


// TODO: implement handlers
function make_clos() {}
function put_clos() {}
function get_clos() {}
function get_global() {}
function make_arg_obj() {}
function get_prop_val() {}
function put_prop_val() {}

function sub() {}
function neq() {}

// TODO: scrap call_handler

// TODO: rename to primitives.js

// TODO: auto static link functions with non-boxed in/out

// TODO: unify constants with compile-time-binding-resolution?
// - Compiler constants are the same for everybody, leave as now
// - Constants such as object prototype should be in "execution context"
//   - Context access instruction, get_ctx_val "", set_ctx_val ""
//   - No need to create before compilation?

/**
Annotations:

static
inline
arg types
return type

*/


/**
Handler function for the HIR add instruction
*/
function add(v1, v2)
{
    "tachyon:handler";


    // TODO: implement also

}

/**
Handler for the HIR get_prop_val instruction
*/
function get_prop_val(obj, propName)
{
    "tachyon:handler";

    // Compute the hash for the property
    // Boxed value, may be a string or an int
    var propHash = iir.unbox(IRType.INT32, computeHash(propName));

    // Until we reach the end of the prototype chain
    while (obj != null)
    {
        // Get a pointer to the object
        var objPtr = iir.unbox(IRType.OBJPTR, obj);

        // Get a pointer to the hash table
        var tblPtr = iir.load(IRType.OBJPTR, objPtr, OBJ_HASH_PTR_OFFSET);

        // Get the size of the hash table
        var tblSize = iir.load(IRType.INT32, objPtr, OBJ_HASH_SIZE_OFFSET);

        // Get the hash table index for this hash value
        var hashIndex = propHash % tblSize;

        // Until the key is found, or a free slot is encountered
        while (true)
        {
            // Get the key value at this hash slot
            var keyVal = iir.load(
                IRType.BOXED,
                tblPtr,
                hashIndex * OBJ_HASH_ENTRY_SIZE
            );

            // If this is the key we want
            if (keyVal === propName)
            {
                // Load the property value
                var propVal = load(
                    IRType.BOXED, 
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
        var obj = iir.load(IRType.BOXED, objPtr, OBJ_PROTO_PTR_OFFSET);
    }    

    // Property not found
    return undefined;
}

