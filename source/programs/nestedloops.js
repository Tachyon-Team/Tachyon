/**
Get a property from an object
*/
function getPropDummy(obj, propName, propHash)
{
    "tachyon:arg propHash pint";

    // Until we reach the end of the prototype chain
    while (true)
    {
        // Until the key is found, or a free slot is encountered
        while (obj)
        {
            // If this is the key we want
            if (obj === propName)
            {
            }
        }

        // Move up in the prototype chain
        var obj = get_obj_proto(obj);
    }
}

