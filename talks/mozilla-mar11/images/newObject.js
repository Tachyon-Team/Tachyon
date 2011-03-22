function newObject(proto)
{
    "tachyon:static";
    "tachyon:noglobal";

    assert (
        proto === null || boolToBox(boxIsObjExt(proto)),
        'invalid object prototype'
    );

    // Allocate space for an object
    var obj = alloc_obj();

    // Initialize the prototype object
    set_obj_proto(obj, proto);

    // Initialize the number of properties
    set_obj_numprops(obj, u32(0));

    // Allocate space for a hash table and set the hash table reference
    var hashtbl = alloc_hashtbl(HASH_MAP_INIT_SIZE);
    set_obj_tbl(obj, hashtbl);

    // Return the object reference
    return obj;
}
