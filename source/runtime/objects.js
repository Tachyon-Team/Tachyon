/**
@fileOverview
Implementation of the memory representation of objects.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

//
// TODO: define constants for object tags
// - platform int constants
//
staticEnv.regBinding(
    'BOX_TAG_MASK',
    ConstValue.getConst(
        7,
        IRType.pint
    )
);
staticEnv.regBinding(
    'BOX_TAG_INT',
    ConstValue.getConst(
        0,
        IRType.pint
    )
);

// Object representation constants
staticEnv.regBinding(
    'OBJ_PROTO_PTR_OFFSET',
    ConstValue.getConst(
        4,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_PTR_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_PROTO_PTR_OFFSET').value + IRType.box.size,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_SIZE_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_HASH_PTR_OFFSET').value + IRType.box.size,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_ENTRY_SIZE',
    ConstValue.getConst(
        16,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_KEY_SIZE',
    ConstValue.getConst(
        8,
        IRType.i32
    )
);
staticEnv.regBinding(
    'OBJ_HASH_EMPTY_KEY',
    ConstValue.getConst(
        0,
        IRType.box
    )
);

