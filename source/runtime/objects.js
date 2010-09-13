/**
@fileOverview
Implementation of the memory representation of objects.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

//=============================================================================
//
// Tag bit constants for boxed values
//
// These constants should have type platform-int, matching the box type size
//
// Tags needed:
// - immediate int      X-----X00   0
// - object             X----X111   7
// - function obj       X----X110   6
// - array obj          X----X101   5
// - float              X----X011   3
// - string             X----X010   2
// - other              X----X001   1
//   - constants (true/false/null/undef)
//   - other heap objects (getter-setter, hash table, etc.)
//
// Immediate integer tags have only two tag bits so they can be used directly
// in 32-bit array indexing. The resulting immediate integers have 30 
// effective usable bits.
//
//=============================================================================

// Number of integer tag bits
staticEnv.regBinding(
    'TAG_NUM_BITS_INT',
    ConstValue.getConst(
        2,
        IRType.pint
    )
);

// Number of reference tag bits
staticEnv.regBinding(
    'TAG_NUM_BITS_REF',
    ConstValue.getConst(
        2,
        IRType.pint
    )
);

// Mask used to extract immediate integer tags
staticEnv.regBinding(
    'TAG_INT_MASK',
    ConstValue.getConst(
        3,
        IRType.pint
    )
);

// Mask used to extract reference tags
staticEnv.regBinding(
    'TAG_REF_MASK',
    ConstValue.getConst(
        7,
        IRType.pint
    )
);

// Tag for immediate integers
staticEnv.regBinding(
    'TAG_INT',
    ConstValue.getConst(
        0,
        IRType.pint
    )
);

// Tag for plain objects
staticEnv.regBinding(
    'TAG_OBJECT',
    ConstValue.getConst(
        7,
        IRType.pint
    )
);

// Tag for function objects
staticEnv.regBinding(
    'TAG_FUNCTION',
    ConstValue.getConst(
        6,
        IRType.pint
    )
);

// Tag for array objects
staticEnv.regBinding(
    'TAG_ARRAY',
    ConstValue.getConst(
        5,
        IRType.pint
    )
);

// Tag for floating-point values
staticEnv.regBinding(
    'TAG_FLOAT',
    ConstValue.getConst(
        3,
        IRType.pint
    )
);

// Tag for strings
staticEnv.regBinding(
    'TAG_STRINGS',
    ConstValue.getConst(
        2,
        IRType.pint
    )
);

// Tag for other values
staticEnv.regBinding(
    'TAG_OTHER',
    ConstValue.getConst(
        1,
        IRType.pint
    )
);

//=============================================================================
//
// JavaScript constant values
//
// Each constant has a specific bit-pattern:
// - true       000--0|00|001   1
// - false      000--0|01|001   9
// - null       000--0|10|001   17
// - undefined  000--0|11|001   25
//
//=============================================================================

// Bit pattern for the true constant
staticEnv.regBinding(
    'BIT_PATTERN_TRUE',
    ConstValue.getConst(
        1,
        IRType.pint
    )
);

// Bit pattern for the false constant
staticEnv.regBinding(
    'BIT_PATTERN_FALSE',
    ConstValue.getConst(
        9,
        IRType.pint
    )
);

// Bit pattern for the null constant
staticEnv.regBinding(
    'BIT_PATTERN_NULL',
    ConstValue.getConst(
        17,
        IRType.pint
    )
);

// Bit pattern for the undefined constant
staticEnv.regBinding(
    'BIT_PATTERN_UNDEF',
    ConstValue.getConst(
        25,
        IRType.pint
    )
);

//=============================================================================
//
// Object memory layout constants
//
//=============================================================================

// Offset of the protype pointer
staticEnv.regBinding(
    'OBJ_HEADER_SIZE',
    ConstValue.getConst(
        4,
        IRType.pint
    )
);

// Offset of the protype pointer
staticEnv.regBinding(
    'OBJ_PROTO_PTR_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_HEADER_SIZE').value,
        IRType.pint
    )
);

// Offset of the hash table
staticEnv.regBinding(
    'OBJ_HASH_PTR_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_PROTO_PTR_OFFSET').value + IRType.box.size,
        IRType.pint
    )
);

// Offset of the hash table size
staticEnv.regBinding(
    'OBJ_HASH_SIZE_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_HASH_PTR_OFFSET').value + IRType.box.size,
        IRType.pint
    )
);

// Size of a basic object
staticEnv.regBinding(
    'OBJ_SIZE',
    ConstValue.getConst(
        staticEnv.getBinding('OBJ_HASH_SIZE_OFFSET').value + IRType.i32.size,
        IRType.pint
    )
);

// Size of a hash table entry
staticEnv.regBinding(
    'OBJ_HASH_ENTRY_SIZE',
    ConstValue.getConst(
        16,
        IRType.pint
    )
);

// Size of a hash table key
staticEnv.regBinding(
    'OBJ_HASH_KEY_SIZE',
    ConstValue.getConst(
        8,
        IRType.pint
    )
);

// Value of the empty-slot hash table key
staticEnv.regBinding(
    'OBJ_HASH_EMPTY_KEY',
    ConstValue.getConst(
        0,
        IRType.box
    )
);

//=============================================================================
//
// String memory layout constants
//
//=============================================================================

// Offset of the protype pointer
staticEnv.regBinding(
    'STR_CODE_UNIT_SIZE',
    ConstValue.getConst(
        IRType.u16.size,
        IRType.pint
    )
);

// Offset of the protype pointer
staticEnv.regBinding(
    'STR_HEADER_SIZE',
    ConstValue.getConst(
        4,
        IRType.pint
    )
);

// Offset of the protype pointer
staticEnv.regBinding(
    'STR_LEN_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('STR_HEADER_SIZE').value,
        IRType.pint
    )
);

// Offset of string data
staticEnv.regBinding(
    'STR_DATA_OFFSET',
    ConstValue.getConst(
        staticEnv.getBinding('STR_LEN_OFFSET').value + IRType.i32.size,
        IRType.pint
    )
);

