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
var TAG_NUM_BITS_INT = 2;
staticEnv.regBinding(
    'TAG_NUM_BITS_INT',
    ConstValue.getConst(
        TAG_NUM_BITS_INT,
        IRType.pint
    )
);

// Number of reference tag bits
var TAG_NUM_BITS_REF = 3;
staticEnv.regBinding(
    'TAG_NUM_BITS_REF',
    ConstValue.getConst(
        TAG_NUM_BITS_REF,
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
    'TAG_STRING',
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
// GC and memory management constants
//
//=============================================================================

// Alignment for heap allocation
staticEnv.regBinding(
    'HEAP_ALIGN',
    ConstValue.getConst(
        8,
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
var BIT_PATTERN_TRUE = 1;
staticEnv.regBinding(
    'BIT_PATTERN_TRUE',
    ConstValue.getConst(
        BIT_PATTERN_TRUE,
        IRType.pint
    )
);

// Bit pattern for the false constant
var BIT_PATTERN_FALSE = 9;
staticEnv.regBinding(
    'BIT_PATTERN_FALSE',
    ConstValue.getConst(
        BIT_PATTERN_FALSE,
        IRType.pint
    )
);

// Bit pattern for the null constant
var BIT_PATTERN_NULL = 17;
staticEnv.regBinding(
    'BIT_PATTERN_NULL',
    ConstValue.getConst(
        BIT_PATTERN_NULL,
        IRType.pint
    )
);

// Bit pattern for the undefined constant
var BIT_PATTERN_UNDEF = 25;
staticEnv.regBinding(
    'BIT_PATTERN_UNDEF',
    ConstValue.getConst(
        BIT_PATTERN_UNDEF,
        IRType.pint
    )
);

//=============================================================================
//
// Object memory layout
//
//=============================================================================

/**
Hash table entry layout object
*/
var hashEntryLayout = new ObjectLayout('hashentry');

// Hash table key
hashEntryLayout.addField(
    'key',
    IRType.box
);

// Hash table value
hashEntryLayout.addField(
    'val',
    IRType.box
);

// Finalize the hash table entry layout
hashEntryLayout.finalize();

/**
Hash table entry layout object
*/
var hashTblLayout = new ObjectLayout('hashtbl');

//
// TODO: header
//

// Hash table key
hashTblLayout.addField(
    'tbl',
    hashEntryLayout,
    undefined,
    Infinity
);

// Finalize the hash table layout and generate accessors
hashTblLayout.finalize();
hashTblLayout.genMethods();

/**
Object layout object
*/
var objLayout = new ObjectLayout('obj');

//
// TODO: header
//

// Prototype reference
objLayout.addField(
    'proto',
    IRType.box
);

// Hash table
objLayout.addField(
    'tbl',
    IRType.box
);

// Hash table size
objLayout.addField(
    'tblsize',
    IRType.i32
);

// Hash table size
objLayout.addField(
    'numprops',
    IRType.i32
);

// Finalize the object layout and generate accessors
objLayout.finalize();
objLayout.genMethods();

// Initial hash map size
staticEnv.regBinding(
    'HASH_MAP_INIT_SIZE',
    ConstValue.getConst(
        10,
        IRType.pint
    )
);

// Hash map max load factor (num/denum)
staticEnv.regBinding(
    'HASH_MAP_MAX_LOAD_NUM',
    ConstValue.getConst(
        3,
        IRType.pint
    )
);
staticEnv.regBinding(
    'HASH_MAP_MAX_LOAD_DENUM',
    ConstValue.getConst(
        5,
        IRType.pint
    )
);

//=============================================================================
//
// String memory layout
//
//=============================================================================

/**
String layout object
*/
var strLayout = new ObjectLayout('str');

//
// TODO: header
//

// Global object
strLayout.addField(
    'len',
    IRType.i32
);

// Object prototype object
strLayout.addField(
    'data',
    IRType.u16,
    undefined,
    Infinity
);

// Finalize the string layout and generate accessors
strLayout.finalize();
strLayout.genMethods();

