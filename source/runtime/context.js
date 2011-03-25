/**
@fileOverview
Implementation of runtime context objects

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Create the context object layout for a given architecture
*/
function makeContextLayout(params)
{
    /** 
    Static environment constants for the context object 
    */
   
    // Alignment for heap allocation 
    params.staticEnv.regBinding(
        'CTX_ALIGN',
        ConstValue.getConst(
            256,
            IRType.pint
        )
    );

    /**
    Run-time context layout object. This layout extends the backend context.
    */
    var ctxLayout = MemLayout.extend(params.target.backendCfg.ctxLayout, 'ctx');

    // Ensure that the context pointer type is valid
    assert (
        ctxLayout.ptrType === IRType.ref,
        'invalid pointer type for context layout'
    );

    // Global object
    ctxLayout.addField(
        'globalobj',
        IRType.box,
        'null'
    );

    // Heap start pointer
    ctxLayout.addField(
        'heapstart',
        IRType.rptr
    );

    // Heap limit pointer
    ctxLayout.addField(
        'heaplimit',
        IRType.rptr
    );

    // Heap allocation pointer
    ctxLayout.addField(
        'allocptr',
        IRType.rptr
    );

    // String table
    ctxLayout.addField(
        'strtbl',
        IRType.box,
        'null'
    );

    // Object prototype object
    ctxLayout.addField(
        'objproto',
        IRType.box,
        'null'
    );

    // Function prototype object
    ctxLayout.addField(
        'funcproto',
        IRType.box,
        'null'
    );

    // Array prototype object
    ctxLayout.addField(
        'arrproto',
        IRType.box,
        'null'
    );

    // String prototype object
    ctxLayout.addField(
        'strproto',
        IRType.box,
        'null'
    );

    // Number prototype object
    ctxLayout.addField(
        'numproto',
        IRType.box,
        'null'
    );

    // Range error constructor
    ctxLayout.addField(
        'rangeerror',
        IRType.box,
        'null'
    );

    // Reference error constructor
    ctxLayout.addField(
        'referror',
        IRType.box,
        'null'
    );

    // Syntax error constructor
    ctxLayout.addField(
        'syntaxerror',
        IRType.box,
        'null'
    );

    // Type error constructor
    ctxLayout.addField(
        'typeerror',
        IRType.box,
        'null'
    );

    // URI error constructor
    ctxLayout.addField(
        'urierror',
        IRType.box,
        'null'
    );

    // Finalize the context layout
    ctxLayout.finalize();
}

