/**
@fileOverview
Implementation of runtime context objects

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
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
    Run-time context layout object
    */
    var ctxLayout = new MemLayout('ctx', IRType.rptr, undefined, params);

    // Global object
    ctxLayout.addField(
        'globalobj',
        IRType.box
    );

    // String table
    ctxLayout.addField(
        'strtbl',
        IRType.box
    );

    // Object prototype object
    ctxLayout.addField(
        'objproto',
        IRType.box
    );

    // Temporary slot for Memory to Memory moves
    ctxLayout.addField(
        'temp',
        IRType.box
    );

    // Function prototype object
    ctxLayout.addField(
        'funcproto',
        IRType.box
    );

    // Range error constructor
    ctxLayout.addField(
        'rangeerror',
        IRType.box
    );

    // Reference error constructor
    ctxLayout.addField(
        'referror',
        IRType.box
    );

    // Syntax error constructor
    ctxLayout.addField(
        'syntaxerror',
        IRType.box
    );

    // Type error constructor
    ctxLayout.addField(
        'typeerror',
        IRType.box
    );

    // URI error constructor
    ctxLayout.addField(
        'urierror',
        IRType.box
    );

    // Heap allocation pointer
    ctxLayout.addField(
        'allocptr',
        IRType.rptr
    );

    // Finalize the context layout
    ctxLayout.finalize();
}

