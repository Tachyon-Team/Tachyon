/**
@fileOverview
Implementation of runtime context objects

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Run-time context layout object
*/
var ctxLayout = new ObjectLayout('ctx', IRType.rptr);

// Global object
ctxLayout.addField(
    'globalobj',
    IRType.box
);

// Object prototype object
ctxLayout.addField(
    'objproto',
    IRType.box
);

// Function prototype object
ctxLayout.addField(
    'funcproto',
    IRType.box
);

// Type error constructor
ctxLayout.addField(
    'typeerror',
    IRType.box
);

// Heap allocation pointer
ctxLayout.addField(
    'allocptr',
    IRType.rptr
);

// Finalize the context layout
ctxLayout.finalize();

// Generate accessor methods
ctxLayout.genMethods();

