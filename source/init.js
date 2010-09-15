/**
@fileOverview
Initialization code for the Tachyon VM.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Initialize the resources required by the Tachyon VM.
*/
function initialize()
{
    // TODO: delay compilation of layout methods into compPrimitives
    ctxLayout.genMethods();

    // Compile the IR primitives
    compPrimitives()
}

/**
Uninitialize resources used by the Tachyon VM
*/
function uninitialize()
{
}

