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
    var primIt;

    // Compile the IR primitives
    backend.primitiveList = compPrimitives();

    for (primIt = new ArrayIterator(backend.primitiveList);
         primIt.valid();
         primIt.next())
    {
        compileIR(primIt.get());
    }
}

/**
Uninitialize resources used by the Tachyon VM
*/
function uninitialize()
{
    var primIt;
    for (primIt = new ArrayIterator(backend.primitiveList);
         primIt.valid();
         primIt.next())
    {
        primIt.get().runtime.free();
    }
}

