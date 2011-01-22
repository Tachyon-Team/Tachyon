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
    // Initialize the Tachyon configuration
    initConfig();

    //config.hostParams.print = print;

    // Create the context and object layouts
    makeContextLayout(config.hostParams);
    makeObjectLayouts(config.hostParams);

    // Initialize the FFI functions
    initFFI(config.hostParams);

    // Compile the Tachyon primitives
    compTachyon(false, config.hostParams);
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

