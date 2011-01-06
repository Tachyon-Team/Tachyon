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

    // Create the context and object layouts
    makeContextLayout(config.hostParams);
    makeObjectLayouts(config.hostParams);

    // Compile the primitives to IR for both configurations
    compPrimitives(config.hostParams);

    // Compile the primitives to machine code
    for (var primIt = new ArrayIterator(config.hostParams.primIR);
         primIt.valid();
         primIt.next())
    {
        //print(primIt.get());

        compileIR(primIt.get(), config.hostParams);
    }

    // TODO: create layouts, compile primitives for bootstrap
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

