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

    // Create the contect object layout
    makeContextLayout(config.params);

    // Create the object layouts
    makeObjectLayouts(config.params);

    // Compile the IR primitives
    backend.primitiveList = compPrimitives(config.params);

    /*
    var func = staticEnv.getBinding('extObjHashTbl');
    print(func);
    */

    for (var primIt = new ArrayIterator(backend.primitiveList);
         primIt.valid();
         primIt.next())
    {
        //print(primIt.get());

        compileIR(primIt.get(), config.params);
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

