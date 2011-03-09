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
function initialize(boot)
{
    // Initialize the Tachyon configuration
    initConfig();

    try
    {
        // Compile and initialize the Tachyon primitives
        bootstrap(boot, boot? config.bootParams:config.hostParams);
    } 
    catch (e)
    {
        print(e.stack);
        return false;
    }

    return true;
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

