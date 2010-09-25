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
    // Compile the IR primitives
    var i;
    var irList = compPrimitives();
    var primitives = irList[0].getChildrenList().concat(irList[1].getChildrenList());
    var map = {};
    var p;

    for (i=0; i < primitives.length; ++i)
    {
        p = primitives[i];
        map[p.funcName] = p;
        //print(p);
    }
    backend.primitiveMap = map;
}

/**
Uninitialize resources used by the Tachyon VM
*/
function uninitialize()
{
}

