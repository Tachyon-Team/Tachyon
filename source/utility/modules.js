/**
@fileOverview
Utility code to allow for the creation of a module system using global
objects with read-only properties

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Function to create a new module
*/
function makeModule(modName, parentObj)
{
    // If no parent object was set, use the global object
    if (parentObj === null || parentObj === undefined)
        parentObj = getGlobalObj;

    // If the property already exists, return its value
    if (parentObj.hasOwnProperty(modName))
        return parentObj[modName];

    // Create an object for the new module
    newMod = {};

    // Define a read-only empty object property in the parent
    Object.defineProperty(parentObj, modName, {value:newMod, writable:false});

    /**
    Function to add a non-redefinable function to the module
    @ignore
    */
    newMod.addFunction = function(fName, fObj)
    {
        Object.defineProperty(this, fName, {value:fObj, writable:false});        
    };

    // Return a reference to the new module
    return newMod;
}

