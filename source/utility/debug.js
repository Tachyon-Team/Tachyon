/**
@fileOverview
Utility code to facilitate debugging.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Implementation of c-like inline assertion functionality
*/
function assert(test, errorText)
{
    if (test == false)
        throw errorText;
}

