/**
@fileOverview
Useful array manipulation functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Add a value to an array set
*/
function arraySetAdd(array, val)
{
    for (var i = 0; i < array.length; ++i)
    {    
        if (array[i] === val)
        {
            return;
        }
    }

    array.push(val);
}

/**
Remove a value from an array set
*/
function arraySetRem(array, val)
{
    for (var i = 0; i < array.length; ++i)
    {
        if (array[i] === val)
        {
            array[i] = array[array.length - 1];
            array.pop();

            return;
        }
    }
}

