/**
@fileOverview
Misc. Tachyon-specific parameters and constants.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Constant to indicate we are running in Tachyon. This will be false
under d8, true under Tachyon. Note that when running under Tachyon,
this becomes a static binding, and does not require global variable
access.
*/
const RUNNING_IN_TACHYON = false;

/**
Constant to indicate the bootstrap generation number. This will be 0
under d8, and incremented by one for every level of bootstrap. Note
that when running under Tachyon, this becomes a static binding, and
does not require global variable access.
*/
const TACHYON_GEN_NUMBER = 0;

/**
Constant to indicate that we are running in debug mmode. This will be
true under d8, and change depending on the bootstrap configuration.
Note that when running under Tachyon, this becomes a static binding,
and does not require global variable access.
*/
const DEBUG = true;

/**
Create the Tachyon-specific constants
*/
function makeTachyonConsts(params)
{
    // Set the running in Tachyon constant to true
    params.staticEnv.regBinding(
        'RUNNING_IN_TACHYON', 
        ConstValue.getConst(true)
    );

    // Compute the bootstrap generation number
    params.staticEnv.regBinding(
        'TACHYON_GEN_NUMBER',
        ConstValue.getConst(
            TACHYON_GEN_NUMBER + 1,
            IRType.box
        )
    );

    // Bind the debug flag constant
    params.staticEnv.regBinding(
        'DEBUG',
        ConstValue.getConst(
            params.debug,
            IRType.box
        )
    );
}

