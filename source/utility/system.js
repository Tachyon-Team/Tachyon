/**
@fileOverview
Basic system/OS interface functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Store the command-line arguments in a globally visible variable.
*/
var command_line_arguments = arguments;

/**
Get the command-line arguments
*/
function command_line()
{
    return command_line_arguments;
}

/**
Parse command-line arguments
*/
function parseCmdLine()
{
    var args = command_line();

    // Map for named arguments
    var options = {};

    // List for trailing unnamed arguments
    var files = [];

    var argIdx = 0;

    // For each named argument
    for (; argIdx < args.length; argIdx++)
    {
        var arg = args[argIdx];

        // If this is not an option argument, stop
        if (arg.charAt(0) !== '-')
            break;

        // Get the option name
        var optName = arg.slice(1);

        if (optName === 'e')
        {
            ++argIdx;
            if (argIdx >= args.length)
            {
                error("No argument specified for -e");
            }
            var optVal = args[argIdx];
        }
        else
        {
            var eqIndex = optName.indexOf("=");

            if (eqIndex === -1)
            {
                var optVal = true;
            }
            else
            {
                var optVal = optName.slice(eqIndex+1);
                optName = optName.slice(0, eqIndex);
            }
        }

        // Store the option value
        options[optName] = optVal;
    }

    // For each remaining argument
    for (; argIdx < args.length; ++argIdx)
    {
        // Add it to the file arguments
        files.push(args[argIdx]);
    }

    // Return the parsed arguments
    return {
        "options"   : options,
        "files"     : files
    };
}

