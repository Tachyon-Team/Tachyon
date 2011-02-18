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

        // If no option value is present, report an error
        if (argIdx >= args.length - 1)
        {
            error('missing value for command-line option "' + optName + '"');
        }

        // Read the option value
        var optVal = args[argIdx + 1];
        argIdx++;

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

