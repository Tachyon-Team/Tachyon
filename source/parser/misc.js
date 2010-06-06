//=============================================================================

// File: "misc.js", Time-stamp: <2010-05-24 16:40:58 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Interface to D8 specific functions.

function read_file(filename)
{
    return read(filename);
}

var command_line_arguments = arguments;

function command_line()
{
    return command_line_arguments;
}

//-----------------------------------------------------------------------------

// I/O.

function File_input_port(filename)
{
    this.filename = filename;
    this.content  = read_file(filename);
    this.pos      = 0;

    // method read_char()

    this.read_char = function ()
    {
        var content = this.content;
        if (this.pos < content.length)
            return content.charCodeAt(this.pos++);
        return EOF;
    };
}

//=============================================================================
