//=============================================================================

// File: "misc.js", Time-stamp: <2011-03-22 14:04:55 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Interface to D8 specific functions.

function read_file(filename)
{
    return readFile(filename);
}

//-----------------------------------------------------------------------------

// I/O.

var EOF = -1;

function File_input_port(filename)
{
    return new String_input_port(
        read_file(filename),
        filename
    );
}

function String_input_port(content, filename)
{
    if (filename === undefined)
        filename = '<string>';

    this.filename = filename;
    this.content = content;
    this.pos = 0;

    this.read_char = function ()
    {
        var content = this.content;
        if (this.pos < content.length)
            return content.charCodeAt(this.pos++);
        else
            return EOF;
    };
}

function String_output_port(init)
{
    this.char_buffer = [];
    this.string_buffer = [];

    // method write_char(c)

    this.empty_char_buffer = function ()
    {
        if (this.char_buffer.length > 0)
        {
            this.string_buffer.push(String.fromCharCode.apply(null, this.char_buffer));
            this.char_buffer = [];
        }
    };

    // method write_char(c)

    this.write_char = function (c)
    {
        this.char_buffer.push(c);
        if (this.char_buffer.length > 500)
            this.empty_char_buffer();
    };

    // method write_string(str)

    this.write_string = function (str)
    {
        for (var i=0; i<str.length; i++)
            this.write_char(str.charCodeAt(i));
    };

    // method get_output_string()

    this.get_output_string = function ()
    {
        this.empty_char_buffer();
        return String.prototype.concat.apply("", this.string_buffer);
    };

    this.write_string(init);
}

//-----------------------------------------------------------------------------

function parse_src_file(filename, params)
{
    assert (
        filename !== undefined,
        'expected file name'
    );

    return parse_src_port(new File_input_port(filename), params);
}

function parse_src_str(str, params)
{
    assert (
        str !== undefined,
        'expected source string'
    );

    return parse_src_port(new String_input_port(str), params);
}

function parse_src_port(port, params)
{
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    var p = new Parser(new Scanner(port), params.parserWarnings, params);
    var ast = p.parse();

    var normalized_ast = ast_normalize(ast, params.debugTrace);

    return normalized_ast;
}

//=============================================================================
