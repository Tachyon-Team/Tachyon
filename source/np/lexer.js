/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Lexer implementation.

@author
Maxime Chevalier-Boisvert
*/

/**
Test if one or two characters are a line terminator sequence
*/
function charsAreNewline(ch0, ch1)
{
    return (
        (ch0 === 10)                ||  // <LF>
        (ch0 === 13 && ch1 !== 10)  ||  // <CR>
        (ch0 === 8232)              ||  // <LS>
        (ch0 === 8233)              ||  // <PS>
        (ch0 === 13 && ch1 === 10)      // <CR><LF>
    );           
}

/**
Test if a character code is whitespace
*/
function charIsWS(charCode)
{
    switch (charCode)
    {
        case 9:     // Tab
        case 10:    // Line feed
        case 11:    // Vertical tab
        case 12:    // Form feed
        case 13:    // Carriage return
        case 32:    // Space
        case 160:   // Non-breaking space
        case 65279: // Byte-order mark
        return true;

        default:
        return false;
    }
}

/**
Test if a character code is a digit
*/
function charIsDigit(charCode)
{
    return (charCode >= 48 && charCode <= 57);
}

/**
Test if a character code is a hexadecimal digit
*/
function charIsHexDigit(charCode)
{
    return (
        (charCode >= 48 && charCode <= 57)  ||  // 0-9
        (charCode >= 97 && charCode <= 122) ||  // a-z
        (charCode >= 64 && charCode <= 90)      // A-Z
    );
}

/**
@class Source code lexeme
*/
function Lexeme(type, pos, value)
{
    /**
    Token type
    */
    this.type = type; 
    
    /**
    Source position
    */
    this.pos = pos;

    /**
    Optional constant value
    */
    this.value = value;
}

/**
Lexeme types
*/
Lexeme.types = [

    // Punctuators
    '{', '}', '(', ')', '[', ']',
    '.', ';', ',',
    ':',

    // Operators
    '<', '>', '<=', '>=', '==', '!=', '===', '!==',
    '+', '-', '*', '%', '++', '--', '<<', '>>', '>>>',
    '&', '|', '^', '~', '!',
    '&&', '||', 
    '?',
    '=', '+=', '-=', '*=', '%=', '<<=', '>>=', '>>>=', '&=', '|=', '^=',

    // Keywords
    'var', 'const', 
    'function', 'this', 'return',
    'if', 'else',
    'do', 'while', 'for', 'in', 'break', 'continue',
    'switch', 'case', 'default',
    'try', 'catch', 'finally',
    'new', 'instanceof', 'typeof', 'delete', 'throw',
    'with',
    'debugger',

    // Terminals
    'ident', 'number', 'string', 'regexp',

    // Comments
    'comment',

    // Newline
    'newline',

    // End of file
    'eof'
];

/**
@class Lexer
*/
function Lexer(str, fileName)
{
    assert (
        typeof str === 'string',
        'invalid input string'
    );

    /**
    Code string to parse
    */
    this.str = str;

    /**
    File being parsed
    */
    this.fileName = fileName;

    /**
    Current character index
    */
    this.curIdx = 0;

    /**
    Current source code line
    */
    this.curLine = 1;

    /**
    Current source code position
    */
    this.curCol = 1;

    /**
    Marked start line
    */
    this.startLine = undefined;

    /**
    Marked start column
    */
    this.startCol = undefined;

    /**
    Marked end line
    */
    this.endLine = undefined;

    /**
    Marked end column
    */
    this.endCol = undefined;
}

/**
Peek at a character of the input
*/
Lexer.prototype.peekCh = function (offset)
{
    if (offset === undefined)
        offset = 0;

    return this.str.charCodeAt(this.curIdx + offset);
}

/**
Read and consume a character from the input
*/
Lexer.prototype.readCh = function ()
{
    //print('readCh');

    return this.str.charCodeAt(this.curIdx++);
}

/**
Mark the start of the current token
*/
Lexer.prototype.markStart = function ()
{
    this.startLine = this.curLine;
    this.startCol = this.curCol;
}

/**
Mark the end of the current token
*/
Lexer.prototype.markEnd = function ()
{
    this.endLine = this.curLine;
    this.endCol = this.curCol;
}

/**
Get the current source position
*/
Lexer.prototype.getPos = function ()
{
    assert (
        this.endLine >= this.startLine &&
        (this.endLine !== this.startLine || this.endCol >= this.startCol),
        'invalid lexeme position marking'
    );

    return new SrcPos(
        this.fileName,
        this.startLine,
        this.startCol,
        this.endLine,
        this.endCol
    );
}

/**
Consume a line terminator sequence
*/
Lexer.prototype.newline = function ()
{
    print('newline');

    this.markStart();

    var ch0 = this.readCh();
    var ch1 = this.peekCh();

    // <LF> or <LS> or <PS> or <CR>
    if (ch0 === 10                  ||
        ch0 === 8232                ||
        ch0 === 8233                ||
        ch0 === 13 && ch1 !== 10)
    {
        // Do nothing
    }

    // <CR> <LF>
    else if (ch0 === 13 && ch1 === 10)
    {
        this.readCh();
    } 
    else 
    {
        error('newline on invalid line terminator sequence');
    }

    this.markEnd();

    // Update the current position
    this.curLine++;
    this.curCol = 1;

    return new Lexeme('newline', this.getPos());
}

/**
Get the current token
*/
Lexer.prototype.getToken = function (/*TODO: exprContext*/)
{
    // Until a lexeme is read or the end of file is reached
    while (this.curIdx < this.str.length)
    {
        // Get the char code for this character and the next
        var ch0 = this.peekCh(0);
        var ch1 = this.peekCh(1);

        //print(this.curIdx + ': ' + ch0);

        // Line terminator sequence
        if (charsAreNewline(ch0, ch1) === true)
        {
            return this.newline();
        }

        // Whitespace characters
        else if (charIsWS(ch0) === true)
        {
            print('skipping ws');

            // Consume the character and iterate again
            this.readCh();
            continue;
        }

        // Start of a string literal (" or ' characters)
        else if (ch0 === 34 || ch0 === 39)
        {
            return this.lexString();
        }

        // Start of a hex number (0x... or 0X...)
        else if (ch0 === 48 && (ch1 === 120 || ch1 === 88))
        {
            return this.lexHexNumber();
        }

        // Start of a decimal number literal
        else if (charIsDigit(ch0) === true)
        {
            this.lexDecNumber();
        }

        // Line comment (//)
        else if (ch0 === 47 && ch1 === 47)
        {
            this.lexLineComment();
        }

        // Multiline comment (/*)
        else if (ch0 === 47 && ch1 === 42)
        {
            this.lexMultiComment();
        }

        // TODO: separators, operators
        // TODO: keywords
        // TODO: symbols
        /*
        // Otherwise, this is either a separator, an operator or a symbol
        else
        {
            return this.lexOther();
        }
        */

        // TODO: regular expressions
        // How do you know that the initial / is not the division operator?
    }

    // End of file
    this.markStart();
    this.markEnd();
    return new Lexeme('eof', this.getPos());
}

/**
Tokenize a string constant
*/
Lexer.prototype.lexString = function ()
{
    print('lexing string');

    // Mark the start of the token
    this.markStart();

    // Get the string opening character
    var openChar = this.readCh();

    // Character codes for the string
    var strChars = [];

    // For each character in the input
    for (;;)
    {
        // End of input
        if (this.curIdx >= this.str.length)
        {
            error('unterminated string literal');
        }

        // Get the char code for this character
        var ch0 = this.peekCh(0);
        var ch1 = this.peekCh(1);
        var ch2 = this.peekCh(2);

        // Newline character
        if (charsAreNewline(ch0, ch1) === true)
        {
            lexError('newline inside string literal');
        }

        // Line continuation, \newline
        else if (ch0 === 92 && charsAreNewline(ch1, ch2) === true)
        {
            this.readCh();
            this.newline();
            strChars.push(10);
            continue;
        }

        // If this is the string closing
        else if (ch0 === openChar)
        {
            this.markEnd();
            this.readCh();
            break;
        }

        // Consume the current character
        this.readCh();

        //
        // TODO: char escape codes
        //

        /*
        SingleEscapeCharacter :: one of
        ' " \ b f n r t v
        NonEscapeCharacter ::
        SourceCharacter but not EscapeCharacter or LineTerminator
        EscapeCharacter ::
        SingleEscapeCharacter DecimalDigit x u
        HexEscapeSequence ::
        x HexDigit HexDigit
        UnicodeEscapeSequence ::
        u HexDigit HexDigit HexDigit HexDigit
        */

        // Add the current character code to the array
        strChars.push(ch0);
    }

    // Get the string from the character codes
    var str = String.fromCharCode.apply(null, strChars);

    return new Lexeme('string', this.getPos(), str);
}

/**
Tokenize a hexadecimal number
*/
Lexer.prototype.lexHexNumber = function ()
{
    print('lexing hexadecimal number');

    // Mark the start of the token
    this.markStart();

    // Character codes for the number
    var numChars = [];

    // Add the hex prefix to the number characters
    numChars.push(this.readCh());
    numChars.push(this.readCh());

    // For each character in the input
    for (;;)
    {
        // End of input
        if (this.curIdx >= this.str.length)
        {
            break;
        }

        // Get the char code for this character
        var ch0 = this.peekCh(0);
        var ch1 = this.peekCh(1);

        // Newline character
        if (charsAreNewline(ch0, ch1) === true)
        {
            this.newline();
            break;
        }

        // If this is the end of the number
        else if (charIsHexDigit(ch0) === false)
        {
            break;
        }

        // Mark the possible end of the number
        this.markEnd();

        // Add the current character code to the array and consume it
        numChars.push(this.readCh());
    }

    // Get the number from the character codes
    var num = Number(String.fromCharCode.apply(null, numChars));

    // Create and store the string literal
    return new Lexeme('number', this.getPos(), num);
}

/**
Tokenize a decimal number
*/
Lexer.prototype.lexDecNumber = function ()
{
    print('lexing decimal number');

    // [-+]? [0-9]* \.? [0-9]+ ([eE][-+]?[0-9]+)?

    // Mark the start of the token
    this.markStart();

    // Character codes for the number
    var numChars = [];

    // Consume a character and add it to the number characters
    var lexer = this;
    function readCh()
    {
        numChars.push(lexer.readCh());
    }

    // States:
    // INT_PART
    // POINT
    // FRAC_PART
    // EXP
    // EXP_SIGN
    // EXP_PART
    // DONE
    var state = 'INT_PART';

    // For each character
    while (state !== 'DONE')
    {
        // Read the current character
        var ch0 = this.peekCh(0);

        switch (state)
        {
            case 'INT_PART':
            {
                if (charIsDigit(ch0) === true)      // digit
                    readCh();
                else if (ch0 === 46)                // .
                    state = 'POINT';
                else if (ch0 === 101 || ch0 === 69) // e or E
                    state = 'EXP';
                else
                    state = 'DONE';
            }
            break;

            case 'POINT':
            {
                // .
                readCh();
                state = 'FRAC_PART';
            }
            break;

            case 'FRAC_PART':
            {
                if (charIsDigit(ch0) === true)      // digit
                    readCh();
                else if (ch0 === 101 || ch0 === 69) // e or E
                    state = 'EXP';
                else
                    state = 'DONE';
            }
            break;

            case 'EXP':
            {
                // e or E
                readCh();
                state = 'EXP_SIGN';
            }
            break;

            case 'EXP_SIGN':
            {
                if (ch0 === 43 || ch0 === 45)       // + or -
                    readCh();
                else if (charIsDigit(ch0) === true) // digit
                    state = 'EXP_PART';
                else
                    error('invalid character in number');
            }
            break;

            case 'EXP_PART':
            {
                if (charIsDigit(ch0) === true)
                    readCh();
                else
                    state = 'END'
            }
            break;

            default:
            error('invalid state: "' + state + '"');
        }
    }

    this.markEnd();

    // Get the number from the character codes
    var num = Number(String.fromCharCode.apply(null, numChars));

    // Create and store the string literal
    return new Lexeme('number', this.getPos(), num);
}

/**
Tokenize a line comment
*/
Lexer.prototype.lexLineComment = function ()
{
    print('lexing line comment');

    // Move past the comment opening
    this.readCh();
    this.readCh();
    
    // For each character in the input
    for (;;)
    {
        // End of input
        if (this.curIdx >= this.str.length)
        {
            break;
        }

        // Get the char code for this character
        var ch0 = this.peekCh(0);
        var ch1 = this.peekCh(1);

        // Newline character
        if (charsAreNewline(ch0, ch1) === true)
        {
            // End of the comment
            // We do not consume the newline for proper
            // auto semicolon insertion semantics
            break;
        }

        // Consume the current character
        this.readCh();
    }
}

/**
Tokenize a multi-line comment
*/
Lexer.prototype.lexMultiComment = function ()
{
    print('lexing multi-line comment');

    // Move past the comment opening
    this.readCh();
    this.readCh();
    
    // For each character in the input
    for (;;)
    {
        // End of input
        if (this.curIdx >= this.str.length)
        {
            error('unterminated multi-line comment');
        }

        // Get the char code for this character
        var ch0 = this.peekCh(0);
        var ch1 = this.peekCh(1);

        // Newline character
        if (charsAreNewline(ch0, ch1) === true)
        {
            // Skip the newline
            this.newline();
            continue;
        }

        // Comment closing
        else if (ch0 === 42 && ch1 === 47)
        {
            this.readCh();
            this.readCh();
            break;
        }

        // Consume the current character
        this.readCh();
    }
}

