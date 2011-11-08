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

//=============================================================================

// File: "scanner.js", Time-stamp: <2011-03-22 14:05:07 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Note: the scanner is incomplete.  It currently lacks support for:
//
//  - some string escapes
//  - hexadecimal numbers
//  - floating point numbers
//  - Unicode syntax
//
// Performance-wise, the scanner processes about 100,000 lines of code
// per second on a 2.8 GHz Intel Core 2 Duo (when running in V8).

//-----------------------------------------------------------------------------


function Scanner(port)
{
    this.port             = port;
    this.current_char_pos = 0;
    this.current_line_pos = 0;
    this.current_line     = 0;
    this.peeked_char      = null;
    this.peeked_char_pos  = null;
    this.pos_window       = [null, null, null, null, null];
    this.char_window      = [null, null, null, null, null];
    this.window_size      = 0;
    this.crossed_eol      = false;
}


// method read_char()

Scanner.prototype.read_char = function ()
{
    var c = this.port.read_char();
    if (c !== EOF)
        this.current_char_pos++;
    return c;
};


// method get_char()

Scanner.prototype.get_char = function ()
{
    var c = this.peeked_char;

    if (c !== null)
        this.peeked_char = null;
    else
        c = this.read_char();

    if (c === LF_CH)
    {
        this.current_line++;
        this.current_line_pos = this.current_char_pos;
        return EOL_CH;
    }
    else if (c === CR_CH)
    {
        this.current_line++;
        this.current_line_pos = this.current_char_pos;
        this.peeked_char_pos = this.current_char_pos;
        var next = this.read_char();
        if (next === LF_CH)
            this.current_line_pos = this.current_char_pos;
        else
            this.peeked_char = next; // remember for next time
        return EOL_CH;
    }
    else
        return c;
};


// method advance(i)

Scanner.prototype.advance = function (i)
{
    var j = 0;
    this.fill_window(i);
    while (i < this.window_size)
    {
        var p = this.pos_window[i];
        var c = this.char_window[i];
        //delete this.pos_window[i];
        //delete this.char_window[i];
        this.pos_window[j] = p;
        this.char_window[j] = c;
        i++;
        j++;
    }
    this.window_size = j;
};


// method lookahead_pos(i)

Scanner.prototype.lookahead_pos = function (i)
{
    this.fill_window(i+1);
    return this.pos_window[i];
};


// method lookahead_char(i)

Scanner.prototype.lookahead_char = function (i)
{
    this.fill_window(i+1);
    return this.char_window[i];
};


// method fill_window(n)

Scanner.prototype.fill_window = function (n)
{
    // fill first n entries of the lookahead window
    var s = this.window_size;
    if (s < n)
    {
        var i = s;
        while (i < n)
        {
            var cp = (this.peeked_char === null)
                     ? this.current_char_pos
                     : this.peeked_char_pos;
            this.pos_window[i] =
                line_and_column_to_position(this.current_line,
                                            cp - this.current_line_pos);
            this.char_window[i] = this.get_char();
            i++;
        }
        this.window_size = i;
    }
};


// method get_token()

Scanner.prototype.get_token = function ()
{
    var c = this.lookahead_char(0);

    this.crossed_eol = false;

    for (;;)
    {
        if (c === EOF)
            return this.simple_token(EOI_CAT, 0);
        else if (c === SPACE_CH || c === EOL_CH || c === TAB_CH)
        {
            if (c === EOL_CH)
                this.crossed_eol = true;
            this.advance(1);
            c = this.lookahead_char(0);
        }
        else if (this.identifier_class(c))
            return this.parse_identifier();
        else if (this.decimal_class(c))
            return this.parse_number();
        else if (c === PERIOD_CH)
        {
            if (this.decimal_class(this.lookahead_char(1)))
                return this.parse_number();
            else
                return this.simple_token(PERIOD_CAT, 1);
        }
        else if (c === EXCL_CH)
        {
            if (this.lookahead_char(1) === EQUAL_CH)
            {
                if (this.lookahead_char(2) === EQUAL_CH)
                    return this.simple_token(STRNEQ_CAT, 3);
                else
                    return this.simple_token(NE_CAT, 2);
            }
            else
                return this.simple_token(EXCL_CAT, 1);
        }
        else if (c === PERCENT_CH)
        {
            if (this.lookahead_char(1) === EQUAL_CH)
                return this.simple_token(MODEQUAL_CAT, 2);
            else
                return this.simple_token(MOD_CAT, 1);
        }
        else if (c === AMPERSAND_CH)
        {
            var x = this.lookahead_char(1);
            if (x === AMPERSAND_CH)
                return this.simple_token(AND_CAT, 2);
            else if (x === EQUAL_CH)
                return this.simple_token(BITANDEQUAL_CAT, 2);
            else
                return this.simple_token(BITAND_CAT, 1);
        }
        else if (c === STAR_CH)
        {
            if (this.lookahead_char(1) === EQUAL_CH)
                return this.simple_token(MULTEQUAL_CAT, 2);
            else
                return this.simple_token(MULT_CAT, 1);
        }
        else if (c === PLUS_CH)
        {
            var x = this.lookahead_char(1);
            if (x === PLUS_CH)
                return this.simple_token(PLUSPLUS_CAT, 2);
            else if (x === EQUAL_CH)
                return this.simple_token(PLUSEQUAL_CAT, 2);
            else
                return this.simple_token(PLUS_CAT, 1);
        }
        else if (c === MINUS_CH)
        {
            var x = this.lookahead_char(1);
            if (x === MINUS_CH)
                return this.simple_token(MINUSMINUS_CAT, 2);
            else if (x === EQUAL_CH)
                return this.simple_token(MINUSEQUAL_CAT, 2);
            else
                return this.simple_token(MINUS_CAT, 1);
        }
        else if (c === SLASH_CH)
        {
            var x = this.lookahead_char(1);
            if (x === SLASH_CH)
            {
                this.advance(2);
                for (;;)
                {
                    c = this.lookahead_char(0);
                    if (c === EOL_CH || c === EOF)
                    {
                        this.crossed_eol = true;
                        break;
                    }
                    this.advance(1);
                }
            }
            else if (x === STAR_CH)
            {
                this.advance(2);
                for (;;)
                {
                    c = this.lookahead_char(0);
                    if (c === EOF)
                        error("unterminated comment");
                    if (c === STAR_CH && this.lookahead_char(1) === SLASH_CH)
                        break;
                    if (c === EOL_CH)
                        this.crossed_eol = true;
                    this.advance(1);
                }
                this.advance(2);
                c = this.lookahead_char(0);
            }
            else if (x === EQUAL_CH)
                return this.simple_token(DIVEQUAL_CAT, 2);
            else
                return this.simple_token(DIV_CAT, 1);
        }
        else if (c === COLON_CH)
            return this.simple_token(COLON_CAT, 1);
        else if (c === EQUAL_CH)
        {
            if (this.lookahead_char(1) === EQUAL_CH)
            {
                if (this.lookahead_char(2) === EQUAL_CH)
                    return this.simple_token(STREQ_CAT, 3);
                else
                    return this.simple_token(EQEQ_CAT, 2);
            }
            else
                return this.simple_token(EQUAL_CAT, 1);
        }
        else if (c === LT_CH)
        {
            var x = this.lookahead_char(1);
            if (x === LT_CH)
            {
                if (this.lookahead_char(2) === EQUAL_CH)
                    return this.simple_token(LSHIFTEQUAL_CAT, 3);
                else
                    return this.simple_token(LSHIFT_CAT, 2);
            }
            else if (x === EQUAL_CH)
                return this.simple_token(LE_CAT, 2);
            else
                return this.simple_token(LT_CAT, 1);
        }
        else if (c === GT_CH)
        {
            var x = this.lookahead_char(1);
            if (x === GT_CH)
            {
                var y = this.lookahead_char(2);
                if (y === GT_CH)
                {
                    if (this.lookahead_char(3) === EQUAL_CH)
                        return this.simple_token(URSHIFTEQUAL_CAT, 4);
                    else
                        return this.simple_token(URSHIFT_CAT, 3);
                }
                else if (y === EQUAL_CH)
                    return this.simple_token(RSHIFTEQUAL_CAT, 3);
                else
                    return this.simple_token(RSHIFT_CAT, 2);
            }
            else if (x === EQUAL_CH)
                return this.simple_token(GE_CAT, 2);
            else
                return this.simple_token(GT_CAT, 1);
        }
        else if (c === QUESTION_CH)
            return this.simple_token(QUESTION_CAT, 1);
        else if (c === CARET_CH)
        {
            if (this.lookahead_char(1) === EQUAL_CH)
                return this.simple_token(BITXOREQUAL_CAT, 2);
            else
                return this.simple_token(BITXOR_CAT, 1);
        }
        else if (c === LPAREN_CH)
            return this.simple_token(LPAREN_CAT, 1);
        else if (c === RPAREN_CH)
            return this.simple_token(RPAREN_CAT, 1);
        else if (c === COMMA_CH)
            return this.simple_token(COMMA_CAT, 1);
        else if (c === SEMICOLON_CH)
            return this.simple_token(SEMICOLON_CAT, 1);
        else if (c === LBRACK_CH)
            return this.simple_token(LBRACK_CAT, 1);
        else if (c === VBAR_CH)
        {
            var x = this.lookahead_char(1);
            if (x === VBAR_CH)
                return this.simple_token(OR_CAT, 2);
            else if (x === EQUAL_CH)
                return this.simple_token(BITOREQUAL_CAT, 2);
            else
                return this.simple_token(BITOR_CAT, 1);
        }
        else if (c === RBRACK_CH)
            return this.simple_token(RBRACK_CAT, 1);
        else if (c === LBRACE_CH)
            return this.simple_token(LBRACE_CAT, 1);
        else if (c === RBRACE_CH)
            return this.simple_token(RBRACE_CAT, 1);
        else if (c === TILDE_CH)
            return this.simple_token(BITNOT_CAT, 1);
        else if (c === DOUBLEQUOTE_CH || c === QUOTE_CH)
            return this.parse_string();
        else
        {
            error("unknown token: " + c);
        }
    }
};


// method identifier_class()

Scanner.prototype.identifier_class = function (c)
{
    return (c >= LOWER_A_CH && c <= LOWER_Z_CH) ||
        (c >= UPPER_A_CH && c <= UPPER_Z_CH) ||
        c === UNDERSCORE_CH ||
        c === DOLLAR_CH;
};


// method decimal_class()

Scanner.prototype.decimal_class = function (c)
{
    return c >= ZERO_CH && c <= NINE_CH;
};

// method hexadecimal_class()

Scanner.prototype.hexadecimal_class = function (c)
{
    return Scanner.prototype.decimal_class(c) ||
           (c >= LOWER_A_CH && c <= LOWER_F_CH) ||
           (c >= UPPER_A_CH && c <= UPPER_F_CH);
};

// method parse_identifier()

Scanner.prototype.parse_identifier_old = function ()
{
    // This iterative algorithm extends the Array of characters using the
    // "push" method.  The growth of the array generates garbage.

    var start_pos = this.lookahead_pos(0);
    var chars = [];
    var h = 0;
    for (;;)
    {
        var c = this.lookahead_char(0);
        if (!(this.identifier_class(c) || this.decimal_class(c)))
            break;
        this.advance(1);
        chars.push(c);
        h = (h * HASH_MULT + c) % HASH_MOD;
    }
    var id = String.fromCharCode.apply(null,chars);
    var x = keyword_hashtable[h];
    if (x !== null && x.id === id)
        return this.valued_token(x.cat, id, start_pos);
    else
        return this.valued_token(IDENT_CAT, id, start_pos);
};

Scanner.prototype.parse_identifier = function ()
{
    var start_pos = this.lookahead_pos(0);
    var id = this.parse_identifier_string();
    var h = 0;
    for (var i=0; i<id.length; i++)
        h = (h * HASH_MULT + id.charCodeAt(i)) % HASH_MOD;
    var x = keyword_hashtable[h];
    if (x !== null && x.id === id)
        return this.valued_token(x.cat, id, start_pos);
    else
        return this.valued_token(IDENT_CAT, id, start_pos);
};

Scanner.prototype.parse_identifier_string = function ()
{
    return String.fromCharCode.apply(null,
                                     this.parse_identifier_string_loop(0));
}

Scanner.prototype.parse_identifier_string_loop = function (n)
{
    // This recursive algorithm saves the characters on the stack
    // and allocates an array of the correct size to hold them.
    // It does not generate garbage.

    var chars;
    var c = this.lookahead_char(0);

    if (this.identifier_class(c) || this.decimal_class(c))
    {
        this.advance(1);
        chars = this.parse_identifier_string_loop(n+1);
        chars[n] = c;
    }
    else
    {
        chars = new Array(n);
    }

    return chars;
}


// method parse_number()
// WARNING: The following implementation will not work
//          in a multi-threaded environment because
//          there could be a race condition on that
// TODO: Refactor not to use a closure and assume 
//       private functions will be lambda lifted
//       by the compiler
Scanner.prototype.parse_number = function ()
{
    // Assuming:
    //     digits      := [0-9]+
    //
    // 3 types of numbers can be parsed:
    //     decimal     := digits
    //     hexadecimal := 0(x|X)[0-9a-fA-F]+
    //     float       := [digits][.digits][(e|E)[(+|-)]digits]

    // Workaround to allow private helper functions
    // to access the "this" object
    var that;

    // Computes the value of a serie of digit characters
    // as if they are on the "left-hand side" of the decimal point
    var lhs_value = function (accepted_char, base, char_value) 
    {
        var n = 0;
        for (;;)
        {
            var c = that.lookahead_char(0);
            if (!accepted_char(c))
                break;
            that.advance(1);
            n = num_add(num_mul(n, base), char_value(c));
        }
        return n;
    };
    
    // Computes the value of a serie of digit characters
    // as if they are on the "right-hand side" of the decimal point
    var rhs_value = function (accepted_char, base, char_value)
    {
        var n = 0;
        var pos = 1;
        for (;;)
        {
            var c = that.lookahead_char(0);
            if (!accepted_char(c))
                break;
            that.advance(1);
            pos = pos * base;
            n = n * base + char_value(c);
        }
        return n/pos; // FIXME: remove reliance on floating point division
    };

    // Decimal helper functions
    function decimal (c)
    {
        return that.decimal_class(c);
    }
    
    function decimal_value (c)
    {
        return c - ZERO_CH;
    }   

    // Hex helper functions
    function hexadecimal (c)
    {
        return that.hexadecimal_class(c);
    }

    function hexadecimal_value (c) 
    {
        if (c >= LOWER_A_CH) 
        {
            return (c - LOWER_A_CH) + 10;
        } else if (c >= UPPER_A_CH)
        {
            return (c - UPPER_A_CH) + 10; 
        } else
        {
            return decimal_value(c);
        }
    }

    // parsing function
    return function ()
    {
        // Workaround for passing "this"
        that = this;

        var start_pos = that.lookahead_pos(0);
        var n;
        var fst_char = that.lookahead_char(0);
        var snd_char = that.lookahead_char(1);
        var exp_sign = 1;

        if (snd_char === LOWER_X_CH || snd_char === UPPER_X_CH)
        {
            // We got an hex number!
            that.advance(2);
            n = lhs_value(hexadecimal, 16, hexadecimal_value);
        }
        else 
        {
            // TODO: Use Clinger's algorithm:
            // http://portal.acm.org/citation.cfm?id=93542.93557

            // We got a decimal number! This should be
            // zero if the first character is a decimal point.
            n = lhs_value(decimal, 10, decimal_value);

            // We might have numbers after the decimal points
            if (that.lookahead_char(0) === PERIOD_CH)
            {
                that.advance(1);
                n = n + rhs_value(decimal, 10, decimal_value);
            }

            // Let's check for an exponent
            fst_char = that.lookahead_char(0);
            if (fst_char === LOWER_E_CH || fst_char === UPPER_E_CH)
            {
                that.advance(1);

                // The exponent might have a sign  
                fst_char = that.lookahead_char(0);
                if (fst_char === PLUS_CH)
                {
                    exp_sign = 1;
                    that.advance(1);
                } else if (fst_char === MINUS_CH)
                {  
                    exp_sign = -1;
                    that.advance(1);
                } 

                n = n * Math.pow(10, exp_sign * 
                                     lhs_value(decimal, 10, decimal_value));
            }
        }


        return that.valued_token(NUMBER_CAT, n, start_pos);
    };
}();


// method parse_string()

Scanner.prototype.parse_string = function ()
{
    var start_pos = this.lookahead_pos(0);
    var chars = [];
    var close = this.lookahead_char(0);
    this.advance(1);
    for (;;)
    {
        var c = this.lookahead_char(0);
        if (c === EOF)
            error("unterminated string");
        this.advance(1);
        if (c === close)
            break;
        else if (c === BACKSLASH_CH)
        {
            c = this.lookahead_char(0);
            if (c === EOF)
                error("unterminated string");
            this.advance(1);            if (c !== LF_CH)
            {
                if (c === LOWER_N_CH)
                    c = LF_CH;
                else if (c === ZERO_CH)
                    c = NUL_CH;
                else if (c === LOWER_B_CH)
                    c = BS_CH;
                else if (c === LOWER_T_CH)
                    c = TAB_CH;
                else if (c === LOWER_V_CH)
                    c = VT_CH;
                else if (c === LOWER_F_CH)
                    c = FF_CH;
                else if (c === LOWER_R_CH)
                    c = CR_CH;
                else if (c === LOWER_X_CH)
                {
                    // Parse \xXX string syntax
                    var value = 0, i = 0;

                    for (; i < 2; ++i)
                    {
                        var hc = this.lookahead_char(i);

                        if (hc >= LOWER_A_CH && hc <= LOWER_F_CH)
                            value = (value * 16) + (hc - 87);
                        else if (hc >= UPPER_A_CH && hc <= UPPER_F_CH)
                            value = (value * 16) + (hc - 55);
                        else if (hc >= ZERO_CH && hc <= NINE_CH)
                            value = (value * 16) + (hc - 48);
                        else
                            break;   
                    }

                    if (i !== 2)
                    {
                        c = LOWER_X_CH;
                    }
                    else
                    {
                        this.advance(2);
                        c = value;
                    }
                }
                else if (c === LOWER_U_CH)
                {
                    // Parse \uXXXX string syntax
                    var value = 0, i = 0;

                    for (; i < 4; ++i)
                    {
                        var hc = this.lookahead_char(i);

                        if (hc >= LOWER_A_CH && hc <= LOWER_F_CH)
                            value = (value * 16) + (hc - 87);
                        else if (hc >= UPPER_A_CH && hc <= UPPER_F_CH)
                            value = (value * 16) + (hc - 55);
                        else if (hc >= ZERO_CH && hc <= NINE_CH)
                            value = (value * 16) + (hc - 48);
                        else
                            break;   
                    }

                    if (i !== 4)
                    {
                        c = LOWER_U_CH;
                    }
                    else
                    {
                        this.advance(4);
                        c = value;
                    }
                }
                chars.push(c);
            }
        }
        else
            chars.push(c);
    }
    var str = String.fromCharCode.apply(null,chars);
    return this.valued_token(STRING_CAT, str, start_pos);
};

Scanner.prototype.parse_regexp = function (pattern)
{
    var flags = [];

    for (var c = this.lookahead_char(0);
         c !== SLASH_CH;
         this.advance(1), c = this.lookahead_char(0))
    {
        if (this.lookahead_char(0) === BACKSLASH_CH &&
            this.lookahead_char(1) === SLASH_CH)
        {
            this.advance(1);
        }
        pattern.push(this.lookahead_char(0));
    }
    this.advance(1);

    for (var c = this.lookahead_char(0);
         this.identifier_class(c) || this.decimal_class(c);
         this.advance(1), c = this.lookahead_char(0))
    {
        flags.push(c);
    }
    return [String.fromCharCode.apply(null, pattern), String.fromCharCode.apply(null, flags)];
}

// method simple_token(cat, n)

Scanner.prototype.simple_token = function (cat, n)
{
    var loc = new Location(this.port.filename,
                           this.lookahead_pos(0),
                           this.lookahead_pos(n));
    this.advance(n);
    return new Token(cat, cat, loc);
};


// method valued_token(cat, value, start_pos)

Scanner.prototype.valued_token = function (cat, value, start_pos)
{
    var loc = new Location(this.port.filename,
                           start_pos,
                           this.lookahead_pos(0));
    return new Token(cat, value, loc);
};

function Token(cat, value, loc)
{
    this.cat   = cat;
    this.value = value;
    this.loc   = loc;
}


Token.prototype.toString = function ()
{
    return this.value.toString();
};


var LINE_SHIFT = 16;


function line_and_column_to_position(line, column)
{
    return line + (column << LINE_SHIFT);
}


function position_to_line(pos)
{
    return (pos & ((1 << LINE_SHIFT) - 1)) + 1;
}


function position_to_column(pos)
{
    return (pos >>> LINE_SHIFT) + 1;
}


function Location(filename, start_pos, end_pos)
{
    this.filename  = filename;
    this.start_pos = start_pos;
    this.end_pos   = end_pos;

    // method join(loc1, loc2)

    this.join = function (loc)
    {
        return new Location(this.filename, this.start_pos, loc.end_pos);
    };

    // method to_string()

    this.to_string = function()
    {
        return "\"" + this.filename + "\"@" +
               position_to_line(this.start_pos) + "." +
               position_to_column(this.start_pos) +
               "-" +
               position_to_line(this.end_pos) + "." +
               position_to_column(this.end_pos);
    };
}


var NUL_CH         =   0;
var BS_CH          =   8;
var TAB_CH         =   9;
var EOL_CH         =  10;
var LF_CH          =  10;
var VT_CH          =  11;
var FF_CH          =  12;
var CR_CH          =  13;
var SPACE_CH       =  32;
var EXCL_CH        =  33;
var DOUBLEQUOTE_CH =  34;
var DOLLAR_CH      =  36;
var PERCENT_CH     =  37;
var AMPERSAND_CH   =  38;
var QUOTE_CH       =  39;
var LPAREN_CH      =  40;
var RPAREN_CH      =  41;
var STAR_CH        =  42;
var PLUS_CH        =  43;
var COMMA_CH       =  44;
var MINUS_CH       =  45;
var PERIOD_CH      =  46;
var SLASH_CH       =  47;
var ZERO_CH        =  48;
var NINE_CH        =  57;
var COLON_CH       =  58;
var SEMICOLON_CH   =  59;
var LT_CH          =  60;
var EQUAL_CH       =  61;
var GT_CH          =  62;
var QUESTION_CH    =  63;
var UPPER_A_CH     =  65;
var UPPER_E_CH     =  69;
var UPPER_F_CH     =  70;
var UPPER_X_CH     =  88;
var UPPER_Z_CH     =  90;
var LBRACK_CH      =  91;
var BACKSLASH_CH   =  92;
var RBRACK_CH      =  93;
var CARET_CH       =  94;
var UNDERSCORE_CH  =  95;
var LOWER_A_CH     =  97;
var LOWER_B_CH     =  98;
var LOWER_E_CH     = 101;
var LOWER_F_CH     = 102;
var LOWER_N_CH     = 110;
var LOWER_R_CH     = 114;
var LOWER_T_CH     = 116;
var LOWER_U_CH     = 117;
var LOWER_V_CH     = 118;
var LOWER_X_CH     = 120;
var LOWER_Z_CH     = 122;
var LBRACE_CH      = 123;
var VBAR_CH        = 124;
var RBRACE_CH      = 125;
var TILDE_CH       = 126;


//-----------------------------------------------------------------------------

// Scanner tables.
//
// *** DO NOT MODIFY THIS SECTION ***
//
// This code was generated by the scripts "yacc2js.scm" and
// "build-keyword-ht.scm".

//START-OF-SCANNER-TABLES
var EOI_CAT = 0;
var error_CAT = 1;
var AUTOSEMICOLON_CAT = 2;
var NULL_CAT = 3;
var TRUE_CAT = 4;
var FALSE_CAT = 5;
var BREAK_CAT = 6;
var CASE_CAT = 7;
var DEFAULT_CAT = 8;
var FOR_CAT = 9;
var NEW_CAT = 10;
var VAR_CAT = 11;
var CONST_CAT = 12;
var CONTINUE_CAT = 13;
var FUNCTION_CAT = 14;
var RETURN_CAT = 15;
var VOID_CAT = 16;
var DELETE_CAT = 17;
var IF_CAT = 18;
var THIS_CAT = 19;
var DO_CAT = 20;
var WHILE_CAT = 21;
var IN_CAT = 22;
var INSTANCEOF_CAT = 23;
var TYPEOF_CAT = 24;
var SWITCH_CAT = 25;
var WITH_CAT = 26;
var RESERVED_CAT = 27;
var THROW_CAT = 28;
var TRY_CAT = 29;
var CATCH_CAT = 30;
var FINALLY_CAT = 31;
var DEBUGGER_CAT = 32;
var ATOMIC_CAT = 33;
var FUTURE_CAT = 34;
var EQEQ_CAT = 35;
var NE_CAT = 36;
var STREQ_CAT = 37;
var STRNEQ_CAT = 38;
var LE_CAT = 39;
var GE_CAT = 40;
var OR_CAT = 41;
var AND_CAT = 42;
var PLUSPLUS_CAT = 43;
var MINUSMINUS_CAT = 44;
var LSHIFT_CAT = 45;
var RSHIFT_CAT = 46;
var URSHIFT_CAT = 47;
var PLUSEQUAL_CAT = 48;
var MINUSEQUAL_CAT = 49;
var MULTEQUAL_CAT = 50;
var DIVEQUAL_CAT = 51;
var LSHIFTEQUAL_CAT = 52;
var RSHIFTEQUAL_CAT = 53;
var URSHIFTEQUAL_CAT = 54;
var BITANDEQUAL_CAT = 55;
var MODEQUAL_CAT = 56;
var BITXOREQUAL_CAT = 57;
var BITOREQUAL_CAT = 58;
var LBRACE_CAT = 59;
var RBRACE_CAT = 60;
var NUMBER_CAT = 61;
var IDENT_CAT = 62;
var STRING_CAT = 63;
var AUTOPLUSPLUS_CAT = 64;
var AUTOMINUSMINUS_CAT = 65;
var CLASS_CAT = 66;
var ENUM_CAT = 67;
var EXPORT_CAT = 68;
var EXTENDS_CAT = 69;
var IMPORT_CAT = 70;
var SUPER_CAT = 71;
var IMPLEMENTS_CAT = 72;
var INTERFACE_CAT = 73;
var LET_CAT = 74;
var PACKAGE_CAT = 75;
var PRIVATE_CAT = 76;
var PROTECTED_CAT = 77;
var PUBLIC_CAT = 78;
var STATIC_CAT = 79;
var YIELD_CAT = 80;
var PLUS_CAT = 81;
var LPAREN_CAT = 82;
var EQUAL_CAT = 83;
var LT_CAT = 84;
var COLON_CAT = 85;
var BITOR_CAT = 86;
var EXCL_CAT = 87;
var LBRACK_CAT = 88;
var RBRACK_CAT = 89;
var DIV_CAT = 90;
var MINUS_CAT = 91;
var COMMA_CAT = 92;
var MULT_CAT = 93;
var RPAREN_CAT = 94;
var GT_CAT = 95;
var BITAND_CAT = 96;
var BITNOT_CAT = 97;
var QUESTION_CAT = 98;
var SEMICOLON_CAT = 99;
var BITXOR_CAT = 100;
var MOD_CAT = 101;
var PERIOD_CAT = 102;
var ELSE_CAT = 103;
var IF_WITHOUT_ELSE_CAT = 104;

var HASH_MOD = 148;
var HASH_MULT = 121;

var keyword_hashtable =
[
 null
,null
,null
,null
,null
,null
,null
,{ id: "future", cat: FUTURE_CAT }
,null
,null
,{ id: "void", cat: VOID_CAT }
,{ id: "null", cat: NULL_CAT }
,null
,null
,{ id: "export", cat: EXPORT_CAT }
,{ id: "yield", cat: YIELD_CAT }
,null
,null
,null
,null
,{ id: "return", cat: RETURN_CAT }
,null
,null
,null
,{ id: "case", cat: CASE_CAT }
,{ id: "while", cat: WHILE_CAT }
,null
,null
,null
,{ id: "debugger", cat: DEBUGGER_CAT }
,{ id: "new", cat: NEW_CAT }
,null
,null
,{ id: "continue", cat: CONTINUE_CAT }
,null
,{ id: "private", cat: PRIVATE_CAT }
,null
,null
,{ id: "class", cat: CLASS_CAT }
,null
,null
,null
,null
,null
,null
,{ id: "var", cat: VAR_CAT }
,null
,{ id: "const", cat: CONST_CAT }
,null
,{ id: "let", cat: LET_CAT }
,null
,null
,null
,{ id: "else", cat: ELSE_CAT }
,null
,null
,null
,null
,null
,{ id: "try", cat: TRY_CAT }
,null
,{ id: "break", cat: BREAK_CAT }
,{ id: "function", cat: FUNCTION_CAT }
,null
,null
,null
,null
,null
,null
,null
,{ id: "switch", cat: SWITCH_CAT }
,{ id: "public", cat: PUBLIC_CAT }
,null
,null
,null
,{ id: "do", cat: DO_CAT }
,null
,null
,null
,{ id: "if", cat: IF_CAT }
,{ id: "with", cat: WITH_CAT }
,null
,null
,{ id: "finally", cat: FINALLY_CAT }
,null
,null
,null
,{ id: "in", cat: IN_CAT }
,null
,{ id: "default", cat: DEFAULT_CAT }
,null
,{ id: "catch", cat: CATCH_CAT }
,{ id: "throw", cat: THROW_CAT }
,null
,{ id: "implements", cat: IMPLEMENTS_CAT }
,{ id: "extends", cat: EXTENDS_CAT }
,{ id: "true", cat: TRUE_CAT }
,null
,{ id: "instanceof", cat: INSTANCEOF_CAT }
,null
,{ id: "this", cat: THIS_CAT }
,null
,null
,null
,null
,{ id: "interface", cat: INTERFACE_CAT }
,null
,{ id: "false", cat: FALSE_CAT }
,null
,null
,null
,null
,null
,null
,null
,null
,null
,{ id: "atomic", cat: ATOMIC_CAT }
,null
,{ id: "import", cat: IMPORT_CAT }
,null
,null
,null
,{ id: "super", cat: SUPER_CAT }
,{ id: "static", cat: STATIC_CAT }
,null
,null
,null
,null
,null
,{ id: "protected", cat: PROTECTED_CAT }
,{ id: "delete", cat: DELETE_CAT }
,{ id: "package", cat: PACKAGE_CAT }
,{ id: "enum", cat: ENUM_CAT }
,null
,null
,null
,null
,null
,{ id: "for", cat: FOR_CAT }
,null
,null
,null
,null
,null
,null
,null
,{ id: "typeof", cat: TYPEOF_CAT }
];
//END-OF-SCANNER-TABLES

//=============================================================================
