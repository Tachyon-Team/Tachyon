//=============================================================================

// File: "scanner.js", Time-stamp: <2010-06-08 20:59:25 feeley>

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
    this.peeked_char      = false;
    this.peeked_char_pos  = false;
    this.pos_window       = [false,false,false,false,false];
    this.char_window      = [false,false,false,false,false];
    this.window_size      = 0;

    // method read_char()

    this.read_char = function ()
    {
        var c = this.port.read_char();
        if (c != EOF)
            this.current_char_pos++;
        return c;
    };

    // method get_char()

    this.get_char = function ()
    {
        var c = this.peeked_char;

        if (c)
            this.peeked_char = false;
        else
            c = this.read_char();

        if (c == LF_CH)
        {
            this.current_line++;
            this.current_line_pos = this.current_char_pos;
            return EOL_CH;
        }
        else if (c == CR_CH)
        {
            this.current_line++;
            this.current_line_pos = this.current_char_pos;
            this.peeked_char_pos = this.current_char_pos;
            var next = this.read_char();
            if (next == LF_CH)
                this.current_line_pos = this.current_char_pos;
            else
                this.peeked_char = next; // remember for next time
            return EOL_CH;
        }
        else
            return c;
    };

    // method advance(i)

    this.advance = function (i)
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

    this.lookahead_pos = function (i)
    {
        this.fill_window(i+1);
        return this.pos_window[i];
    };

    // method lookahead_char(i)

    this.lookahead_char = function (i)
    {
        this.fill_window(i+1);
        return this.char_window[i];
    };

    // method fill_window(n)

    this.fill_window = function (n)
    {
        // fill first n entries of the lookahead window
        var s = this.window_size;
        if (s < n)
        {
            var i = s;
            while (i < n)
            {
                var cp = this.peeked_char
                         ? this.peeked_char_pos
                         : this.current_char_pos;
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

    this.get_token = function ()
    {
        var c = this.lookahead_char(0);

        for (;;)
        {
            if (c == EOF)
                return this.simple_token(EOI_CAT, 0);
            else if (c == SPACE_CH || c == EOL_CH || c == TAB_CH)
            {
                this.advance(1);
                c = this.lookahead_char(0);
            }
            else if (this.identifier_class(c))
                return this.parse_identifier();
            else if (this.decimal_class(c))
                return this.parse_number();
            else if (c == PERIOD_CH)
            {
                if (this.decimal_class(this.lookahead_char(1)))
                    return this.parse_number();
                else
                    return this.simple_token(PERIOD_CAT, 1);
            }
            else if (c == EXCL_CH)
            {
                if (this.lookahead_char(1) == EQUAL_CH)
                {
                    if (this.lookahead_char(2) == EQUAL_CH)
                        return this.simple_token(STRNEQ_CAT, 3);
                    else
                        return this.simple_token(NE_CAT, 2);
                }
                else
                    return this.simple_token(EXCL_CAT, 1);
            }
            else if (c == PERCENT_CH)
            {
                if (this.lookahead_char(1) == EQUAL_CH)
                    return this.simple_token(MODEQUAL_CAT, 2);
                else
                    return this.simple_token(MOD_CAT, 1);
            }
            else if (c == AMPERSAND_CH)
            {
                var x = this.lookahead_char(1);
                if (x == AMPERSAND_CH)
                    return this.simple_token(AND_CAT, 2);
                else if (x == EQUAL_CH)
                    return this.simple_token(BITANDEQUAL_CAT, 2);
                else
                    return this.simple_token(BITAND_CAT, 1);
            }
            else if (c == STAR_CH)
            {
                if (this.lookahead_char(1) == EQUAL_CH)
                    return this.simple_token(MULTEQUAL_CAT, 2);
                else
                    return this.simple_token(MULT_CAT, 1);
            }
            else if (c == PLUS_CH)
            {
                var x = this.lookahead_char(1);
                if (x == PLUS_CH)
                    return this.simple_token(PLUSPLUS_CAT, 2);
                else if (x == EQUAL_CH)
                    return this.simple_token(PLUSEQUAL_CAT, 2);
                else
                    return this.simple_token(PLUS_CAT, 1);
            }
            else if (c == MINUS_CH)
            {
                var x = this.lookahead_char(1);
                if (x == MINUS_CH)
                    return this.simple_token(MINUSMINUS_CAT, 2);
                else if (x == EQUAL_CH)
                    return this.simple_token(MINUSEQUAL_CAT, 2);
                else
                    return this.simple_token(MINUS_CAT, 1);
            }
            else if (c == SLASH_CH)
            {
                var x = this.lookahead_char(1);
                if (x == SLASH_CH)
                {
                    this.advance(2);
                    for (;;)
                    {
                        c = this.lookahead_char(0);
                        if (c == EOL_CH || c == EOF)
                            break;
                        this.advance(1);
                    }
                }
                else if (x == STAR_CH)
                {
                    this.advance(2);
                    for (;;)
                    {
                        c = this.lookahead_char(0);
                        if (c == EOF)
                            error("unterminated comment");
                        if (c == STAR_CH && this.lookahead_char(1) == SLASH_CH)
                            break;
                        this.advance(1);
                    }
                    this.advance(2);
                    c = this.lookahead_char(0);
                }
                else if (x == EQUAL_CH)
                    return this.simple_token(DIVEQUAL_CAT, 2);
                else
                    return this.simple_token(DIV_CAT, 1);
            }
            else if (c == COLON_CH)
                return this.simple_token(COLON_CAT, 1);
            else if (c == EQUAL_CH)
            {
                if (this.lookahead_char(1) == EQUAL_CH)
                {
                    if (this.lookahead_char(2) == EQUAL_CH)
                        return this.simple_token(STREQ_CAT, 3);
                    else
                        return this.simple_token(EQEQ_CAT, 2);
                }
                else
                    return this.simple_token(EQUAL_CAT, 1);
            }
            else if (c == LT_CH)
            {
                var x = this.lookahead_char(1);
                if (x == LT_CH)
                {
                    if (this.lookahead_char(2) == EQUAL_CH)
                        return this.simple_token(LSHIFTEQUAL_CAT, 3);
                    else
                        return this.simple_token(LSHIFT_CAT, 2);
                }
                else if (x == EQUAL_CH)
                    return this.simple_token(LE_CAT, 2);
                else
                    return this.simple_token(LT_CAT, 1);
            }
            else if (c == GT_CH)
            {
                var x = this.lookahead_char(1);
                if (x == GT_CH)
                {
                    var y = this.lookahead_char(2);
                    if (y == GT_CH)
                    {
                        if (this.lookahead_char(3) == EQUAL_CH)
                            return this.simple_token(URSHIFTEQUAL_CAT, 4);
                        else
                            return this.simple_token(URSHIFT_CAT, 3);
                    }
                    else if (y == EQUAL_CH)
                        return this.simple_token(RSHIFTEQUAL_CAT, 3);
                    else
                        return this.simple_token(RSHIFT_CAT, 2);
                }
                else if (x == EQUAL_CH)
                    return this.simple_token(GE_CAT, 2);
                else
                    return this.simple_token(GT_CAT, 1);
            }
            else if (c == QUESTION_CH)
                return this.simple_token(QUESTION_CAT, 1);
            else if (c == CARET_CH)
            {
                if (this.lookahead_char(1) == EQUAL_CH)
                    return this.simple_token(BITXOREQUAL_CAT, 2);
                else
                    return this.simple_token(BITXOR_CAT, 1);
            }
            else if (c == LPAREN_CH)
                return this.simple_token(LPAREN_CAT, 1);
            else if (c == RPAREN_CH)
                return this.simple_token(RPAREN_CAT, 1);
            else if (c == COMMA_CH)
                return this.simple_token(COMMA_CAT, 1);
            else if (c == SEMICOLON_CH)
                return this.simple_token(SEMICOLON_CAT, 1);
            else if (c == LBRACK_CH)
                return this.simple_token(LBRACK_CAT, 1);
            else if (c == VBAR_CH)
            {
                var x = this.lookahead_char(1);
                if (x == VBAR_CH)
                    return this.simple_token(OR_CAT, 2);
                else if (x == EQUAL_CH)
                    return this.simple_token(BITOREQUAL_CAT, 2);
                else
                    return this.simple_token(BITOR_CAT, 1);
            }
            else if (c == RBRACK_CH)
                return this.simple_token(RBRACK_CAT, 1);
            else if (c == LBRACE_CH)
                return this.simple_token(LBRACE_CAT, 1);
            else if (c == RBRACE_CH)
                return this.simple_token(RBRACE_CAT, 1);
            else if (c == TILDE_CH)
                return this.simple_token(BITNOT_CAT, 1);
            else if (c == DOUBLEQUOTE_CH || c == QUOTE_CH)
                return this.parse_string();
            else
                error("unknown token");
        }
    };

    // method identifier_class()

    this.identifier_class = function (c)
    {
        return (c >= LOWER_A_CH && c <= LOWER_Z_CH) ||
               (c >= UPPER_A_CH && c <= UPPER_Z_CH) ||
               c == UNDERSCORE_CH ||
               c == DOLLAR_CH;
    };

    // method decimal_class()

    this.decimal_class = function (c)
    {
        return c >= ZERO_CH && c <= NINE_CH;
    };

    // method parse_identifier()

    this.parse_identifier = function ()
    {
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
        if (x != false && x.id == id)
            return this.valued_token(x.cat, id, start_pos);
        else
            return this.valued_token(IDENT_CAT, id, start_pos);
    };

    // method parse_number()

    this.parse_number = function ()
    {
        var start_pos = this.lookahead_pos(0);
        var n = 0;
        for (;;)
        {
            var c = this.lookahead_char(0);
            if (!this.decimal_class(c))
                break;
            this.advance(1);
            n = n * 10 + (c - ZERO_CH);
        }
        return this.valued_token(NUMBER_CAT, n, start_pos);
    };

    // method parse_string()

    this.parse_string = function ()
    {
        var start_pos = this.lookahead_pos(0);
        var chars = [];
        var close = this.lookahead_char(0);
        this.advance(1);
        for (;;)
        {
            var c = this.lookahead_char(0);
            if (c == EOF)
                error("unterminated string");
            this.advance(1);
            if (c == close)
                break;
            else if (c == BACKSLASH_CH)
            {
                c = this.lookahead_char(0);
                if (c == EOF)
                    error("unterminated string");
                this.advance(1);
                if (c == LOWER_N_CH)
                    c = LF_CH;
                else if (c == ZERO_CH)
                    c = NUL_CH;
                chars.push(c);
            }                    
            else
                chars.push(c);
        }
        var str = String.fromCharCode.apply(null,chars);
        return this.valued_token(STRING_CAT, str, start_pos);
    };

    // method simple_token(cat, n)

    this.simple_token = function (cat, n)
    {
        var loc = new Location(this.port.filename,
                               this.lookahead_pos(0),
                               this.lookahead_pos(n));
        this.advance(n);
        return new Token(cat, cat, loc);
    };

    // method valued_token(cat, value, start_pos)

    this.valued_token = function (cat, value, start_pos)
    {
        var loc = new Location(this.port.filename,
                               start_pos,
                               this.lookahead_pos(0));
        return new Token(cat, value, loc);
    };
}

function Token(cat, value, loc)
{
    this.cat   = cat;
    this.value = value;
    this.loc   = loc;
}

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
var TAB_CH         =   9;
var EOL_CH         =  10;
var LF_CH          =  10;
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
var NULL_CAT = 2;
var TRUE_CAT = 3;
var FALSE_CAT = 4;
var BREAK_CAT = 5;
var CASE_CAT = 6;
var DEFAULT_CAT = 7;
var FOR_CAT = 8;
var NEW_CAT = 9;
var VAR_CAT = 10;
var CONST_CAT = 11;
var CONTINUE_CAT = 12;
var FUNCTION_CAT = 13;
var RETURN_CAT = 14;
var VOID_CAT = 15;
var DELETE_CAT = 16;
var IF_CAT = 17;
var THIS_CAT = 18;
var DO_CAT = 19;
var WHILE_CAT = 20;
var IN_CAT = 21;
var INSTANCEOF_CAT = 22;
var TYPEOF_CAT = 23;
var SWITCH_CAT = 24;
var WITH_CAT = 25;
var RESERVED_CAT = 26;
var THROW_CAT = 27;
var TRY_CAT = 28;
var CATCH_CAT = 29;
var FINALLY_CAT = 30;
var DEBUGGER_CAT = 31;
var EQEQ_CAT = 32;
var NE_CAT = 33;
var STREQ_CAT = 34;
var STRNEQ_CAT = 35;
var LE_CAT = 36;
var GE_CAT = 37;
var OR_CAT = 38;
var AND_CAT = 39;
var PLUSPLUS_CAT = 40;
var MINUSMINUS_CAT = 41;
var LSHIFT_CAT = 42;
var RSHIFT_CAT = 43;
var URSHIFT_CAT = 44;
var PLUSEQUAL_CAT = 45;
var MINUSEQUAL_CAT = 46;
var MULTEQUAL_CAT = 47;
var DIVEQUAL_CAT = 48;
var LSHIFTEQUAL_CAT = 49;
var RSHIFTEQUAL_CAT = 50;
var URSHIFTEQUAL_CAT = 51;
var BITANDEQUAL_CAT = 52;
var MODEQUAL_CAT = 53;
var BITXOREQUAL_CAT = 54;
var BITOREQUAL_CAT = 55;
var LBRACE_CAT = 56;
var RBRACE_CAT = 57;
var NUMBER_CAT = 58;
var IDENT_CAT = 59;
var STRING_CAT = 60;
var AUTOPLUSPLUS_CAT = 61;
var AUTOMINUSMINUS_CAT = 62;
var CLASS_CAT = 63;
var ENUM_CAT = 64;
var EXPORT_CAT = 65;
var EXTENDS_CAT = 66;
var IMPORT_CAT = 67;
var SUPER_CAT = 68;
var IMPLEMENTS_CAT = 69;
var INTERFACE_CAT = 70;
var LET_CAT = 71;
var PACKAGE_CAT = 72;
var PRIVATE_CAT = 73;
var PROTECTED_CAT = 74;
var PUBLIC_CAT = 75;
var STATIC_CAT = 76;
var YIELD_CAT = 77;
var PLUS_CAT = 78;
var LPAREN_CAT = 79;
var EQUAL_CAT = 80;
var LT_CAT = 81;
var COLON_CAT = 82;
var BITOR_CAT = 83;
var EXCL_CAT = 84;
var LBRACK_CAT = 85;
var RBRACK_CAT = 86;
var DIV_CAT = 87;
var MINUS_CAT = 88;
var COMMA_CAT = 89;
var MULT_CAT = 90;
var RPAREN_CAT = 91;
var GT_CAT = 92;
var BITAND_CAT = 93;
var BITNOT_CAT = 94;
var QUESTION_CAT = 95;
var SEMICOLON_CAT = 96;
var BITXOR_CAT = 97;
var MOD_CAT = 98;
var PERIOD_CAT = 99;
var FOOBAR_CAT = 100;
var ELSE_CAT = 101;
var IF_WITHOUT_ELSE_CAT = 102;

var HASH_MOD = 147;
var HASH_MULT = 17;

var keyword_hashtable =
[
 { id: "const", cat: CONST_CAT }
,{ id: "continue", cat: CONTINUE_CAT }
,false
,false
,false
,false
,false
,false
,false
,{ id: "try", cat: TRY_CAT }
,false
,false
,false
,false
,{ id: "finally", cat: FINALLY_CAT }
,false
,false
,false
,false
,{ id: "enum", cat: ENUM_CAT }
,false
,{ id: "for", cat: FOR_CAT }
,false
,false
,{ id: "debugger", cat: DEBUGGER_CAT }
,{ id: "class", cat: CLASS_CAT }
,false
,{ id: "public", cat: PUBLIC_CAT }
,false
,false
,false
,false
,{ id: "switch", cat: SWITCH_CAT }
,false
,false
,false
,false
,false
,{ id: "break", cat: BREAK_CAT }
,{ id: "true", cat: TRUE_CAT }
,false
,false
,{ id: "typeof", cat: TYPEOF_CAT }
,false
,false
,false
,{ id: "this", cat: THIS_CAT }
,{ id: "do", cat: DO_CAT }
,false
,false
,false
,false
,false
,{ id: "throw", cat: THROW_CAT }
,false
,false
,false
,false
,false
,false
,false
,false
,false
,false
,{ id: "implements", cat: IMPLEMENTS_CAT }
,{ id: "case", cat: CASE_CAT }
,false
,false
,false
,{ id: "package", cat: PACKAGE_CAT }
,false
,false
,false
,false
,false
,{ id: "delete", cat: DELETE_CAT }
,false
,false
,{ id: "default", cat: DEFAULT_CAT }
,false
,{ id: "import", cat: IMPORT_CAT }
,{ id: "super", cat: SUPER_CAT }
,false
,{ id: "protected", cat: PROTECTED_CAT }
,{ id: "false", cat: FALSE_CAT }
,false
,false
,false
,{ id: "yield", cat: YIELD_CAT }
,false
,false
,false
,false
,false
,{ id: "null", cat: NULL_CAT }
,{ id: "return", cat: RETURN_CAT }
,false
,false
,false
,false
,false
,false
,false
,false
,{ id: "while", cat: WHILE_CAT }
,false
,false
,false
,false
,{ id: "with", cat: WITH_CAT }
,{ id: "new", cat: NEW_CAT }
,false
,false
,false
,false
,{ id: "private", cat: PRIVATE_CAT }
,false
,{ id: "let", cat: LET_CAT }
,false
,false
,{ id: "void", cat: VOID_CAT }
,{ id: "function", cat: FUNCTION_CAT }
,false
,{ id: "if", cat: IF_CAT }
,false
,{ id: "export", cat: EXPORT_CAT }
,false
,false
,false
,false
,false
,{ id: "in", cat: IN_CAT }
,false
,{ id: "interface", cat: INTERFACE_CAT }
,{ id: "else", cat: ELSE_CAT }
,{ id: "instanceof", cat: INSTANCEOF_CAT }
,false
,false
,false
,false
,false
,{ id: "catch", cat: CATCH_CAT }
,false
,false
,{ id: "var", cat: VAR_CAT }
,{ id: "extends", cat: EXTENDS_CAT }
,{ id: "static", cat: STATIC_CAT }
];
//END-OF-SCANNER-TABLES

//=============================================================================
