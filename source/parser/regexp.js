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

function RegExpParser() {}

RegExpParser.prototype.parse = function (pattern)
{
    this.pattern = pattern;
    this.curc = pattern.charCodeAt(0);
    this.index = 0;

    this.tree = this.parseDisjunction(false, 0);
    return this.tree;
}

RegExpParser.prototype.current = function ()
{
    return this.curc;
} 

RegExpParser.prototype.advance = function ()
{
    if (this.index < this.pattern.length - 1)
    {
        this.curc = this.pattern.charCodeAt( ++(this.index) );
    }
    else
    {
        this.curc = null;
    }
}

function genLevel (level)
{
    var s = "";

    for (var i = 0; i < level; i++)
    {
        s += "=";
    }

    if (level > 0)
    {
        s += " ";
    }
    return s;
}

/**
Disjunction ::
  Alternative
  Alternative | Disjunction

@param sub : true if the disjunction is a group (sub-expression), false otherwise.
@param type : 0 :: basic disjunction
              1 :: positive lookahead
              2 :: negative lookahead
              3 :: grouping (????)
*/
function RegExpDisjunction(sub, type)
{
    this.alternatives = [];
    this.sub = sub;
    this.type = type;
}

RegExpDisjunction.prototype.pp = function (level)
{
    if (level == undefined)
        level = 0;

    var s = genLevel(level);
    s += "Disjunction";
    print(s);

    for (var i = 0; i < this.alternatives.length; ++i)
        this.alternatives[i].pp(level + 1);
}

RegExpParser.prototype.parseDisjunction = function(sub, type)
{
    var node = new RegExpDisjunction(sub, type);

    while (true)
    {
        switch (this.current())
        {
            case 41: // ')'
                this.advance();
                if (!node.sub)
                    print("unmatched )");
            case null: // EOL
                return node;

            case 124: // '|'
                this.advance();
            default:
                node.alternatives.push( this.parseAlternative() );
                break;
        }
    }
}

/**
Alternative ::
  [empty]
  Alternative Term
*/

function RegExpAlternative()
{
    this.terms = [];
}

RegExpAlternative.prototype.pp = function(level)
{
    var s = genLevel(level);
    s += "Alternative";
    print(s);

    for (var i = 0; i < this.terms.length; i++)
        this.terms[i].pp(level + 1);
}

RegExpParser.prototype.parseAlternative = function()
{
    var node = new RegExpAlternative();

    while (true)
    {
        switch (this.current())
        {
        case null: // EOL
        case 124: // '|'
        case 41: // ')'
            return node;

        default:
            node.terms.push( this.parseTerm() );
            break;
        }
    }
}

/**
Term ::
  Assertion
  Atom
  Atom Quantifier
*/

function RegExpTerm() {}

RegExpTerm.prototype.pp = function (level)
{
    var s = genLevel(level);
    s += "Term";
    print(s);

    if (this.prefix != undefined)
        this.prefix.pp(level + 1);
    if (this.quantifier != undefined)
        this.quantifier.pp(level + 1);
}

RegExpParser.prototype.parseTerm = function ()
{
    var node = new RegExpTerm();

    switch (this.current())
    {
    case null: // EOL
       return node; 

    // Assertion parsing.
    case 94: // '^'
    case 36: // '$'
        node.prefix = new RegExpAssertion( this.current() );
        this.advance();
        return node;

    // Sub-disjunction
    case 40: // '('
        this.advance();
        if (this.current() == 63) { // '?'
            this.advance();
            if (this.current() == 61) { // '='
                node.prefix = new RegExpAssertion( this.parseDisjunction(true, 1) );
            } else if (this.current() == 33) { // '!'
                node.prefix = new RegExpAssertion( this.parseDisjunction(true), 2 );
            } else if (this.current() == 58) { // ':'
                node.prefix = new RegExpAtom( this.parseDisjunction(true), 3 );
            } else {
                // TODO: Invalid group error handling.
            }
        } else {
            node.prefix = this.parseDisjunction(true, 0);
        }
        break;

    // Escaped sequence
    case 92: // '\'
        this.advance();
        // \b and \B are treated as an assertion
        if (this.current() == 98 || this.current() == 66) { // 'b' | 'B'
            node.prefix = new RegExpAssertion( this.current() );
            this.advance();
        } else {
            node.prefix = new RegExpAtom( this.parseAtomEscape() );
        }
        // TODO: proper escape character handling
        break;

    // Atom
    case 46: // '.'
        node.prefix = new RegExpAtom( this.current() );
        this.advance();
        break;

    case 41: // ')'
        return node;

    // CharacterClass
    case 91: // '['
        node.prefix = this.parseCharacterClass();
        break;

    case 42: // '*'
    case 43: // '+'
    case 63: // '?'
    case 123: // '{'
    case 125: // '}'
    case 93: // ']'
    case 93: // '|'
        break;

    // PatternCharacter
    default:
        node.prefix = new RegExpAtom( new RegExpPatternCharacter( this.current() ) );
        this.advance();
        break;
    }

    // Quantifier reading.
    switch (this.current())
    {
        // Quantifier
        case 42: // '*'
        case 43: // '+'
        case 63: // '?'
        case 123: // '{'
            if (node.prefix == undefined || node.prefix instanceof RegExpAssertion) {
                print("invalid quantifier without atom");
            } else {
                node.quantifier = this.parseQuantifier();
            }
            break;
    }

    return node;
}

/**
Assertion ::
  ^
  $
  \b
  \B
  (?= Disjunction)
  (?! Disjunction)
*/
function RegExpAssertion(value)
{
    this.value = value;
}

RegExpAssertion.prototype.pp = function (level)
{
    var s = genLevel(level);
    s += "Assertion" ;

    if (this.value instanceof RegExpDisjunction) {
        print(s);
        this.value.pp(level + 2);
    } else {
        str += " : " + this.value;
        print(s);
    }
}

/**
Quantifier ::
  QuantifierPrefix
  QuantifierPrefix ?

QuantifierPrefix ::
  *
  +
  ?
  { DecimalDigits }
  { DecimalDigits , }
  { DecimalDigits , DecimalDigits }
*/

function RegExpQuantifier(value)
{
    this.value = value;
}

RegExpQuantifier.prototype.pp = function(level)
{
    var s = genLevel(level);
    s += "Quantifier (min " + this.min + ", max " + (this.max == -1 ? "inf" : this.max) + ")";
    print(s);
}

RegExpParser.prototype.parseQuantifier = function ()
{
    var node = new RegExpQuantifier();
    node.greedy = true;

    switch (this.current())
    {
        case 42: // '*'
        node.min = 0;
        node.max = -1;
        this.advance();
        break;

        case 43: // '+'
        node.min = 1;
        node.max = -1;
        this.advance();
        break;

        case 63: // '?'
        node.min = 0;
        node.max = 0;
        this.advance();
        break;

        case 123: // '{'
        this.advance();
        // Parse min limit.
        if (this.current() >= 48 && this.current() <= 57) { // 0-9
            node.min = this.parseDecimalDigit();
        } else {
            print("ill formed quantifier");
        }

        if (this.current() == 44) { // ','
            this.advance();
            if (this.current() >= 48 && this.current() <= 57) {
                node.max = this.parseDecimalDigit();
            } else {
                node.max = -1; // infinity
            }
        } 
        // Should be closing }
        if (this.current() == 125) {
            this.advance();
        } else {
            print("ill formed quantifier");
        }
        break;
    }

    // Is the quantifier non greedy ?
    if (this.current() == 63) { // '?'
        node.greedy = false;
        this.advance();
    }

    return node;
}

/**
Atom ::
  PatternCharacter
  .
  \ AtomEscape
  CharacterClass
  ( Disjunction )
  (?: Disjunction )
*/
function RegExpAtom(value)
{
    this.value = value;
}

RegExpAtom.prototype.pp = function(level)
{
    var str = "";

    if (level == undefined)
        level = 0;

    for (var i = 0; i < level; i++)
        str += "=";
    str += " Atom ";

    if (this.value instanceof RegExpAtomEscape)
    {
        str += this.value.value.value;
    }
    else if (this.value instanceof RegExpPatternCharacter)
    {
        str += this.value.value;
    }

    print(str);

    if (this.value instanceof RegExpCharacterClass)
    {
        this.prefix.pp();   
    }
}

/**
PatternCharacter
*/
function RegExpPatternCharacter(value)
{
    this.value = value;
}

RegExpPatternCharacter.prototype.pp = function(level)
{
    var str = "";

    if (level == undefined)
        level = 0;

    for (var i = 0; i < level; i++)
        str += "-";
    str += " PatternCharacter : " + this.value;
    print(str);
}

/**
AtomEscape ::
  DecimalEscape
  CharacterEscape
  CharacterClassEscape
*/

function RegExpAtomEscape() {}

/**
DecimalEscape
*/
function RegExpDecimalEscape(value)
{
    this.value = value;
}

/**
CharacterEscape ::
  ControlEscape
  c ControlLetter
  HexEscapeSequence
  UnicodeEscapeSequence
  IdentityEscapeSequence
*/
function RegExpCharacterEscape()
{
}

function RegExpCharacterClassEscape(value)
{
    this.value = value;
}

function RegExpControlSequence(value)
{
    this.value = value;
}

function RegExpHexSequence(value)
{
    this.value = value;
}

function RegExpUnicodeSequence(value)
{
    this.value = value;
}

RegExpParser.prototype.parseAtomEscape = function ()
{
    var node = new RegExpAtomEscape();

    if (this.current() >= 48 && this.current() <= 57) {
        node.value = new RegExpDecimalEscape( this.parseDecimalDigit() );
    } else {
        switch (this.current())
        {
            case 100: // 'd'
            case 68: // 'd'
            case 115: // 's'
            case 83: // 'S'
            case 119: // 'w'
            case 87: // 'W'
                node.value = new RegExpCharacterClassEscape( this.current() );
                break;

            case 99: // 'c'
                // Parse control sequence.
                this.advance();
                if ((this.current() >= 65 && this.current() <= 90) || // A-Z
                    (this.current() >= 97 && this.current() <= 122)) // a-z
                {
                    node.value = new RegExpControlSequence( this.current() );
                } 
                else
                {
                    print("invalid control sequence");
                }
                return node;

            case 120: // 'x'
                // Parse hexadecimal sequence.
                this.advance();
                node.value = new RegExpHexSequence( this.parseHexadecimalSequence(2) );

            case 117: // 'u'
                // Parse unicode hexadecimal sequence.
                this.advance();
                node.value = new RegExpUnicodeSequence( this.parseHexadecimalSequence(4) );

            default:
                node.value = new RegExpPatternCharacter( this.current() );
                this.advance();
        }
    }

    return node;
}

RegExpParser.prototype.parseHexadecimalSequence = function (size)
{
    var value = 0;

    while (size-- > 0)
    {
        if (this.current() >= 48 && this.current() <= 57) // 0-9
        {
           value = value * 16 + (this.current() - 48); 
        }
        else if (this.current() >= 65 && this.current() <= 70) // A-F
        {
           value = value * 16 + (this.current() - 55); 
        }
        else if (this.current() >= 97 && this.current() <= 102) // a-f
        {
           value = value * 16 + (this.current() - 87); 
        }
        else
        {
            print("invalid hexadecimal sequence");
        }
        this.advance();
    }

    return value;
}

function RegExpCharacterClass()
{
    this.classAtoms = [];
}

RegExpCharacterClass.prototype.pp = function (level)
{
    var s = genLevel(level);

    s += "CharacterClass";
    print(s);

    for (var i = 0; i < this.classAtoms.length; ++i)
        this.classAtoms[i].pp();
}

RegExpParser.prototype.parseCharacterClass = function ()
{
    var node = new RegExpCharacterClass();

    while (true)
    {
        switch (this.current())
        {
            case 91: // '['
                this.advance();
                break;

            case 93: // ']'
                this.advance();
                return node;

            default:
                node.classAtoms.push( this.parseClassAtom() );
                break;
        }
    }

    return node;
}

function RegExpClassAtom()
{
}

RegExpClassAtom.prototype.pp = function ()
{

}

RegExpParser.prototype.parseClassAtom = function ()
{
    var node = new RegExpClassAtom();

    switch (this.current())
    {
        case 45: // '\'
            this.advance();
            node.min = this.parseAtomEscape();
            break;

        case 93: // ']'
            return node;

        default:
            node.min = this.current();
            this.advance();
            break;
    }

    return node;
}


function RegExpDecimalDigit()
{
}

RegExpParser.prototype.parseDecimalDigit = function()
{
    var node = new RegExpDecimalDigit();
    node.value = 0;

    while (this.current() >= 48 && this.current() <= 57)
    {
       node.value = (node.value * 10) + this.current() - 48; 
       this.advance();
    }
    return node;
}

