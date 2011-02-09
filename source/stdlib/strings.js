/**
@fileOverview
Implementation of ECMAScript 5 string string routines.

@author
Bruno Dufour, Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
@class 15.5.2 String constructor
new String(value)
String(value)
*/
function String(value)
{
    // If this is a constructor call (new String)
    if (isGlobalObj(this) === false)
    {
        // Convert the value to a string
        var strVal = boxToString(value);

        // Store the value in the new object
        // TODO: this should be a hidden/internal property
        this.value = strVal;
    }
    else
    {
        // Convert the value to a string
        return boxToString(value);
    }
}

/**
15.5.3.1 String prototype object
*/
String.prototype = {};

/**
Anonymous function to initialize this library
*/
(function ()
{
    // Get a reference to the context
    var ctx = iir.get_ctx();

    // Set the String prototype object in the context
    set_ctx_strproto(ctx, String.prototype);
})();

//-----------------------------------------------------------------------------

// Operations on String objects.

function string_internal_charCodeAt(s, pos)
{
    var idx = unboxInt(pos);

    var ch = get_str_data(s, idx);

    return boxInt(iir.icast(IRType.pint, ch));
}

function string_internal_getLength(s)
{
    var strLen = iir.icast(IRType.pint, get_str_len(s));

    return boxInt(strLen);
}

function string_internal_toCharCodeArray(x)
{
    // TODO: implement this in IIR?
    var s = x.toString();
    var a = new Array(s.length);

    var i;
    for (i = 0; i < s.length; i++)
    {
        a[i] = string_internal_charCodeAt(s, i); // Using built-in function
    }

    return a;
}

function string_internal_fromCharCodeArray(a)
{
    // TODO: implement this in IIR
    return String.fromCharCode.apply(null, a);
}

function string_internal_toString(s)
{
    return s;
}

function string_toString()
{
    return string_internal_toString(this);
}

function string_valueOf()
{
    return string_internal_toString(this);
}

function string_fromCharCode()
{
    var args = Array.prototype.slice.call(arguments, 0);
    return string_internal_fromCharCodeArray(args);
}

function string_charCodeAt(pos)
{
    var len = string_internal_getLength(this);

    if (pos >= 0 && pos < len)
    {
        return string_internal_charCodeAt(this, pos);
    }

    // FIXME: return NaN when doubles are implemented
    // error("Invalid string index in 'charCodeAt'")
    return -1;
}

function string_charAt(pos)
{
    if (pos < 0 || pos >= string_internal_getLength(this))
    {
        return string_internal_fromCharCodeArray([]);
    }

    var ch = this.string_charCodeAt(pos);
    return string_internal_fromCharCodeArray([ch]);
    // return this.string_substring(pos, pos+1);
}

function string_concat()
{
    var a = string_internal_toCharCodeArray(this);
    for (var i in arguments)
    {
        var arg = arguments[i];
        a = a.concat(string_internal_toCharCodeArray(arg));
    }

    return string_internal_fromCharCodeArray(a);
}

function string_indexOf(searchString, pos)
{
    var search = string_internal_toCharCodeArray(searchString);
    var searchLen = search.length;

    var a = string_internal_toCharCodeArray(this);
    var len = a.length;

    if (pos === undefined || pos < 0) pos = 0;
    if (searchLen > len) return -1;

    var start = pos;
    if (start > len) start = len;

    if (searchLen === 0) return start;
    
    var end = a.length - searchLen;
    var firstChar = search[0];
    for (var i = pos; i <= end; i++)
    {
        if (a[i] === firstChar)
        {
            var match = true;
            for (var j = 1; j < searchLen; j++)
            {
                if (a[i + j] !== search[j])
                {
                    match = false;
                    break;
                }
            }
            if (match) return i;
        }
    }

    return -1;
}

function string_lastIndexOf(searchString, pos)
{
    var search = string_internal_toCharCodeArray(searchString);
    var searchLen = search.length;

    var a = string_internal_toCharCodeArray(this);
    var len = a.length;

    if (searchLen > len) return -1;

    if (pos === undefined) pos = len;
    else if (pos >= len) pos = len;
    else if (pos < 0) pos = 0;

    if (searchLen === 0) return pos;

    if (pos + searchLen > len) pos = len - searchLen;
    
    var firstChar = search[0];
    for (var i = pos; i >= 0; i--)
    {
        if (a[i] === firstChar)
        {
            var match = true;
            for (var j = 1; j < searchLen; j++)
            {
                if (a[i + j] !== search[j])
                {
                    match = false;
                    break;
                }
            }
            if (match) return i;
        }
    }

    return -1;
}

function string_localeCompare(that)
{
    var first = string_internal_toCharCodeArray(this);
    var second = string_internal_toCharCodeArray(that);
    
    var len = first.length;
    if (second.length < len) len = second.length;

    var i;
    for (i = 0; i < len; i++)
    {
        var a = first[i];
        var b = second[i];
        if (a !== b)
        {
            return a - b;
        }
    }

    if (first.length > len)
    {
        return 1;
    }
    else if (second.length > len)
    {
        return -1;
    }
    else
    {
        return 0;
    }
}

function string_slice(start, end)
{
    var a = string_internal_toCharCodeArray(this);
    return string_internal_fromCharCodeArray(a.slice(start, end));
}

function string_match(regexp)
{
    error("Regular expressions are not supported at the moment");
}

function string_replace(searchValue, replaceValue)
{
    // FIXME: support function as replaceValue
    // FIXME: support regexp
    var pos = this.string_indexOf(searchValue);
    if (pos >= 0)
    {
        return this.string_substring(0, pos).concat(
                replaceValue.toString(),
                this.string_substring(pos + string_internal_getLength(searchValue)));
    }

    return this.toString();
}

function string_search(regexp)
{
    error("Regular expressions are not supported at the moment");
}

function string_split(separator, limit)
{
    var res = new Array();
    if (limit === 0) return res;

    var len = string_internal_getLength(this);
    if (len === 0) return res;

    var pos = this.string_indexOf(separator);
    var start = 0;
    var sepLen = string_internal_getLength(separator);

    while (pos >= 0)
    {
        res.push(this.string_substring(start, pos));
        if (res.length === limit) return res;
        start = pos + sepLen;
        pos = this.string_indexOf(separator, pos + sepLen);
    }

    if (start <= len)
    {
        res.push(this.substring(start));
    }

    return res;
}

function string_substring(start, end)
{
    var a = string_internal_toCharCodeArray(this);
    var len = a.length;

    var from = start;
    var to = end;

    if (from < 0) from = 0;
    else if (from > len) from = len;

    if (to === undefined) to = len;
    else if (to < 0) to = 0;
    else if (to > len) to = len;

    if (from > to)
    {
        // Swap 'from' and 'to'
        var t = to;
        to = from;
        from = t;
    }

    return string_internal_fromCharCodeArray(a.slice(from, to));
}

function string_toLowerCase()
{
    var a = string_internal_toCharCodeArray(this);

    // This code assumes the array is a copy of the internal char array.
    // It may be more efficient to expose the internal data directly and
    // make a copy only when necessary.
    
    for (var i = 0; i < a.length; i++)
    {
        var c = a[i];
        // FIXME: support full Unicode
        if (c > 255) error("Only ASCII characters are currently supported");

        if ((c >= 65 && c <= 90)
                || (c >= 192 && c <= 214)
                || (c >= 216 && c <= 222))
        {
            a[i] = c + 32;
        }
    }

    return string_internal_fromCharCodeArray(a);
}

function string_toLocaleLowerCase()
{
    // FIXME: not quire correct for the full Unicode
    return this.string_toLowerCase();
}

function string_toUpperCase()
{
    var a = string_internal_toCharCodeArray(this);

    // This code assumes the array is a copy of the internal char array.
    // It may be more efficient to expose the internal data directly and
    // make a copy only when necessary.
    
    for (var i = 0; i < a.length; i++)
    {
        var c = a[i];
        // FIXME: support full Unicode
        if (c > 255) error("Only ASCII characters are currently supported");

        if ((c >= 97 && c <= 122)
                || (c >= 224 && c <= 246)
                || (c >= 248 && c <= 254))
        {
            a[i] = c - 32;
        }
    }

    return string_internal_fromCharCodeArray(a);
}

function string_toLocaleUpperCase()
{
    // FIXME: not quire correct for the full Unicode
    return this.string_toUpperCase();
}

function string_internal_isWhiteSpace(c)
{
    // FIXME: add support for other Unicode characters
    return (c >= 9 && c <= 13) || (c === 32) || (c === 160);
}

function string_trim()
{
    var a = string_internal_toCharCodeArray(this);
    var len = a.length;
    var i;

    var from = 0;
    var to = len;

    for (i = 0; i < len; i++)
    {
        var c = a[i];

        if (!string_internal_isWhiteSpace(c))        
        {
            from = i;
            break;
        }
    }

    for (i = len - 1; i >= 0; i--)
    {
        var c = a[i];
        if (!string_internal_isWhiteSpace(c))        
        {
            to = i + 1;
            break;
        }
    }

    return string_internal_fromCharCodeArray(a.slice(from, to));
}

// Setup string methods
String.fromCharCode = string_fromCharCode;

// Setup String.prototype
String.prototype.toString = string_toString;
String.prototype.charCodeAt = string_charCodeAt;
String.prototype.valueOf = string_valueOf;
String.prototype.charAt = string_charAt;
String.prototype.charCodeAt = string_charCodeAt;
String.prototype.concat = string_concat;
String.prototype.indexOf = string_indexOf;
String.prototype.lastIndexOf = string_lastIndexOf;
String.prototype.localeCompare = string_localeCompare;
String.prototype.slice = string_slice;
String.prototype.match = string_match;
String.prototype.replace = string_replace;
String.prototype.search = string_search;
String.prototype.split = string_split;
String.prototype.substring = string_substring;
String.prototype.toLowerCase = string_toLowerCase;
String.prototype.toLocaleLowerCase = string_toLocaleLowerCase;
String.prototype.toUpperCase = string_toUpperCase;
String.prototype.toLocaleUpperCase = string_toLocaleUpperCase;
String.prototype.internal_isWhiteSpace = string_internal_isWhiteSpace;
String.prototype.trim = string_trim;

//-----------------------------------------------------------------------------

/*
function fail(expected, actual, f)
{
    var msg;
    if (f !== undefined)
    {
        msg = "in " + f + ": ";
    }
    else
    {
        msg = "";
    }

    if (typeof expected !== typeof actual)
    {
        msg = msg + "(type mismatch) ";
    }

    msg = msg + "expected '" + expected + "', got '" + actual + "'";
    print(msg);
}

function assertEq(expected, actual, f)
{
    if (expected !== actual) fail(expected, actual, f);
}

function assertArrayEq(expected, actual, f)
{
    if (expected.length === actual.length)
    {
        for (var i = 0; i < expected.length; i++)
        {
            if (expected[i] !== actual[i])
            {
                fail(expected, actual, f);
                break;
            }
        }
    }
}

function assertSignEq(expected, actual, f)
{
    if ((expected === 0 && actual === 0)
            || (expected < 0 && actual < 0)
            || (expected > 0 && actual > 0))
        return;

    fail(expected, actual, f);
}

// Check correctness.

function check_internal_toCharCodeArray(s, a)
{
    assertArrayEq(a, string_internal_toCharCodeArray(s), "toCharCodeArray");
}

check_internal_toCharCodeArray("", []);
check_internal_toCharCodeArray("a", [97]);
check_internal_toCharCodeArray("ab", [97,98]);
check_internal_toCharCodeArray("abc", [97,98,99]);

function check_internal_fromCharCodeArray(s)
{
    var a = string_internal_toCharCodeArray(s);
    assertEq(s.length, a.length);
    assertEq(s, string_internal_fromCharCodeArray(a), "fromCharCodeArray");
}

check_internal_fromCharCodeArray("");
check_internal_fromCharCodeArray("a");
check_internal_fromCharCodeArray("ab");
check_internal_fromCharCodeArray("abc");
check_internal_fromCharCodeArray("abc def");
check_internal_fromCharCodeArray("abc\ndef");

function check_toString(s)
{
    assertEq(s.toString(), s.string_toString(), "toString");
}

check_toString("");
check_toString("a");
check_toString("ab");
check_toString("abc");
check_toString("abc def");
check_valueOf("abc\ndef");

function check_valueOf(s)
{
    assertEq(s.valueOf(), s.string_valueOf(), "valueOf"); 
}

check_valueOf("");
check_valueOf("a");
check_valueOf("ab");
check_valueOf("abc");
check_valueOf("abc def");
check_valueOf("abc\ndef");

function check_fromCharCode(arr)
{
    var s1 = String.fromCharCode.apply(arr);
    var s2 = String.string_fromCharCode.apply(arr);

    assertEq(s1, s2, "fromCharCode");
}

check_fromCharCode([]);
check_fromCharCode([97]);
check_fromCharCode([97,98]);
check_fromCharCode([97, 98, 99, 32, 100, 101, 10, 102]);

function check_charCodeAt(s)
{
    for (var i = -1; i <= s.length + 1; i++)
    {
        var res1 = s.charCodeAt(i);
        var res2 = s.string_charCodeAt(i);

        if (isNaN(res1)) res1 = -1;

        assertEq(res1, res2, "charCodeAt");
    }
}

check_charCodeAt("");
check_charCodeAt("a");
check_charCodeAt("ab");
check_charCodeAt("abc");
check_charCodeAt("abc def");
check_charCodeAt("abc\ndef");

function check_charAt(s)
{
    for (var i = -1; i <= s.length + 1; i++)
    {
        assertEq(s.charAt(i), s.string_charAt(i), "charAt");
    }
}

check_charAt("");
check_charAt("a");
check_charAt("ab");
check_charAt("abc");
check_charAt("abc def");
check_charAt("abc def");
check_charAt("abc\ndef");

function check_concat(s1, s2, s3, s4)
{
    if (s4 !== undefined)
    {
        assertEq(s1.concat(s2, s3, s4), s1.concat(s2, s3, s4), "concat");
    }
    else if (s3 !== undefined)
    {
        assertEq(s1.concat(s2, s3), s1.concat(s2, s3), "concat");
    }
    else if (s2 !== undefined)
    {
        assertEq(s1.concat(s2), s1.concat(s2), "concat");
    }
    else
    {
        assertEq(s1.concat(), s1.concat(), "concat");
    }
}

check_concat("");
check_concat("a");
check_concat("ab");
check_concat("", "ab");
check_concat("ab", "");
check_concat("ab", "cd");
check_concat("ab", "", "cd");
check_concat("ab", "cd", "");
check_concat("ab", "cd", "de");
check_concat("ab", "cd", "de", "fg");
check_concat("ab\n", "cd\n", "de\n", "fg");

function check_indexOf(s, w)
{
    assertEq(s.indexOf(w), s.string_indexOf(w), "indexOf");
    for (var i = -1; i <= s.length + 1; i++)
    {
        assertEq(s.indexOf(w, i), s.string_indexOf(w, i), "indexOf");
    }
}

check_indexOf("", "");
check_indexOf("", "a");
check_indexOf("a", "");
check_indexOf("a", "a");
check_indexOf("a", "b");
check_indexOf("ab", "ab");
check_indexOf("ab", "ba");
check_indexOf("abcdef", "ab");
check_indexOf("abcdef", "bc");
check_indexOf("abcdef", "cd");
check_indexOf("abcdef", "de");
check_indexOf("abcdef", "ef");
check_indexOf("abcdef", "bcde");
check_indexOf("abcdef", "defg");
check_indexOf("aaaaaa", "a");
check_indexOf("ababaab", "ab");

function check_lastIndexOf(s, w)
{
    assertEq(s.lastIndexOf(w), s.string_lastIndexOf(w), "lastIndexOf");
    for (var i = -1; i <= s.length + 1; i++)
    {
        assertEq(s.lastIndexOf(w, i), s.string_lastIndexOf(w, i), "lastIndexOf");
    }
}

check_lastIndexOf("", "");
check_lastIndexOf("", "a");
check_lastIndexOf("a", "");
check_lastIndexOf("a", "a");
check_lastIndexOf("a", "b");
check_lastIndexOf("ab", "ab");
check_lastIndexOf("ab", "ba");
check_lastIndexOf("abcdef", "ab");
check_lastIndexOf("abcdef", "bc");
check_lastIndexOf("abcdef", "cd");
check_lastIndexOf("abcdef", "de");
check_lastIndexOf("abcdef", "ef");
check_lastIndexOf("abcdef", "bcde");
check_lastIndexOf("abcdef", "defg");

function check_localeCompare(s1, s2)
{
    var s1_copy = s1.slice(0);
    var s2_copy = s2.slice(0);

    var res1 = s1.localeCompare(s2);
    var res2 = s1_copy.string_localeCompare(s2_copy);
    assertSignEq(res1, res2, "localeCompare");
}

check_localeCompare("", "");
check_localeCompare("a", "a");
check_localeCompare("ab", "ab");
check_localeCompare("ab", "bc");
check_localeCompare("cd", "ab");
check_localeCompare("", "ab");
check_localeCompare("ab", "");

function check_slice(s)
{
    assertEq(s.slice(), s.string_slice(), "slice");
    for (var i = -1; i <= s.length + 1; i++)
    {
        assertEq(s.slice(i), s.string_slice(i));
        for (var j = -1; j <= s.length + 1; j++)
        {
            assertEq(s.slice(i, j), s.string_slice(i, j), "slice");
        }
    }
}

check_slice("");
check_slice("a");
check_slice("ab");
check_slice("abc");
check_slice("abc def");

function check_match()
{
    // TODO: implement
}

function check_replace(s, pat, repl)
{
    assertEq(s.replace(pat, repl), s.string_replace(pat, repl));
}

check_replace("", "", "");
check_replace("a", "", "");
check_replace("", "a", "");
check_replace("", "", "a");
check_replace("a", "a", "b");
check_replace("a", "c", "b");
check_replace("abc", "b", "xYz");
check_replace("abcdef", "bc", "xYz");
check_replace("abcdef", "bc", "xYz");

function check_search()
{
    // TODO: implement
}

function check_split(s, sep)
{
    assertArrayEq(s.split(s, sep), s.string_split(s, sep), "split");
    for (var i = 0; i <= s.length + 1; i++)
    {
        assertArrayEq(s.split(s, sep, i), s.string_split(s, sep, i), "split");
    }
}

check_split("", "");
check_split("a", "");
check_split("ab", "");
check_split("abc", "");
check_split("a", "a");
check_split("ab", "b");
check_split("ab", "c");
check_split("ab", "ab");
check_split("aaaaaa", "a");
check_split("ababab", "ab");
check_split("", "ab");

function check_substring(s, from, to)
{
    assertEq(s.substring(), s.string_substring(), "substring");
    for (var i = -1; i <= s.length + 1; i++)
    {
        assertEq(s.substring(i), s.string_substring(i));
        for (var j = -1; j <= s.length + 1; j++)
        {
            assertEq(s.substring(i, j), s.string_substring(i, j), "substring");
        }
    }
}

check_substring("");
check_substring("a");
check_substring("ab");
check_substring("abc def");

function check_toLowerCase(s)
{
    var s1 = s.slice(0);
    var s2 = s.slice(0);

    assertEq(s1.toLowerCase(), s2.string_toLowerCase(), "toLowerCase");
}

check_toLowerCase("");
check_toLowerCase("abc\ndef");
check_toLowerCase("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
check_toLowerCase("àâéèêîôùûÀÂÉÈÊÎÔÙÛ");

function check_toLocaleLowerCase(s)
{
    var s1 = s.slice(0);
    var s2 = s.slice(0);

    assertEq(s1.toLocaleLowerCase(), s2.string_toLocaleLowerCase(), "toLocaleLowerCase");
}

check_toLocaleLowerCase("");
check_toLocaleLowerCase("AbC\nDeF");
check_toLocaleLowerCase("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
check_toLocaleLowerCase("àâéèêîôùûÀÂÉÈÊÎÔÙÛ");

function check_toUpperCase(s)
{
    var s1 = s.slice(0);
    var s2 = s.slice(0);

    assertEq(s1.toUpperCase(), s2.string_toUpperCase(), "toUpperCase");
}

check_toUpperCase("");
check_toUpperCase("AbC\nDeF");
check_toUpperCase("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
check_toUpperCase("àâéèêîôùûÀÂÉÈÊÎÔÙÛ");

function check_toLocaleUpperCase(s)
{
    var s1 = s.slice(0);
    var s2 = s.slice(0);

    assertEq(s1.toLocaleUpperCase(), s2.string_toLocaleUpperCase(), "toLocaleUpperCase");
}

check_toLocaleUpperCase("");
check_toLocaleUpperCase("AbC\nDeF");
check_toLocaleUpperCase("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
check_toLocaleUpperCase("àâéèêîôùûÀÂÉÈÊÎÔÙÛ");

function check_trim(s)
{
    var s1 = s.slice(0);
    var s2 = s.slice(0);

    assertEq(s1.trim(), s2.string_trim(), "trim");
}

check_trim("");
check_trim("a");
check_trim("abc");
check_trim(" abc");
check_trim("abc ");
check_trim(" \t\r\nX \t\r\n");

*/
