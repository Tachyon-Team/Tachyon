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

function check_equal_matches (
    a,
    b
)
{
    if (b instanceof Array)
    {
        if (a instanceof Array)
        {
            if (a.length !== b.length)
                return false;

            for (var i = 0; i < a.length; ++i)
                if (a[i] !== b[i])
                    return false;
        }
        else
        {
            return false;
        }
    }
    else
    {
        if (a !== b)
            return false;
    }
    return true;
}

function test_char_match ()
{    
    if (!check_equal_matches(new RegExp("a").exec("a"), ["a"]))
        return false;
    if (!check_equal_matches(new RegExp("a").exec("b"), null))
        return false;
    if (!check_equal_matches(new RegExp("\\t\\n\\v\\f\\r").exec("\t\n\v\f\r"), ["\t\n\v\f\r"]))
        return false;
    return true;
}

function test_char_class_match ()
{
    if (!check_equal_matches(new RegExp("[^a]").exec("b"), ["b"]))
        return false;
    if (!check_equal_matches(new RegExp("[^a]").exec("a"), null))
        return false;
    if (!check_equal_matches(new RegExp(".").exec("a"), ["a"]))
        return false;
    if (!check_equal_matches(new RegExp(".").exec("\n"), null))
        return false;
    if (!check_equal_matches(new RegExp("[ab]").exec("a"), ["a"]))
        return false;
    if (!check_equal_matches(new RegExp("[ab]").exec("c"), null))
        return false;
    if (!check_equal_matches(new RegExp("\\d+").exec("foobar42foo"), ["42"]))
        return false;
    if (!check_equal_matches(new RegExp("\\D+").exec("foobar42foo"), ["foobar"]))
        return false;
    if (!check_equal_matches(new RegExp("\\s+").exec("foobar  42foo"), ["  "]))
        return false;
    if (!check_equal_matches(new RegExp("\\S+").exec("foobar  42foo"), ["foobar"]))
        return false;
    if (!check_equal_matches(new RegExp("\\w+").exec("foobar  42foo"), ["foobar"]))
        return false;
    if (!check_equal_matches(new RegExp("\\W+").exec("foobar  ?+=/42foo"), ["  ?+=/"]))
        return false;
    return true;
}

function test_assertion ()
{
    if (!check_equal_matches(new RegExp("^$").exec(""), [""]))
        return false;
    if (!check_equal_matches(new RegExp("^foo").exec("foo"), ["foo"]))
        return false;
    if (!check_equal_matches(new RegExp("^foo").exec(" foo"), null))
        return false;
    if (!check_equal_matches(new RegExp("^foo$").exec("foo"), ["foo"]))
        return false;
    if (!check_equal_matches(new RegExp("^foo$").exec("foo "), null))
        return false;
    if (!check_equal_matches(new RegExp("(?=(a+))a*b\\1").exec("baaabac"), ["aba", "a"]))
        return false;
    if (!check_equal_matches(new RegExp("(?=(a+))").exec("baaabac"), ["", "aaa"]))
        return false;
    if (!check_equal_matches(new RegExp("(?!foo).*").exec("foofoobar"), ["oofoobar"]))
        return false;
    if (!check_equal_matches(new RegExp("(.*?)a(?!(a+)b\\2c)\\2(.*)").exec("baaabaac"), ["baaabaac", "ba", undefined, "abaac"]))
        return false;
    if (!check_equal_matches(new RegExp("a+ \\bb+").exec("aaaaa bbb"), ["aaaaa bbb"]))
        return false;
    if (!check_equal_matches(new RegExp("a+\\Bb+").exec("aaaaabbb"), ["aaaaabbb"]))
        return false;
    return true;

}

function test_quantifier ()
{
    if (!check_equal_matches(new RegExp("a??").exec("a"), [""]))
        return false;
    if (!check_equal_matches(new RegExp("a??").exec("b"), [""]))
        return false;
    if (!check_equal_matches(new RegExp("a??").exec(""), [""]))
        return false;
    if (!check_equal_matches(new RegExp("a[a-z]{2,4}").exec("abcdefghi"), ["abcde"]))
        return false;
    if (!check_equal_matches(new RegExp("a[a-z]{2,4}?").exec("abcdefghi"), ["abc"]))
        return false;
    if (!check_equal_matches(new RegExp("(a*)(b*)").exec("aaaabbbb"), ["aaaabbbb", "aaaa", "bbbb"]))
        return false;
    if (!check_equal_matches(new RegExp("(a*?)(b*?)").exec("aaaabbbb"), ["", "", ""]))
        return false;
    if (!check_equal_matches(new RegExp("(a*?)(b+?)").exec("aaaabbb"), ["aaaab", "aaaa", "b"]))
        return false;
    if (!check_equal_matches(new RegExp("(x*)*").exec("xxx"), ["xxx", "xxx"]))
        return false;
    if (!check_equal_matches(new RegExp("(a*)*").exec("b"), ["",undefined]))
        return false;
    if (!check_equal_matches(new RegExp("(((x*)*)*)*").exec("xxx"), ["xxx", "xxx", "xxx", "xxx"]))
        return false;
    if (!check_equal_matches(new RegExp("(x+x+)+y").exec("xxxxxxxxxxxxy"), ["xxxxxxxxxxxxy", "xxxxxxxxxxxx"]))
        return false;
    if (!check_equal_matches(new RegExp("(x+x+)+y").exec("xxxxx"), null))
        return false;
    return true;
}

function test_disjunction ()
{
    if (!check_equal_matches(new RegExp("aa|bb|cc").exec("cc"), ["cc"]))
        return false;
    if (!check_equal_matches(new RegExp("(aa|aabaac|ba|b|c)*").exec("aabaac"), ["aaba", "ba"]))
        return false;
    if (!check_equal_matches(new RegExp("a|ab").exec("abc"), ["a"]))
        return false;
    return true;
}

function test_capture ()
{
    if (!check_equal_matches(new RegExp("(z)((a+)?(b+)?(c))*").exec("zaacbbbcac"), ["zaacbbbcac", "z", "ac", "a", undefined, "c"]))
        return false;
    if (!check_equal_matches(new RegExp("^(.(.(.(.(.(.(.(.(.(.(.(.(.)))))))))))))$").exec("aaaaaaaaaaaaa"), ["aaaaaaaaaaaaa","aaaaaaaaaaaaa","aaaaaaaaaaaa","aaaaaaaaaaa","aaaaaaaaaa","aaaaaaaaa","aaaaaaaa","aaaaaaa","aaaaaa","aaaaa","aaaa","aaa","aa","a"]))
        return false;
    if (!check_equal_matches(new RegExp("((a)|(ab))((c)|(bc))").exec("abc"), ["abc", "a", "a", undefined, "bc", undefined, "bc"]))
        return false;
    if (!check_equal_matches(new RegExp("()").exec(""), ["", ""]))
        return false;
    return true;
}

function test_backreference ()
{
    if (!check_equal_matches(new RegExp("<(.*)>(.*)</\\1>").exec("<h1>Foobar</h1>"), ["<h1>Foobar</h1>", "h1", "Foobar"]))
        return false;
    if (!check_equal_matches(new RegExp("(.)(.)(.)(.)(.)(.)(.)\\7\\6\\5\\4\\3\\2\\1").exec("abcdefggfedcba"), ["abcdefggfedcba", "a", "b", "c", "d", "e", "f", "g"]))
        return false;
    if (!check_equal_matches(new RegExp("(.)(.)(.)(.)(.)(.)(.)\\7\\6\\5\\4\\3\\2\\1").exec("abcdefgabcdefg"), null))
        return false;
    if (!check_equal_matches(new RegExp("(a*)b\\1+").exec("baaaac"), ["b", ""]))
        return false;
    if (!check_equal_matches(new RegExp("()\\1*").exec(""), ["", ""]))
        return false;
    if (!check_equal_matches(new RegExp("^(x?)(x?)(x?).*;(?:\\1|\\2|\\3x),(?:\\1|\\2x|\\3),(?:\\1x|\\2x|\\3),(?:\\1x|\\2x|\\3x),").exec("xxx;x,x,x,x,"), ["xxx;x,x,x,x,", "x", "", "x"]))
        return false;
    if (!check_equal_matches(new RegExp("^(a+)\\1*,(?:\\1)+$").exec("aaaaaaaaaa,aaaaaaaaaaaaaaa"), ["aaaaaaaaaa,aaaaaaaaaaaaaaa", "aaaaa"]))
        return false;
    return true;

}

function test ()
{
    if (!test_char_match())
        return false;
    if (!test_char_class_match())
        return false;
    if (!test_assertion())
        return false;
    if (!test_quantifier())
        return false;
    if (!test_disjunction())
        return false;
    if (!test_capture())
        return false;
    if (!test_backreference())
        return false;
    return true;
}
