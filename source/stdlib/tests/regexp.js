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

tests.stdlib = (tests.stdlib !== undefined)? tests.stdlib : tests.testSuite();

tests.stdlib.regexps = function ()
{
    function check_equal_matches (
        n,
        a,
        b
    )
    {
        if (typeof a !== typeof b)
        {
            print(a);
            throw "regexp unit " + n + " failed (" + typeof a + " != " + typeof b + ")";
        }

        if (typeof a === "string" || typeof b === "string")
        {
            if (a !== b)
                throw "regexp unit " + n + " failed (" + a + " != " + b + ")";
            else
                return;
        }

        if (a === null || b === null)
        {
            if (a !== b)
                throw "regexp unit " + n + " failed (" + a + " != " + b + ")";
            else
                return;
        }

        if (a.length !== b.length)
            throw "regexp unit " + n + " failed";

        // Test string array equality.
        for (var i = 0; i < a.length; ++i)
            if (a[i] !== b[i])
                throw "regexp unit " + n + " failed";
    }
    
//    check_equal_matches(1, new RegExp("a").exec("a"), "a");
    check_equal_matches(2, new RegExp("a").exec("b"), null);
    check_equal_matches(3, new RegExp("[^a]").exec("b"), "b");
    check_equal_matches(4, new RegExp("[^a]").exec("a"), null);
    check_equal_matches(5, new RegExp(".").exec("a"), "a");
    check_equal_matches(6, new RegExp(".").exec("\n"), null);
    check_equal_matches(7, new RegExp("[ab]").exec("a"), "a");
    check_equal_matches(8, new RegExp("[ab]").exec("c"), null);
    check_equal_matches(9, new RegExp("^$").exec(""), "");
    check_equal_matches(10, new RegExp("a??").exec("a"), "");
    check_equal_matches(11, new RegExp("a??").exec("b"), "");
    check_equal_matches(12, new RegExp("a??").exec(""), "");
    check_equal_matches(13, new RegExp("^foo").exec("foo"), "foo");
    check_equal_matches(14, new RegExp("^foo").exec(" foo"), null);
    check_equal_matches(15, new RegExp("^foo$").exec("foo"), "foo");
    check_equal_matches(16, new RegExp("^foo$").exec("foo "), null);
    check_equal_matches(17, new RegExp("\\d+").exec("foobar42foo"), "42");
    check_equal_matches(18, new RegExp("\\D+").exec("foobar42foo"), "foobar");
    check_equal_matches(19, new RegExp("\\s+").exec("foobar  42foo"), "  ");
    check_equal_matches(20, new RegExp("\\S+").exec("foobar  42foo"), "foobar");
    check_equal_matches(21, new RegExp("\\w+").exec("foobar  42foo"), "foobar");
    check_equal_matches(22, new RegExp("\\W+").exec("foobar  ?+=/42foo"), "  ?+=/");
    check_equal_matches(23, new RegExp("\\t\\n\\v\\f\\r").exec("\t\n\v\f\r"), "\t\n\v\f\r");
    check_equal_matches(24, new RegExp("(z)((a+)?(b+)?(c))*").exec("zaacbbbcac"), ["zaacbbbcac", "z", "ac", "a", undefined, "c"]);
    check_equal_matches(25, new RegExp("aa|bb|cc").exec("cc"), "cc");
    check_equal_matches(26, new RegExp("(aa|aabaac|ba|b|c)*").exec("aabaac"), ["aaba", "ba"]);
    check_equal_matches(27, new RegExp("a[a-z]{2,4}").exec("abcdefghi"), "abcde");
    check_equal_matches(28, new RegExp("a[a-z]{2,4}?").exec("abcdefghi"), "abc");
    check_equal_matches(29, new RegExp("a|ab").exec("abc"), "a");
    check_equal_matches(30, new RegExp("((a)|(ab))((c)|(bc))").exec("abc"), ["abc", "a", "a", undefined, "bc", undefined, "bc"]);
    check_equal_matches(31, new RegExp("<(.*)>(.*)</\\0>").exec("<h1>Foobar</h1>"), ["<h1>Foobar</h1>", "h1", "Foobar"]);
    check_equal_matches(32, new RegExp("^(.(.(.(.(.(.(.(.(.(.(.(.(.)))))))))))))$").exec("aaaaaaaaaaaaa"), ["aaaaaaaaaaaaa","aaaaaaaaaaaaa","aaaaaaaaaaaa","aaaaaaaaaaa","aaaaaaaaaa","aaaaaaaaa","aaaaaaaa","aaaaaaa","aaaaaa","aaaaa","aaaa","aaa","aa","a"]);
    check_equal_matches(33, new RegExp("(.)(.)(.)(.)(.)(.)(.)\\7\\6\\5\\4\\3\\2\\1").exec("abcdefggfedcba"), ["abcdefggfedcba", "a", "b", "c", "d", "e", "f", "g"]);
    check_equal_matches(34, new RegExp("(.)(.)(.)(.)(.)(.)(.)\\7\\6\\5\\4\\3\\2\\1").exec("abcdefgabcdefg"), null);
    check_equal_matches(35, new RegExp("(x+x+)+y").exec("xxxxxxxxxxxxy"), ["xxxxxxxxxxxxy", "xxxxxxxxxxxx"]);
    check_equal_matches(36, new RegExp("(x+x+)+y").exec("xxxxx"), null);
    check_equal_matches(37, new RegExp("^(x?)(x?)(x?).*;(?:\\1|\\2|\\3x),(?:\\1|\\2x|\\3),(?:\\1x|\\2x|\\3),(?:\\1x|\\2x|\\3x),").exec("xxx;x,x,x,x,"), ["xxx;x,x,x,x,", "x", "", "x"]);
    check_equal_matches(38, new RegExp("(a*)(b*)").exec("aaaabbbb"), ["aaaabbbb", "aaaa", "bbbb"]);
    check_equal_matches(39, new RegExp("(a*?)(b*?)").exec("aaaabbbb"), ["", "", ""]);
    check_equal_matches(40, new RegExp("(a*?)(b+?)").exec("aaaabbb"), ["aaaab", "aaaa", "b"]);
    check_equal_matches(41, new RegExp("a+ \\bb+").exec("aaaaa bbb"), "aaaaa bbb");
    check_equal_matches(42, new RegExp("a+\\Bb+").exec("aaaaabbb"), "aaaaabbb");
    check_equal_matches(43, new RegExp("(x*)*").exec("xxx"), ["xxx", "xxx"]);
    check_equal_matches(44, new RegExp("(((x*)*)*)*").exec("xxx"), ["xxx", "xxx", "xxx", "xxx"]);
    check_equal_matches(45, new RegExp("^(a+)\\1*,(?:\\1)+$").exec("aaaaaaaaaa,aaaaaaaaaaaaaaa"), ["aaaaaaaaaa,aaaaaaaaaaaaaaa", "aaaaa"]);
    check_equal_matches(46, new RegExp("(?=(a+))a*b\\1").exec("baaabac"), ["aba", "a"]);
    check_equal_matches(47, new RegExp("(?=(a+))").exec("baaabac"), ["", "aaa"]);
    check_equal_matches(48, new RegExp("(?!foo).*").exec("foofoobar"), "oofoobar");
    check_equal_matches(49, new RegExp("(.*?)a(?!(a+)b\\2c)\\2(.*)").exec("baaabaac"), ["baaabaac", "ba", undefined, "abaac"]);
    check_equal_matches(50, new RegExp("()\\1*").exec(""), ["", ""]);
    check_equal_matches(51, new RegExp("(a*)*").exec("b"), ["",undefined]);
    check_equal_matches(52, new RegExp("(a*)b\\1+").exec("baaaac"), ["b", ""]);
    check_equal_matches(53, new RegExp("()").exec(""), ["", ""]);
};
