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

function printMatches (
    matches
)
{
    if (typeof matches === "object" && matches != null) 
    {
        // Print array of matches.
        if (matches.length > 0)
        {
            var str = "[";
            for (var i = 0; i < matches.length; ++i)
                str += matches[i] + ",";
            print(str.substring(0, str.length - 1) + "]");
        }
        else
        {
            print("[]");
        }
    }
    else
    {
        print(matches);
    }
}

function assertEqual (
    a,
    b
)
{
    if (typeof a !== typeof b)
        return false;

    if (a == null)
        if (b == null)
            return true;
        else
            return false;

    if (b == null)
        if (a == null)
            return true;
        else
            return false;

    if (typeof a === "string")
        return a == b;

    if (typeof a === "object")
    {
        if (a.length != b.length)
            return false;

        // Test string array equality.
        for (var i = 0; i < a.length; ++i)
            if (a[i] != b[i])
                return false;
        return true;
    }
    return false;
}
    

function execUnit (
    no,
    pattern,
    input,
    output
)
{
    var ast = new RegExpParser().parse(pattern);
    var graph = new REAstToGraph().compile(ast);
    var matches = execre(input, graph);

    if (!assertEqual(matches, output))
    {
        print(ast.pp());
        print("*** test no " + no + " failed");
        printMatches(matches);
        throw 0;
    }
}

execUnit(1, "a", "a", "a");
execUnit(2, "a", "b", null);
execUnit(3, "[^a]", "b", "b");
execUnit(4, "[^a]", "b", "b");
execUnit(5, "[^a]", "a", null);
execUnit(6, ".", "a", "a");
execUnit(7, ".", "\n", null);
execUnit(8, "[ab]", "a", "a");
execUnit(9, "[ab]", "c", null);
execUnit(10, "^$", "", "");
execUnit(11, "a??", "a", "");
execUnit(12, "a??", "b", "");
execUnit(13, "a??", "", "");
execUnit(14, "^foo", "foo", "foo");
execUnit(15, "^foo", " foo", null);
execUnit(16, "^foo$", "foo", "foo");
execUnit(17, "^foo$", "foo ", null);
execUnit(17, "\\d+", "foobar42foo", "42");
execUnit(18, "\\D+", "foobar42foo", "foobar");
execUnit(19, "\\s+", "foobar  42foo", "  ");
execUnit(20, "\\S+", "foobar  42foo", "foobar");
execUnit(21, "\\w+", "foobar  42foo", "foobar");
execUnit(22, "\\W+", "foobar  ?+=/42foo", "  ?+=/");
execUnit(23, "éàöüÀ", "éàöüÀ", "éàöüÀ");
execUnit(24, "\\u00E9\\u00E0\\xEB", "éàë", "éàë");
execUnit(25, "\\t\\n\\v\\f\\r", "\t\n\v\f\r", "\t\n\v\f\r");
execUnit(26, "(z)((a+)?(b+)?(c))*", "zaacbbbcac", ["zaacbbbcac", "z", "ac", "a", undefined, "c"]);
execUnit(27, "aa|bb|cc", "cc", "cc");
execUnit(28, "(aa|aabaac|ba|b|c)*", "aabaac", ["aaba", "ba"]);
execUnit(29, "a[a-z]{2,4}", "abcdefghi", "abcde");
execUnit(30, "a[a-z]{2,4}?", "abcdefghi", "abc");
execUnit(31, "a|ab", "abc", "a");
execUnit(32, "((a)|(ab))((c)|(bc))", "abc", ["abc", "a", "a", undefined, "bc", undefined, "bc"]);
execUnit(33, "<(.*)>(.*)</\\1>", "<h1>Foobar</h1>", ["<h1>Foobar</h1>", "h1", "Foobar"]);
execUnit(34, "^(.(.(.(.(.(.(.(.(.(.(.(.(.)))))))))))))$", "aaaaaaaaaaaaa", ["aaaaaaaaaaaaa","aaaaaaaaaaaaa","aaaaaaaaaaaa","aaaaaaaaaaa","aaaaaaaaaa","aaaaaaaaa","aaaaaaaa","aaaaaaa","aaaaaa","aaaaa","aaaa","aaa","aa","a"]);
execUnit(35, "(.)(.)(.)(.)(.)(.)(.)\\7\\6\\5\\4\\3\\2\\1", "abcdefggfedcba", ["abcdefggfedcba", "a", "b", "c", "d", "e", "f", "g"]);
execUnit(36, "(.)(.)(.)(.)(.)(.)(.)\\7\\6\\5\\4\\3\\2\\1", "abcdefgabcdefg", null);
execUnit(37, "(x+x+)+y", "xxxxxxxxxxxxy", ["xxxxxxxxxxxxy", "xxxxxxxxxxxx"]);
execUnit(38, "(x+x+)+y", "xxxxx", null);
execUnit(39, "^(x?)(x?)(x?).*;(?:\\1|\\2|\\3x),(?:\\1|\\2x|\\3),(?:\\1x|\\2x|\\3),(?:\\1x|\\2x|\\3x),", "xxx;x,x,x,x,", ["xxx;x,x,x,x,", "x", "", "x"]);
execUnit(40, "^(a+)\\1+,\\1+$", "aaaaaaaaaa,aaaaaaaaaaaaaaa", ["aaaaaaaaaa,aaaaaaaaaaaaaaa", "aaaaa"]);

