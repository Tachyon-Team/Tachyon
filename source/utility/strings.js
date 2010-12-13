/**
@fileOverview
Useful string-related functions.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Escape JavaScript strings for output
*/
function escapeJSString(input)
{
    var chars = [];

    for (var i = 0; i < input.length; ++i)
    {
        var charCode = input.charCodeAt(i);

        switch (charCode)
        {
            // '\0'
            case 0:
            chars.push(92);
            chars.push(48);
            break;

            // '\b'
            case 8:
            chars.push(92);
            chars.push(98);
            break;

            // '\t'
            case 9:
            chars.push(92);
            chars.push(116);
            break;

            // '\n'
            case 10:
            chars.push(92);
            chars.push(110);
            break;

            // '\v'
            case 11:
            chars.push(92);
            chars.push(118);
            break;

            // '\f'
            case 12:
            chars.push(92);
            chars.push(102);
            break;

            // '\r'
            case 13:
            chars.push(92);
            chars.push(114);
            break;

            // '\"'
            case 34:
            chars.push(92);
            chars.push(34);
            break;

            // '\''
            case 39:
            chars.push(92);
            chars.push(39);
            break;

            // '\r'
            case 92:
            chars.push(92);
            chars.push(92);
            break;

            default:
            {
                // If this character is in the printable ASCII range
                if (charCode >= 32 && charCode <= 126)
                {
                    chars.push(charCode);
                }
                else
                {
                    var numStr = charCode.toString(16);
                    var hexStr = '0000'.slice(numStr.length) + numStr;

                    chars.push(92);
                    chars.push(117);                    
                    for (var j = 0; j < hexStr.length; ++j)
                        chars.push(hexStr.charCodeAt(j));                               
                }
            }
        }
    }

    return String.fromCharCode.apply(null, chars);
}

/**
Escape XML strings for output
*/
function escapeXMLString(input, isHTML)
{
    var chars = [];

    function pushStr(str)
    {
        for (var i = 0; i < str.length; ++i)
            chars.push(str.charCodeAt(i));
    }

    for (var i = 0; i < input.length; ++i)
    {
        var charCode = input.charCodeAt(i);

        switch (charCode)
        {
            // &
            case 38:
            pushStr('&amp;');
            break;

            // <
            case 60:
            pushStr('&lt;');
            break;

            // >
            case 62:
            pushStr('&gt;');
            break;

            // "
            case 34:
            pushStr('&quot;');
            break;

            // '
            case 39:
            pushStr('&apos;');
            break;

            // space
            case 32:
            pushStr('&nbsp;');
            break;

            // newline
            case 10:
            if (isHTML)
                pushStr('<br />');
            else
                chars.push(charCode)
            break;

            default:
            chars.push(charCode);
        }
    }

    return String.fromCharCode.apply(null, chars);
}

/**
Indent each line of a text string
*/
function indentText(inputStr, indentStr)
{
    if (!indentStr)
        indentStr = '\t';

    var chars = [];

    if (inputStr.length > 0)
    {
        for (var i = 0; i < indentStr.length; ++i)
            chars.push(indentStr.charCodeAt(i));
    }

    for (var i = 0; i < inputStr.length; ++i)
    {
        var charCode = inputStr.charCodeAt(i);

        chars.push(charCode);

        if (charCode == 10 && i != inputStr.length - 1)
        {
            for (var j = 0; j < indentStr.length; ++j)
                chars.push(indentStr.charCodeAt(j));
        }
    }

    return String.fromCharCode.apply(null, chars);
}

/**
Pluralize a word in function of a count
*/
function pluralize(word, count)
{
    return word + ((count != 1)? 's':'');
}

/**
Left-pad a string until it has the desired length
*/
function leftPadStr(str, pad, len)
{
    str = str.toString();

    while (str.length < len)
        str = pad + str;

    return str;
}

/**
Remove a given set of characters from the start and end of a string
*/
function stripStr(str, chars)
{
    if (!chars)
        chars = ' \v\t\r\n';

    BEGIN_LOOP:
    for (var startIdx = 0; startIdx < str.length; ++startIdx)
    {
        ch = str.charAt(startIdx);

        for (var j = 0; j < chars.length; ++j)
        {
            ch2 = chars.charAt(j);

            if (ch == ch2)
                continue BEGIN_LOOP;
        }

        break;
    }

    END_LOOP:
    for (var endIdx = str.length - 1; endIdx >= 0; --endIdx)
    {
        ch = str.charAt(endIdx);

        for (var j = 0; j < chars.length; ++j)
        {
            ch2 = chars.charAt(j);

            if (ch == ch2)
                continue END_LOOP;
        }

        break;
    }

    return str.substr(startIdx, endIdx + 1);
}

