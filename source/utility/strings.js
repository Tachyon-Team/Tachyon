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

