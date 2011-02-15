/**
@fileOverview
Implementation of ECMAScript 5 array library routines.

@author
Marc Feeley, Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Tachyon Javascript Engine, All Rights Reserved
*/

/**
15.4.2 Array constructor function.
new Array (len)
new Array ([item0 [, item1 [, … ]]])
Array ([item0 [, item1 [, … ]]])
*/
function Array(len)
{
    // Constructor call with length
    if (isGlobalObj(this) === false &&
        typeof len === 'number' &&
        arguments.length === 1)
    {
        var a = [];
        a.length = len;

        return a;
    }

    var a = [];
    a.length = arguments.length;

    for (var i = 0; i < arguments.length; ++i)
        a[i] = arguments[i];

    return a;
}

/**
15.4.3.1 Array prototype object
*/
Array.prototype = {};

/**
Anonymous function to initialize this library
*/
(function ()
{
    // Get a reference to the context
    var ctx = iir.get_ctx();

    // Set the Array prototype object in the context
    set_ctx_arrproto(ctx, Array.prototype);
})();

//-----------------------------------------------------------------------------

// Operations on Array objects.

function array_toObject(x)
{
    return x;
}

function array_toString()
{
    var o = array_toObject(this);

    return o.join(',');
}

function array_concat()
{
    var o = array_toObject(this);
    var len = o.length;

    for (var i=arguments.length-1; i>=0; i--)
        len += arguments[i].length;

    var a = new Array(len);

    for (var i=arguments.length-1; i>=0; i--)
    {
        var x = arguments[i];

        for (var j=x.length-1; j>=0; j--)
            a[--len] = x[j];
    }

    for (var j=o.length-1; j>=0; j--)
        a[--len] = o[j];

    return a;
}

function array_join(separator)
{
    var o = array_toObject(this);

    if (separator === undefined)
        separator = ",";
    else
        separator = separator.toString();

    var str = "";
    for (var i=o.length-1; i>=0; i--)
    {
        var e = o[i];

        var estr = (i !== 0)? separator:'';

        if (e !== UNDEFINED)
        {
            estr = estr + String(e);
        }

        str = estr + str;
    }

    return str;
}

function array_pop()
{
    var o = array_toObject(this);
    var len = o.length;

    if (len === 0)
        return undefined;

    var result = o[len-1];

    delete o[len-1];

    o.length = len-1;

    return result;
}

function array_push()
{
    var o = array_toObject(this);
    var len = o.length;

    for (var i=0; i<arguments.length; i++)
        o[len+i] = arguments[i];

    return o.length;
}

function array_reverse()
{
    // This implementation of reverse assumes that no element of the
    // array is deleted.

    var o = array_toObject(this);
    var len = o.length;
    var lo = 0;
    var hi = len - 1;

    while (lo < hi)
    {
        var tmp = o[hi];
        o[hi] = o[lo];
        o[lo] = tmp;
        lo++;
        hi--;
    }

    return o;
}

function array_shift()
{
    // This implementation of shift assumes that no element of the
    // array is deleted.

    var o = array_toObject(this);
    var len = o.length;

    if (len === 0)
        return undefined;

    var first = o[0];

    for (var i=1; i<len; i++)
        o[i-1] = o[i];

    delete o[len-1];
    o.length = len-1;

    return first;
}

function array_slice(start, end)
{
    var o = array_toObject(this);
    var len = o.length;

    if (start === undefined)
        start = 0;
    else
    {
        if (start < 0)
        {
            start = len + start;
            if (start < 0)
                start = 0;
        }
        else if (start > len)
            start = len;
    }

    if (end === undefined)
        end = len;
    else
    {
        if (end < 0)
        {
            end = len + end;
            if (end < start)
                end = start;
        }
        else if (end < start)
            end = start;
        else if (end > len)
            end = len;
    }

    var n = end - start;
    var a = new Array(n);

    for (var i=n-1; i>=0; i--)
        a[i] = o[start+i];

    return a;
}

function array_sort(comparefn)
{
    var o = array_toObject(this);
    var len = o.length;

    if (comparefn === undefined)
        comparefn = array_sort_comparefn_default;

    /* Iterative mergesort algorithm */

    if (len >= 2)
    {
        /* Sort pairs in-place */

        for (var start=((len-2)>>1)<<1; start>=0; start-=2)
        {
            if (comparefn(o[start], o[start+1]) > 0)
            {
                var tmp = o[start];
                o[start] = o[start+1];
                o[start+1] = tmp;
            }
        }

        if (len > 2)
        {
            /*
             * For each k>=1, merge each pair of groups of size 2^k to
             * form a group of size 2^(k+1) in a second array.
             */

            var a1 = o;
            var a2 = new Array(len);

            var k = 1;
            var size = 2;

            do
            {
                var start = ((len-1)>>(k+1))<<(k+1);
                var j_end = len;
                var i_end = start+size;

                if (i_end > len)
                    i_end = len;

                while (start >= 0)
                {
                    var i = start;
                    var j = i_end;
                    var x = start;

                    for (;;)
                    {
                        if (i < i_end)
                        {
                            if (j < j_end)
                            {
                                if (comparefn(a1[i], a1[j]) > 0)
                                    a2[x++] = a1[j++];
                                else
                                    a2[x++] = a1[i++];
                            }
                            else
                            {
                                while (i < i_end)
                                    a2[x++] = a1[i++];
                                break;
                            }
                        }
                        else
                        {
                            while (j < j_end)
                                a2[x++] = a1[j++];
                            break;
                        }
                    }

                    j_end = start;
                    start -= 2*size;
                    i_end = start+size;
                }

                var t = a1;
                a1 = a2;
                a2 = t;

                k++;
                size *= 2;
            } while (len > size);

            if ((k & 1) === 0)
            {
                /* Last merge was into second array, so copy it back to o. */

                for (var i=len-1; i>=0; i--)
                    o[i] = a1[i];
            }
        }
    }

    return o;
}

function array_sort_comparefn_default(x, y)
{
    if (String(x) > String(y))
        return 1;
    else
        return -1;
}

function array_splice(start, deleteCount)
{
    var o = array_toObject(this);
    var len = o.length;

    if (start === undefined)
        start = len;
    else
    {
        if (start < 0)
        {
            start = len + start;
            if (start < 0)
                start = 0;
        }
        else if (start > len)
            start = len;
    }

    if (deleteCount === undefined)
        deleteCount = len - start;
    else
    {
        if (deleteCount < 0)
            deleteCount = 0;
        else if (deleteCount > len - start)
            deleteCount = len - start;
    }

    var itemCount = arguments.length - 2;

    if (itemCount < 0)
        itemCount = 0;

    var adj = itemCount - deleteCount;
    var deleteEnd = start + deleteCount;

    var result = o.slice(start, deleteEnd);

    if (adj < 0)
    {
        for (var i=deleteEnd; i<len; i++)
            o[i+adj] = o[i];
        for (var i=len+adj; i<len; i++)
            delete o[i];
        o.length = len+adj;
    }
    else if (adj > 0)
    {
        for (var i=len-1; i>=deleteEnd; i--)
            o[i+adj] = o[i];
    }

    for (var i=itemCount-1; i>=0; i--)
        o[start+i] = arguments[2+i];

    return result;
}

function array_unshift()
{
    var o = array_toObject(this);
    var len = o.length;
    var argCount = arguments.length;

    if (argCount > 0)
    {
        for (var i=len-1; i>=0; i--)
            o[i+argCount] = o[i];
        for (var i=argCount-1; i>=0; i--)
            o[i] = arguments[i];
    }

    return len + argCount;
}

function array_indexOf(searchElement, fromIndex)
{
    var o = array_toObject(this);
    var len = o.length;

    if (arguments.length <= 1)
        fromIndex = 0;
    else
    {
        if (fromIndex < 0)
        {
            fromIndex = len + fromIndex;
            if (fromIndex < 0)
                fromIndex = 0;
        }
    }

    for (var i=fromIndex; i<len; i++)
        if (o[i] === searchElement)
            return i;

    return -1;
}

function array_lastIndexOf(searchElement, fromIndex)
{
    var o = array_toObject(this);
    var len = o.length;

    if (arguments.length <= 1 || fromIndex >= len)
        fromIndex = len-1;
    else if (fromIndex < 0)
        fromIndex = len + fromIndex;

    for (var i=fromIndex; i>=0; i--)
        if (o[i] === searchElement)
            return i;

    return -1;
}

function array_forEach(callbackfn, thisArg)
{
    var o = array_toObject(this);
    var len = o.length;

    for (var i=0; i<len; i++)
        callbackfn.call(thisArg, o[i], i, o);
}

function array_map(callbackfn, thisArg)
{
    var o = array_toObject(this);
    var len = o.length;

    var a = new Array(len);

    for (var i=0; i<len; i++)
        a[i] = callbackfn.call(thisArg, o[i], i, o);

    return a;
}

function array_filter(callbackfn, thisArg)
{
    var o = array_toObject(this);
    var len = o.length;

    var a = [];

    for (var i=0; i<len; i++)
    {
        var x = o[i];
        if (callbackfn.call(thisArg, x, i, o))
            a.push(x);
    }

    return a;
}

// Setup Array.prototype .

Array.prototype.toString    = array_toString;
Array.prototype.concat      = array_concat;
Array.prototype.join        = array_join;
Array.prototype.pop         = array_pop;
Array.prototype.push        = array_push;
Array.prototype.reverse     = array_reverse;
Array.prototype.shift       = array_shift;
Array.prototype.slice       = array_slice;
Array.prototype.sort        = array_sort;
Array.prototype.splice      = array_splice;
Array.prototype.unshift     = array_unshift;
Array.prototype.indexOf     = array_indexOf;
Array.prototype.lastIndexOf = array_lastIndexOf;
Array.prototype.forEach     = array_forEach;
Array.prototype.map         = array_map;
Array.prototype.filter      = array_filter;

//-----------------------------------------------------------------------------
