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

    /* Sort pairs in-place */

    for (var start=((len-1)>>1)<<1; start>=0; start-=2)
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

    return o;
}

function array_sort_comparefn_default(x, y)
{
    if (x > y)
        return 1;
    else
        return -1;
}

function array_splice(start, deleteCount)
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

    if (deleteCount === undefined)
        deleteCount = 0;
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
//
// Array.prototype.toString = array_toString;
// ...

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

/*
// Check correctness.

function check_toString(arr)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1 = arr1.toString();
    var res2 = arr2.array_toString();

    if (res1 !== res2)
        error("toString, results are different");
}

check_toString([]);
check_toString([11]);
check_toString([11,22]);
check_toString([11,22,33]);
check_toString([11,22,33,44,55,66,77,88,99]);

function check_concat(arr, arg1, arg2, arg3)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1;
    var res2;

    if (arg3 !== undefined)
    {
        res1 = arr1.concat(arg1, arg2, arg3);
        res2 = arr2.array_concat(arg1, arg2, arg3);
    }
    else if (arg2 !== undefined)
    {
        res1 = arr1.concat(arg1, arg2);
        res2 = arr2.array_concat(arg1, arg2);
    }
    else if (arg1 !== undefined)
    {
        res1 = arr1.concat(arg1);
        res2 = arr2.array_concat(arg1);
    }
    else
    {
        res1 = arr1.concat();
        res2 = arr2.array_concat();
    }

    if (res1.length !== res2.length)
        error("concat, results are different");
    for (var i=0; i<res1.length; i++)
        if (res1[i] !== res2[i])
            error("concat, results are different");
}

check_concat([11,22]);
check_concat([11,22],[33,44]);
check_concat([11,22],[33,44],[55,66]);
check_concat([11,22],[33,44],[55,66],[77,88]);

function check_join(arr, sep)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1;
    var res2;

    if (sep === undefined)
    {
        res1 = arr1.join();
        res2 = arr2.array_join();
    }
    else
    {
        res1 = arr1.join(sep);
        res2 = arr2.array_join(sep);
    }

    if (res1 !== res2)
        error("join, results are different");
}

check_join([]);
check_join([11]);
check_join([11,22]);
check_join([11,22,33]);
check_join([],"::");
check_join([11],"::");
check_join([11,22],"::");
check_join([11,22,33],"::");

function check_pop(arr)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1 = arr1.pop();
    var res2 = arr2.array_pop();

    if (res1 !== res2)
        error("pop, results are different");

    if (arr1.length !== arr2.length)
        error("pop, results are different");
    for (var i=0; i<arr1.length; i++)
        if (arr1[i] !== arr2[i])
            error("pop, results are different");
}

check_pop([]);
check_pop([11]);
check_pop([11,22]);
check_pop([11,22,33]);
check_pop([11,22,33,44,55,66,77,88,99]);

function check_push(arr, item1, item2, item3)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1; 
    var res2;

    if (item3 !== undefined)
    {
        res1 = arr1.push(item1, item2, item3);
        res2 = arr2.array_push(item1, item2, item3);
    }
    else if (item2 !== undefined)
    {
        res1 = arr1.push(item1, item2);
        res2 = arr2.array_push(item1, item2);
    }
    else if (item1 !== undefined)
    {
        res1 = arr1.push(item1);
        res2 = arr2.array_push(item1);
    }
    else
    {
        res1 = arr1.push();
        res2 = arr2.array_push();
    }

    if (res1 !== res2)
        error("push, results are different");

    if (arr1.length !== arr2.length)
        error("push, results are different");
    for (var i=0; i<arr1.length; i++)
        if (arr1[i] !== arr2[i])
            error("push, results are different");
}

check_push([11,22,33]);
check_push([11,22,33], 44);
check_push([11,22,33], 44, 55);
check_push([11,22,33], 44, 55, 66);

function check_reverse(arr)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1 = arr1.reverse();
    var res2 = arr2.array_reverse();

    if (res1.length !== res2.length)
        error("reverse, results are different");
    for (var i=0; i<res1.length; i++)
        if (res1[i] !== res2[i])
            error("reverse, results are different");

    if (arr1.length !== arr2.length)
        error("reverse, results are different");
    for (var i=0; i<arr1.length; i++)
        if (arr1[i] !== arr2[i])
            error("reverse, results are different");
}

check_reverse([]);
check_reverse([11]);
check_reverse([11,22]);
check_reverse([11,22,33]);
check_reverse([11,22,33,44,55,66,77,88,99]);

function check_shift(arr)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1 = arr1.shift();
    var res2 = arr2.array_shift();

    if (res1 !== res2)
        error("shift, results are different");

    if (arr1.length !== arr2.length)
        error("shift, results are different");
    for (var i=0; i<arr1.length; i++)
        if (arr1[i] !== arr2[i])
            error("shift, results are different");
}

check_shift([]);
check_shift([11]);
check_shift([11,22]);
check_shift([11,22,33]);
check_shift([11,22,33,44,55,66,77,88,99]);

function check_slice(arr, start, end)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1 = arr1.slice(start, end);
    var res2 = arr2.array_slice(start, end);

    if (res1.length !== res2.length)
        error("slice, results are different");
    for (var i=0; i<res1.length; i++)
        if (res1[i] !== res2[i])
            error("slice, results are different");
}

check_slice([11,22,33,44,55,66,77,88,99]);
check_slice([11,22,33,44,55,66,77,88,99], 1);
check_slice([11,22,33,44,55,66,77,88,99], 1, 3);
check_slice([11,22,33,44,55,66,77,88,99], 1, -3);
check_slice([11,22,33,44,55,66,77,88,99], -7, -3);

function check_sort(arr)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1 = arr1.sort(check_sort_comparefn);
    var res2 = arr2.array_sort(check_sort_comparefn);

    if (arr1.length !== arr2.length)
        error("sort, results are different");
    for (var i=0; i<arr1.length; i++)
        if (arr1[i] !== arr2[i])
            error("sort, results are different");
}

function check_sort_comparefn(x, y)
{
    if (x < y)
        return -1;
    else if (x > y)
        return 1;
    else
        return 0;
}

check_sort([]);
check_sort([11]);
check_sort([11,22]);
check_sort([22,11]);
check_sort([11,22,33]);
check_sort([11,33,22]);
check_sort([22,11,33]);
check_sort([33,11,22]);
check_sort([22,33,11]);
check_sort([33,22,11]);
check_sort([11,22,33,44]);
check_sort([11,33,22,44]);
check_sort([22,11,33,44]);
check_sort([33,11,22,44]);
check_sort([22,33,11,44]);
check_sort([33,22,11,44]);
check_sort([11,22,44,33]);
check_sort([11,33,44,22]);
check_sort([22,11,44,33]);
check_sort([33,11,44,22]);
check_sort([22,33,44,11]);
check_sort([33,22,44,11]);
check_sort([11,44,22,33]);
check_sort([11,44,33,22]);
check_sort([22,44,11,33]);
check_sort([33,44,11,22]);
check_sort([22,44,33,11]);
check_sort([33,44,22,11]);
check_sort([44,11,22,33]);
check_sort([44,11,33,22]);
check_sort([44,22,11,33]);
check_sort([44,33,11,22]);
check_sort([44,22,33,11]);
check_sort([44,33,22,11]);
check_sort([11,22,33,44,55,66,77,88,99]);
check_sort([99,88,77,66,55,44,33,22,11]);

function check_splice(arr, start, end, item1, item2, item3)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1; 
    var res2;

    if (item3 !== undefined)
    {
        res1 = arr1.splice(start, end, item1, item2, item3);
        res2 = arr2.array_splice(start, end, item1, item2, item3);
    }
    else if (item2 !== undefined)
    {
        res1 = arr1.splice(start, end, item1, item2);
        res2 = arr2.array_splice(start, end, item1, item2);
    }
    else if (item1 !== undefined)
    {
        res1 = arr1.splice(start, end, item1);
        res2 = arr2.array_splice(start, end, item1);
    }
    else
    {
        res1 = arr1.splice(start, end);
        res2 = arr2.array_splice(start, end);
    }

    if (res1.length !== res2.length)
        error("splice, results are different");
    for (var i=0; i<res1.length; i++)
        if (res1[i] !== res2[i])
            error("splice, results are different");

    if (arr1.length !== arr2.length)
        error("splice, results are different");
    for (var i=0; i<arr1.length; i++)
        if (arr1[i] !== arr2[i])
            error("splice, results are different");
}

check_splice([11,22,33,44,55,66,77,88,99]);
check_splice([11,22,33,44,55,66,77,88,99], 1);
check_splice([11,22,33,44,55,66,77,88,99], 1, 3);
check_splice([11,22,33,44,55,66,77,88,99], 1, 2, 11111);
check_splice([11,22,33,44,55,66,77,88,99], 1, 2, 11111, 22222);
check_splice([11,22,33,44,55,66,77,88,99], 1, 2, 11111, 22222, 33333);
check_splice([11,22,33,44,55,66,77,88,99], -7, 2, 11111);
check_splice([11,22,33,44,55,66,77,88,99], -7, 2, 11111, 22222);
check_splice([11,22,33,44,55,66,77,88,99], -7, 2, 11111, 22222, 33333);

function check_unshift(arr, item1, item2, item3)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1; 
    var res2;

    if (item3 !== undefined)
    {
        res1 = arr1.unshift(item1, item2, item3);
        res2 = arr2.array_unshift(item1, item2, item3);
    }
    else if (item2 !== undefined)
    {
        res1 = arr1.unshift(item1, item2);
        res2 = arr2.array_unshift(item1, item2);
    }
    else if (item1 !== undefined)
    {
        res1 = arr1.unshift(item1);
        res2 = arr2.array_unshift(item1);
    }
    else
    {
        res1 = arr1.unshift();
        res2 = arr2.array_unshift();
    }

    if (res1 !== res2)
        error("unshift, results are different");

    if (arr1.length !== arr2.length)
        error("unshift, results are different");
    for (var i=0; i<arr1.length; i++)
        if (arr1[i] !== arr2[i])
            error("unshift, results are different");
}

check_unshift([11,22,33]);
check_unshift([11,22,33], 44);
check_unshift([11,22,33], 44, 55);
check_unshift([11,22,33], 44, 55, 66);

function check_indexOf(arr, searchElement, fromIndex)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1;
    var res2;

    if (fromIndex !== undefined)
    {
        res1 = arr1.indexOf(searchElement, fromIndex);
        res2 = arr2.array_indexOf(searchElement, fromIndex);
    }
    else
    {
        res1 = arr1.indexOf(searchElement);
        res2 = arr2.array_indexOf(searchElement);
    }

    if (res1 !== res2)
        error("indexOf, results are different");
}

check_indexOf([11,22,11,33,22,44,22], 22);
check_indexOf([11,22,11,33,22,44,22], 22, 0);
check_indexOf([11,22,11,33,22,44,22], 22, 1);
check_indexOf([11,22,11,33,22,44,22], 22, 2);
check_indexOf([11,22,11,33,22,44,22], 22, 7);
check_indexOf([11,22,11,33,22,44,22], 22, -1);
check_indexOf([11,22,11,33,22,44,22], 22, -2);
check_indexOf([11,22,11,33,22,44,22], 22, -10);

function check_lastIndexOf(arr, searchElement, fromIndex)
{
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    var res1;
    var res2;

    if (fromIndex !== undefined)
    {
        res1 = arr1.lastIndexOf(searchElement, fromIndex);
        res2 = arr2.array_lastIndexOf(searchElement, fromIndex);
    }
    else
    {
        res1 = arr1.lastIndexOf(searchElement);
        res2 = arr2.array_lastIndexOf(searchElement);
    }

    if (res1 !== res2)
        error("lastIndexOf, results are different");
}

check_lastIndexOf([11,22,11,33,22,44,22], 22);
check_lastIndexOf([11,22,11,33,22,44,22], 22, 0);
check_lastIndexOf([11,22,11,33,22,44,22], 22, 1);
check_lastIndexOf([11,22,11,33,22,44,22], 22, 2);
check_lastIndexOf([11,22,11,33,22,44,22], 22, 7);
check_lastIndexOf([11,22,11,33,22,44,22], 22, -1);
check_lastIndexOf([11,22,11,33,22,44,22], 22, -2);
check_lastIndexOf([11,22,11,33,22,44,22], 22, -10);

function check_forEach(arr)
{
    var callbackfn = check_forEach_callbackfn;
    var thisArg = undefined;
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    check_forEach_sum = 0;
    arr1.forEach(callbackfn, thisArg);
    var sum1 = check_forEach_sum;
    check_forEach_sum = 0;
    arr2.array_forEach(callbackfn, thisArg);
    var sum2 = check_forEach_sum;

    if (sum1 !== sum2)
        error("forEach, results are different");
}

var check_forEach_sum;

function check_forEach_callbackfn(val, index, arr)
{
    check_forEach_sum += (val + index * 100 + arr.length * 10000);
}

check_forEach([]);
check_forEach([1]);
check_forEach([1,2]);
check_forEach([1,2,3]);
check_forEach([1,2,3,4]);

function check_map(arr)
{
    var callbackfn = check_map_callbackfn;
    var thisArg = undefined;
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    check_map_sum = 0;
    var res1 = arr1.map(callbackfn, thisArg);
    var sum1 = check_map_sum;
    check_map_sum = 0;
    var res2 = arr2.array_map(callbackfn, thisArg);
    var sum2 = check_map_sum;

    if (sum1 !== sum2)
        error("map, results are different");

    if (res1.length !== res2.length)
        error("map, results are different");
    for (var i=0; i<res1.length; i++)
        if (res1[i] !== res2[i])
            error("map, results are different");
}

var check_map_sum;

function check_map_callbackfn(val, index, arr)
{
    check_map_sum += (val + index * 100 + arr.length * 10000);
    return val*val + 1000;
}

check_map([]);
check_map([1]);
check_map([1,2]);
check_map([1,2,3]);
check_map([1,2,3,4]);

function check_filter(arr)
{
    var callbackfn = check_filter_callbackfn;
    var thisArg = undefined;
    var arr1 = arr.slice(0);
    var arr2 = arr.slice(0);
    check_filter_sum = 0;
    var res1 = arr1.filter(callbackfn, thisArg);
    var sum1 = check_filter_sum;
    check_filter_sum = 0;
    var res2 = arr2.array_filter(callbackfn, thisArg);
    var sum2 = check_filter_sum;

    if (sum1 !== sum2)
        error("filter, results are different");

    if (res1.length !== res2.length)
        error("filter, results are different");
    for (var i=0; i<res1.length; i++)
        if (res1[i] !== res2[i])
            error("filter, results are different");
}

var check_filter_sum;

function check_filter_callbackfn(val, index, arr)
{
    check_filter_sum += (val + index * 100 + arr.length * 10000);
    return (val & 1) === 1;
}

check_filter([]);
check_filter([1]);
check_filter([1,2]);
check_filter([1,2,3]);
check_filter([1,2,3,4]);
*/

//-----------------------------------------------------------------------------
