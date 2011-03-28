var zero = 0;

function sum(list) {
    var sum = zero;
    for (var i = 0; i < list.length; ++i) {
        var t = list[i];
        sum = numberAdd(sum, t);
    }
    return sum;
}

function f(x)
{
    zero = x;
    if (!(zero instanceof Number))
        recompile(sum);
}

print(sum([1,2,3,4,5]));

