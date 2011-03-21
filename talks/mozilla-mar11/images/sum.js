function sum(list) {
    var sum = 0;
    for (var i = 0; i < list.length; ++i)
        sum += f(list[i]);
    return sum;
}

function f(v) { return v*v; }
print(sum([1,2,3,4,5]));
