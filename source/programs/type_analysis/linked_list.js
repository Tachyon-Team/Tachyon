var list = null;

typeAssert(list, '["and", "null", ["not", "undef"]]');

function linkValue(value)
{
    typeAssert(value, '"int"');

    list = { value: value, next: list };

    typeAssert(list.next, '["and", "null", "object"]');
}

function sumValues()
{
    var sum = 0;

    for (var node = list; node !== null; node = node.next)
    {
        typeAssert(node.value, '["and", "int", [">=", 1]]');

        sum += node.value;
    }
    
    return sum;
}

function test()
{
    var sum = sumValues();

    typeAssert(sum, '"int"');

    if (sum !== 15)
        return 1;

    return 0;
}

linkValue(1);
linkValue(2);
linkValue(3);
linkValue(4);
linkValue(5);

test();

typeAssert(list, '["and", "object", ["not", "null"], ["not", "undef"]]');

