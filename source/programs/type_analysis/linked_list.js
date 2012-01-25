var list = null;

function linkValue(value)
{
    typeAssert(value, '"int"');

    list = { value: value, next: list };

    typeAssert(list.next, '["or", "null", "object"]');
}

function sumValues()
{
    var sum = 0;

    for (var node = list; node !== null; node = node.next)
    {
        typeAssert(node.value, '"int"');

        sum += node.value;
    }
    
    return sum;
}

function test()
{
    var sum = sumValues();

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

