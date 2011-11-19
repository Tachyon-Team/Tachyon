var list = null;

function linkValue(value)
{
    list = { value: value, next: list };
}

function sumValues()
{
    var sum = 0;

    for (var node = list; node !== null; node = node.next)
        sum += node.value;    
    
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

