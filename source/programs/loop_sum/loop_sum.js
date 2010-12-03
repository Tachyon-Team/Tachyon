function loop_sum(n)
{
    var sum = 0;

    for (var i = 0; i < n; ++i)
    {
        sum += i;
    }

    return sum;
}
return loop_sum(10);
