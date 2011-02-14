function bubbleSort(list, compOp)
{
    do
    {
        var swapped = false;

        for (i = 1; i < list.length; ++i)
        {
            if (compOp(list[i-1], list[i]))
            {
                var temp = list[i-1];
                list[i-1] = list[i];
                list[i] = temp;

                swapped = true;
            }
        }

    } while (swapped);
}

function numCompOp(x, y)
{
    return x > y;
}

function test()
{
    var list = [4,6,34,5,6,8,1,-5,2,7];

    var sorted = [-5,1,2,4,5,6,6,7,8,34];

    bubbleSort(list, numCompOp);

    if (String(list) != String(sorted))
        return 1;

    return 0;
}

