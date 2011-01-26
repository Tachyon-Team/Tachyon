function linkedlist(n)
{
    var head = null;

    for (var i = 0; i < n; ++i)
    {
        head = { val: i, next: head };
    }

    var sum = 0;

    for (var cur = head; cur !== null; cur = cur.next)
    {
        sum += cur.val;
    } 

    return sum;
}

return linkedlist(5);
