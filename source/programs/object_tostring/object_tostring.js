function myCtor(n)
{
    this.val = n;
}

myCtor.prototype = {};

myCtor.prototype.toString = function ()
{
    return 'val=' + this.val;
}

function test()
{
    var o = new myCtor(5);

    var s = o + 3;

    if (s != "val=53")
        return 1;    

    return 0;
}
