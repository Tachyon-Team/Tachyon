var o = { count: 0 };

o.incr = function ()
{
    this.count++;
}

o.reset = function ()
{
    this.count = 0;
}

o.getCount = function ()
{
    return this.count;
}

o.incr();

o.reset();

o.incr();
o.incr();

o.getCount();

