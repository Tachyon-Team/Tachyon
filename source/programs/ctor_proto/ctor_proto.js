function counter(n)
{
    this.count = n;
}

counter.prototype = {};

counter.prototype.inc = function ()
{
    this.count++;
};

counter.prototype.dec = function ()
{
    this.count--;
};

function test(n)
{
    var c = new counter(n);

    // +5
    c.inc();
    c.inc();
    c.inc();
    c.inc();
    c.inc();

    // -1
    c.dec();

    return c.count;
}
