function Node(v)
{
    this.value = v;
    this.edges = [];
}

Node.prototype.addEdge = function (n)
{
    this.edges.push(n);
};

Node.prototype.remEdge = function (n)
{
    //iir.trace_print('remEdge');    

    var idx = this.edges.indexOf(n);

    //iir.trace_print('splicing');

    if (idx !== -1)
        this.edges.splice(idx, 1);

    //iir.trace_print('leaving remEdge');
};

function graphSum(root)
{
    //iir.trace_print('entering graphSum');

    var visited = [];

    var sum = 0;

    function visit(n)
    {
        //iir.trace_print('entering visit');

        if (visited.indexOf(n) !== -1)
            return;

        //iir.trace_print('calling push');

        visited.push(n);

        //iir.trace_print('called push');

        sum += n.value;

        for (var i = 0; i < n.edges.length; ++i)
            visit(n.edges[i]);

        //iir.trace_print('leaving visit');
    }
    
    visit(root);

    //iir.trace_print('leaving graphSum');

    return sum;
}

function test()
{
    var ctx = iir.get_ctx();
    var initGCCount = get_ctx_gccount(ctx);

    var root = new Node(1);
    var a = new Node(2);
    var b = new Node(3);

    root.addEdge(a);
    a.addEdge(b);
    b.addEdge(root);

    if (graphSum(root) !== 6)
        return 1;

    for (;;)
    {
        var curGCCount = get_ctx_gccount(ctx);
        if (curGCCount >= initGCCount + u32(3))
            break;

        //iir.trace_print('creating new node');

        var oa = root.edges[0];
        var na = new Node(oa.value);

        //iir.trace_print('patching edges');

        root.remEdge(oa);
        root.addEdge(na);
        na.addEdge(b);

        //iir.trace_print('in-loop sum');

        if (graphSum(root) !== 6)
            return 2;
    }

    iir.trace_print('final sum');

    if (graphSum(root) !== 6)
        return 3;

    iir.trace_print('final gcCollect');

    gcCollect();

    return 0;
}

