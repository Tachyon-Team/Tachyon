function Node(v)
{
    this.value = v;
    this.edges = [];
}

function addEdge(n)
{
    //iir.trace_print('entering addEdge');

    this.edges.push(n);
}
Node.prototype.addEdge = addEdge;

function remEdge(n)
{
    //iir.trace_print('remEdge');    

    var idx = this.edges.indexOf(n);

    //iir.trace_print('splicing');

    if (idx !== -1)
        this.edges.splice(idx, 1);
    else
        error('edge missing');

    //iir.trace_print('leaving remEdge');
}
Node.prototype.remEdge = remEdge;

function graphSum(root)
{
    //iir.trace_print('entering graphSum');

    var visited = [];

    var sum = 0;

    function visit(n)
    {
        //iir.trace_print('entering visit');

        assert (
            ptrInHeap(iir.icast(IRType.rptr, unboxRef(n))),
            'node not in heap'
        );

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

    // Shrink the heap for testing
    var heapSize = get_ctx_heapsize(ctx);
    shrinkHeap(puint(2000000));

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
        //iir.trace_print('itr');

        var curGCCount = get_ctx_gccount(ctx);
        if (curGCCount >= initGCCount + u32(20))
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

    //iir.trace_print('final sum');

    if (graphSum(root) !== 6)
        return 3;

    // Restore the old heap size
    set_ctx_heapsize(ctx, heapSize);

    //iir.trace_print('final gcCollect');

    gcCollect();

    return 0;
}

