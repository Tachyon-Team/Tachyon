function bottomUpTree()
{
}

bottomUpTree();

var d = 0;

typeAssert(d, '["and", "int", ["not", "undef"]]');

bottomUpTree();

/*
FIXME: call to bottomUpTree makes d undef...

Could also be a problem at the merge at entry of bottomUpTree
- Removing the first call makes the problem vanish

On the second call, we enter bottomUpTree with d not existing,
but the global object exists?

In this case, the analysis is correct? Inside bottomUpTree, d could
indeed be undefined on the global object

To fix this problem, would need calling context or lazy propagation
system.
*/
//typeAssert(d, '["and", "int", ["not", "undef"]]');

