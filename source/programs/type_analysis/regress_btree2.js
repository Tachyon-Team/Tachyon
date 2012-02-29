function TreeNode(left,right,item)
{
    this.left = left;
    this.right = right;
    this.item = item;
}

function bottomUpTree(depth)
{
    if (depth > 0)
    {
        return new TreeNode(
            bottomUpTree(depth-1),
            bottomUpTree(depth-1),
            3
        );
    }
    else 
    {
        return new TreeNode(
            null,
            null,
            7
        );
    }
}

var r = bottomUpTree(3);



// Assert that the object is not undef
typeAssert(r, '["and", "object", ["not", "undef"]]');





// Assert that the object can come from both branches
//typeAssert(r.item, '["and", "int", "string"]');

