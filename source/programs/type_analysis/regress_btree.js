function bottomUpTree(item, depth)
{
    if (depth > 0)
    {
        return {
            next: bottomUpTree(2*item-1, depth-1),
            item: 'foo'
        };
    }
    else 
    {
        return {
            next: null,
            item: 3
        };
    }
}

var r = bottomUpTree(3, 3);

// Assert that the object is not undef
typeAssert(r, '["and", "object", ["not", "undef"]]');

// Assert that the object can come from both branches
typeAssert(r.item, '["and", "int", "string"]');

