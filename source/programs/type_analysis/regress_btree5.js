function bottomUpTree()
{
}

for (var n = 4; n < 7; n += 1)
{
    var iterations = 3 - depth;

    for (var i = 1; i < iterations; i++)
        bottomUpTree();
}

