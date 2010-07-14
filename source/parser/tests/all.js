function f(x, y)
{
    var a;
    var b = 0;
    var c, d;
    while (x > 0) x = x-1;
    do { x = x+1; } while (x < 10);
    for (x=0; x<20; x++) print(x);
    for (var i, j, k; i<20; i++) print(i);
    for (var i=0; i<20; i++) print(i);
    for (a in y) print(i);
    for (var i in y) print(i);
    with (x) { print("yo!"); }
    switch (x)
    {
        case 0: print("ho");
        case 1: print("hi");
        default: print("hello!");
    }
    lab: x = 10;
    throw x;
    try { print("a"); } finally { print("b"); }
    x = function (y) { try { print("a"); } catch(e) { y = function () { print(e+y+z); }; } };
    try { print("a"); } catch(e) { print(e); } finally { print("b"); }
    debugger;
}
