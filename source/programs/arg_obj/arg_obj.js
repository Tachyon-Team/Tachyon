function foo(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9)
{
    return arguments.length + 
           arguments[0] + 
           arguments[1] +
           arguments[2] + 
           arguments[3] + 
           arguments[4] + 
           arguments[5] + 
           arguments[6] + 
           arguments[7] +
           arguments[8] + 
           arguments[9];
}

function foo_proxy()
{
    return foo(0,1,2,3,4,5,6,7,8,9);
}

