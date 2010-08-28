function f(x)
{
    if (x < 2)
        return x;
    else
        return f(x-1) + f(x-2);
}
return f(20);
