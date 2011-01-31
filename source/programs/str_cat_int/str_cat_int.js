function foo()
{
    var str1 = -12 + "hi" + 0 + "my" + 10 + "friend" + 3;
    var str2 = "-12hi0my10friend3";

    if (str1 === str2)
        return 0;
    else
        return 1;
}
