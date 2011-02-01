function myCtor(v)
{
    this.val = v;
}

function foo(n)
{
    var myObj = new myCtor(n);

    myObj.val += 1;

    return myObj.val;
}
