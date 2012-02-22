function theMaker()
{
    function theClosure()
    {
    }

    return theClosure.prototype;
}

var o1 = theMaker();
var o2 = theMaker();

o1.f = "foo";
o2.f = 3;

typeAssert(o1.f, '"string"');
typeAssert(o2.f, '"int"');

