function test_ctor()
{
    var o = {};

    if (typeof Object !== 'function')
        return 1;

    if (typeof o !== 'object')
        return 2;

    if (!(o instanceof Object))
        return 3;

    return 0;
}

function test_getPrototypeOf()
{
    //Object.getPrototypeOf = function (obj)

    var o = {}

    if (Object.getPrototypeOf(o) !== Object.prototype)
        return 1;

    return 0;
}

function test_create()
{
    //Object.create = function (obj, props)

    var a = {};

    var o = Object.create(a);

    if (Object.getPrototypeOf(o) !== a)
        return 1;

    return 0;
}

function test_defineProperty()
{
    //Object.defineProperty = function (obj, prop, attribs)

    var o = {};

    Object.defineProperty(o, 'p', { value: 7 });

    if (o.p !== 7)
        return 1;

    return 0;
}

function test_toString()
{
    //Object.prototype.toString = function ()

    var o = {};

    if (o.toString() !== 'object')
        return 1;

    return 0;
}

function test_hasOwnProperty()
{
    //Object.prototype.hasOwnProperty = function (prop)

    var a = { va: 9 };

    var b = Object.create(a);

    b.vb = 10;

    if (b.hasOwnProperty('vb') !== true)
        return 1;

    if (b.hasOwnProperty('va') !== false)
        return 2;

    if (a.hasOwnProperty('va') !== true)
        return 3;

    return 0;
}

function test_isPrototypeOf()
{
    //Object.prototype.isPrototypeOf = function (obj)

    var a = {};

    var o = Object.create(a);

    if (a.isPrototypeOf(o) !== true)
        return 1;

    if (Object.prototype.isPrototypeOf(a) !== true)
        return 2;

    return 0;
}

function test()
{
    var r = test_ctor();
    if (r != 0)
        return 100 + r;

    var r = test_getPrototypeOf();
    if (r != 0)
        return 200 + r;

    var r = test_create();
    if (r != 0)
        return 300 + r;

    var r = test_defineProperty();
    if (r != 0)
        return 400 + r;

    var r = test_toString();
    if (r != 0)
        return 500 + r;

    var r = test_hasOwnProperty();
    if (r != 0)
        return 600 + r;

    var r = test_isPrototypeOf();
    if (r != 0)
        return 700 + r;

    return 0;
}

