var T = true;
var F = false;
var undef = undefined;

var o_str = { toString: function () { return 'foo'; } };
var o_num = { toString: function () { return 3; } };

var tests = [
    // v1     v2      <  <= >  >= == != === !==
    [1      , 2     , T, T, F, F, F, T, F, T],
    ['1'    , 2     , T, T, F, F, F, T, F, T],
    [1      , '2'   , T, T, F, F, F, T, F, T],
    ['1'    , '2'   , T, T, F, F, F, T, F, T],
    ['34'   , '4'   , T, T, F, F, F, T, F, T],
    ['-100' , 'a'   , T, T, F, F, F, T, F, T],
    [undef  , '2'   , F, F, F, F, F, T, F, T],

    [2      , 1     , F, F, T, T, F, T, F, T],
    [2      , '1'   , F, F, T, T, F, T, F, T],
    ['2'    , 1     , F, F, T, T, F, T, F, T],
    ['2'    , '1'   , F, F, T, T, F, T, F, T],
    ['4'    , '34'  , F, F, T, T, F, T, F, T],
    ['a'    , '-100', F, F, T, T, F, T, F, T],
    ['2'    , undef , F, F, F, F, F, T, F, T],

    [o_num  , '4'   , T, T, F, F, F, T, F, T],
    [o_num  , '3'   , F, T, F, T, T, F, F, T],
    [o_num  , 3     , F, T, F, T, T, F, F, T],

    [o_str  , 'foo' , F, T, F, T, T, F, F, T],
    [o_str  , 'goo' , T, T, F, F, F, T, F, T],

    ['1'    , true  , F, T, F, T, T, F, F, T],
    ['0'    , false , F, T, F, T, T, F, F, T],

    [1      , 1     , F, T, F, T, T, F, T, F],
    ['2'    , '2'   , F, T, F, T, T, F, T, F],
    ['foo'  , 'foo' , F, T, F, T, T, F, T, F],
    [true   , true  , F, T, F, T, T, F, T, F],
    [false  , false , F, T, F, T, T, F, T, F],
    [null   , null  , F, T, F, T, T, F, T, F],
    [undef  , undef , F, F, F, F, T, F, T, F]
];

function test()
{
    for (var i = 0; i < tests.length; ++i)
    {
        var test = tests[i];

        var testNo = 10 * i;

        var v1 = test[0]
        var v2 = test[1];

        var lt  = test[2];
        var le  = test[3];
        var gt  = test[4];
        var ge  = test[5];
        var eq  = test[6];
        var ne  = test[7];
        var seq = test[8];
        var sne = test[9];

        if ((v1 < v2) !== lt)
            return testNo + 1;
        if ((v1 <= v2) !== le)
            return testNo + 2;
        if ((v1 > v2) !== gt)
            return testNo + 3;
        if ((v1 >= v2) !== ge)
            return testNo + 4;
        if ((v1 == v2) !== eq)
            return testNo + 5;
        if ((v1 != v2) !== ne)
            return testNo + 6;
        if ((v1 === v2) !== seq)
            return testNo + 7;
        if ((v1 !== v2) !== sne)
            return testNo + 8;
    }

    return 0;
}

//print(test());

