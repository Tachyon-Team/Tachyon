// Initialize the Tachyon configuration
initConfig(PLATFORM_64BIT, log.level('all'));

// Perform a minimal Tachyon compilation
bootstrap(config.hostParams, false, false);

// Execute the backend unit tests
tests.x86.asmEncoding();
tests.x86.asmExecution();
tests.x86.irToAsm();

function compileStr(str)
{
    const params = config.hostParams;

    var ast = parse_src_str(str, params);

    var ir = unitToIR(ast, params);

    lowerIRFunc(ir, params);

    print(ir);

    params.backend.genCode(ir, params);
}

try 
{
    // TODO: ir-to-asm unit tests, using C call conv?
    // Start simple, IIR only

    // TODO: 
    // [X] test iir arith ops
    // [ ] test iir arith ops w/ spills
    // [ ] test if stmt + iir
    // [ ] test iir loops
    // [ ] test args obj
    // [ ] test calling C funcs    

    /*
    compileStr('                        \
    function add (v1, v2)               \
    {                                   \
        "tachyon:arg v1 pint";          \
        "tachyon:arg v2 pint";          \
        "tachyon:ret pint";             \
                                        \
        var x0 = iir.add(v1, v2);       \
        var x1 = iir.mul(x0, pint(3));  \
        var x2 = iir.sub(x1, pint(2));  \
                                        \
        return x2;                      \
    }                                   \
    ');
    */


    compileStr('                        \
    function add (v1, v2, v3)           \
    {                                   \
        "tachyon:arg v1 pint";          \
        "tachyon:arg v2 pint";          \
        "tachyon:arg v3 pint";          \
        "tachyon:ret pint";             \
                                        \
        var x1 = iir.add(v1, pint(1));  \
        var x2 = iir.add(v1, pint(2));  \
        var x3 = iir.add(v1, pint(3));  \
        var x4 = iir.add(v1, pint(4));  \
        var x5 = iir.add(v1, pint(5));  \
        var x6 = iir.add(v1, pint(6));  \
        var x7 = iir.add(v1, pint(7));  \
                                        \
        var y = iir.mul(x1, x2);        \
        var y = iir.mul(y, x3);         \
        var y = iir.mul(y, x4);         \
        var y = iir.mul(y, x5);         \
        var y = iir.mul(y, x6);         \
        var y = iir.mul(y, x7);         \
                                        \
        var z = iir.sub(y, pint(1));    \
                                        \
        return z;                       \
    }                                   \
    ');


    /*
    compileStr('                        \
    function add (v1, v2, v3)           \
    {                                   \
        "tachyon:arg v1 pint";          \
        "tachyon:arg v2 pint";          \
        "tachyon:arg v3 pint";          \
        "tachyon:ret pint";             \
                                        \
        if (v1 !== pint(0))             \
        {                               \
            return v2;                  \
        }                               \
        else                            \
        {                               \
            return v3;                  \
        }                               \
    }                                   \
    ');
    */

    //compileStr('function add(v1, v2) { return v1 + v2; }');
}

catch (e)
{
    if (e.stack)
        print(e.stack);
    else
        print(e);
}

print('');
print('done');

