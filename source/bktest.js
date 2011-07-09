// Execute the backend unit tests
tests.x86.encoding();
tests.x86.codegen();

// Initialize the Tachyon configuration
initConfig(PLATFORM_64BIT, log.level('all'));

// Perform a minimal Tachyon compilation
bootstrap(config.hostParams, false, false);

// Create a backend interface instance
var backend = new x86.Backend(PLATFORM_64BIT);

assert (
    config.hostParams.backend === undefined,
    'backend object already defined on params'
);

config.hostParams.backend = backend;

function compileStr(str)
{
    const params = config.hostParams;

    var ast = parse_src_str(str, params);

    var ir = unitToIR(ast, params);

    lowerIRFunc(ir, params);

    print(ir);

    backend.genCode(ir, params);
}

try 
{
    // TODO: 
    // [ ] test simple iir ops
    // [ ] test loops
    // [ ] test args obj
    // [ ] test calling C funcs    


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

