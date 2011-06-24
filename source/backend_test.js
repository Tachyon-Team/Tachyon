function compileStr(str)
{
    const params = config.hostParams;

    var ast = parse_src_str(str, params);

    var ir = unitToIR(ast, params);

    lowerIRFunc(ir, params);

    print(ir);

    backend.genCode(ir);
}

// Execute the backend unit tests
tests.x86.encoding();
tests.x86.codegen();

// Initialize the Tachyon configuration
initConfig(PLATFORM_64BIT, log.level('all'));

// Perform a minimal Tachyon compilation
bootstrap(config.hostParams, false, false);

// Create a backend interface instance
var backend = new x86.Backend(PLATFORM_64BIT);




compileStr('function add(v1, v2) { return v1 + v2; }');




print('done');
