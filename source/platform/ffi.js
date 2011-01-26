/**
@fileOverview
Implementation of the Foreign Function Interface (FFI) to interface with
the C/C++ code required by Tachyon.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
Convert from a C type name to the corresponding C type
*/
function cTypeToIRType(cType, params)
{
    switch (cType)
    {
        case 'short':
        return IRType.i16;

        case 'unsigned short':
        return IRType.u16;

        case 'int':
        return IRType.pint;

        case 'char*':
        case 'void*':
        return IRType.rptr;

        case 'void':
        return IRType.none;

        default:
        error('unsupported C type: ' + cType);        
    }
}

/**
Generate a code string to perform a type conversion
*/
function genTypeConv(inType, outType, inVar)
{
    if (inType === outType)
        return inVar;

    switch (inType)
    {
        case IRType.box:
        switch (outType)
        {
            case IRType.pint:
            return 'unboxInt(' + inVar + ')';

            case IRType.rptr:
            return 'iir.icast(IRType.rptr, ' + inVar + ')';

            case IRType.i16:
            return 'iir.icast(IRType.i16, unboxInt(' + inVar + '))';
            
            case IRType.u16:
            return 'iir.icast(IRType.u16, unboxInt(' + inVar + '))';            
        }
        break;

        case IRType.pint:
        switch (outType)
        {
            case IRType.box:
            return 'boxInt(' + inVar + ')';
        }
        break;

        case IRType.i16:
        switch (outType)
        {
            case IRType.box:
            return 'boxInt(iir.icast(IRType.i16, ' + inVar + '))';
        }
        break;
        
        case IRType.u16:
        switch (outType)
        {
            case IRType.box:
            return 'boxInt(iir.icast(IRType.u16, ' + inVar + '))';
        }
        break;

        /*        
        TODO: string conversions
        case IRType.rptr:
        {
            case IRType.box:
            return 'TODO';
        }
        break;
        */
    }

    assert (
        false,
        'cannot convert from ' + inType + ' to ' + outType
    );
}

/**
Represents a C FFI function
*/
function CFunction(
    funcName,
    cArgTypes,
    cRetType,
    params,
    tachArgTypes,
    tachRetType
)
{
    assert (
        cArgTypes instanceof Array && cRetType !== undefined
    );
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    // If tachyon argument types are not specified
    if (tachArgTypes === undefined)
    {
        tachArgTypes = [];
        for (var i = 0; i < cArgTypes.length; ++i)
            tachArgTypes.push(IRType.box);
    }

    // If the tachyon return type was not specified
    if (tachRetType === undefined)
    {
        tachRetType = IRType.box;
    }

    assert (
        tachArgTypes.length === cArgTypes.length
    );

    // Convert the C argument types to IR types
    cArgTypes = cArgTypes.map(function (t) { return cTypeToIRType(t, params); });

    // Convert the C return type to an IR type
    cRetType = cTypeToIRType(cRetType, params);

    /**
    Name of the C function
    @field
    */
    this.funcName = funcName;

    /**
    Argument types of the C function
    @field
    */
    this.cArgTypes = cArgTypes;

    /**
    Return type of the C function
    @field
    */
    this.cRetType = cRetType;

    /**
    Argument types of the wrapper
    @field
    */
    this.tachArgTypes = tachArgTypes;

    /**
    Return type of the wrapper
    @field
    */
    this.tachRetType = tachRetType;

    /**
    Address of the C function
    @field
    */
    this.funcPtr = asm.address(getFuncAddr(funcName));
}
CFunction.prototype = new IRValue();

/**
Return the IR value name for this function
*/
CFunction.prototype.getValName = function ()
{
    return '<c-ffi' + (this.funcName? (' "' + this.funcName + '"'):'') + '>';
};

/**
Obtain a string representation of the function
*/
CFunction.prototype.toString = CFunction.prototype.getValName;

/**
Generate code for a wrapper function for a C FFI function
*/
CFunction.prototype.genWrapper = function ()
{
    // Source string to store the generated code
    var sourceStr = '';

    sourceStr += 'function ' + this.funcName + '(';

    for (var i = 0; i < this.tachArgTypes.length; ++i)
    {
        sourceStr += 'a' + i;        
        if (i !== this.tachArgTypes.length - 1)
            sourceStr += ', ';
    }

    sourceStr += ')\n';
    sourceStr += '{\n';
    sourceStr += '\t"tachyon:static";\n';
    sourceStr += '\t"tachyon:ret ' + this.tachRetType + '";\n';

    for (var i = 0; i < this.tachArgTypes.length; ++i)
    {
        var argType = this.tachArgTypes[i];
        sourceStr += '\t"tachyon:arg a' + i + ' ' + argType + '";\n';
    }

    for (var i = 0; i < this.tachArgTypes.length; ++i)
    {
        var cType = this.cArgTypes[i];
        var tType = this.tachArgTypes[i];

        if (cType === tType)
            continue;

        var varName = 'a' + i;

        sourceStr += '\t' + varName + ' = ';
        sourceStr += genTypeConv(tType, cType, varName) + ';\n';
    }
    
    var retVoid = this.cRetType === IRType.none;

    sourceStr += '\t' + ((retVoid === true)? '':'var r = ') + 'iir.call_ffi(ffi_' + this.funcName;

    for (var i = 0; i < this.tachArgTypes.length; ++i)
    {
        sourceStr += ', a' + i;
    }

    sourceStr += ');\n';

    // Generate the return statement
    sourceStr += '\treturn';
    if (retVoid === false)
        sourceStr += ' ' + genTypeConv(this.cRetType, this.tachRetType, 'r');
    sourceStr += ';\n';

    sourceStr += '}\n';

    //print(sourceStr);

    // Return the generated code
    return sourceStr;
};

/**
@class Represents a proxy function callable from C for a Tachyon function
*/
function CProxy(
    irFunction,
    params,
    cArgTypes,
    cRetType,
    ctxVal
)
{
    assert (
        irFunction instanceof IRFunction
    );

    // The types presented to C must be specified
    assert (
        cArgTypes instanceof Array && cRetType !== undefined
    );
    
    assert (
        params instanceof CompParams
    );

    assert (
        irFunction.argTypes.length === cArgTypes.length
    );

    assert (
        ctxVal instanceof ConstValue ||
        ctxVal === undefined
    );

    // For now, assume the context is always passed as an argument
    assert (ctxVal === undefined);

    // Convert the C argument types to IR types
    cArgTypes = cArgTypes.map(function (t) { return cTypeToIRType(t, params); });

    // Convert the C return type to an IR type
    cRetType = cTypeToIRType(cRetType, params);

    // If the context should be passed as an argument, make the first
    // C argument a void pointer
    if (ctxVal === undefined)
        cArgTypes.unshift(IRType.rptr);

    // Find a free global name to call the function through
    var funcName = findFreeName(
        function (n) { return params.staticEnv.hasBinding(n); }, 
        irFunction.funcName + '_tproxy'
    )

    // Bind the function in the static environment
    params.staticEnv.regBinding(funcName, irFunction);

    /**
    Tachyon function to be wrapped by this proxy
    @field
    */
    this.irFunction = irFunction;    

    /**
    Global name to call the function through
    @field
    */
    this.funcName = funcName;

    /**
    Argument types of the C function
    @field
    */
    this.cArgTypes = cArgTypes;

    /**
    Return type of the C function
    @field
    */
    this.cRetType = cRetType;

    /**
    Context pointer to be used. May be undefined if
    the context is to be passed by argument.
    @field
    */
    this.ctxVal = ctxVal;
}
CProxy.prototype = {};

/**
Generate the proxy function
*/
CProxy.prototype.genProxy = function ()
{
    // Source string to store the generated code
    var sourceStr = '';

    var funcName = 'cproxy_' + this.irFunction.funcName;

    sourceStr += 'function ' + funcName + '(';

    if (this.ctxVal === undefined)
    {
        sourceStr += 'ctx';

        if (this.irFunction.argTypes.length > 0)
            sourceStr += ', ';
    }

    for (var i = 0; i < this.irFunction.argTypes.length; ++i)
    {
        sourceStr += 'a' + i;        
        if (i !== this.irFunction.argTypes.length - 1)
            sourceStr += ', ';
    }

    sourceStr += ')\n';
    sourceStr += '{\n';
    sourceStr += '\t"tachyon:cproxy";\n';
    sourceStr += '\t"tachyon:ret ' + this.cRetType + '";\n';

    if (this.ctxVal === undefined)
    {
        sourceStr += '\t"tachyon:arg ctx rptr";\n';
    }

    for (var i = 0; i < this.irFunction.argTypes.length; ++i)
    {
        var argType = this.cArgTypes[
            this.cArgTypes.length - this.irFunction.argTypes.length + i
        ];

        sourceStr += '\t"tachyon:arg a' + i + ' ' + argType + '";\n';
    }
    
    if (this.ctxVal === undefined)
    {
        sourceStr += '\tiir.set_ctx(ctx);\n';
    }

    //sourceStr += '\tprintInt(1337);\n';

    // Get the global object from the context if available
    sourceStr += '\tvar global = ';
    if (this.ctxVal === undefined)
        sourceStr += '(ctx !== NULL_PTR)? get_ctx_globalobj(ctx):UNDEFINED';
    else
        sourceStr += 'UNDEFINED';
    sourceStr += ';\n';

    // Convert the types of function arguments
    for (var i = 0; i < this.irFunction.argTypes.length; ++i)
    {
        var cType = this.cArgTypes[
            this.cArgTypes.length - this.irFunction.argTypes.length + i
        ];
        var tType = this.irFunction.argTypes[i];

        if (cType === tType)
            continue;

        var varName = 'a' + i;

        sourceStr += '\t' + varName + ' = ';
        sourceStr += genTypeConv(cType, tType, varName) + ';\n';
    }
    
    var retVoid = this.cRetType === IRType.none;

    sourceStr += '\t' + ((retVoid === true)? '':'var r = ') + 'iir.call(';
    sourceStr += this.funcName + ', global';

    for (var i = 0; i < this.irFunction.argTypes.length; ++i)
    {
        sourceStr += ', ' + 'a' + i;
    }

    sourceStr += ');\n';

    //sourceStr += '\tprintInt(13372);\n';

    if (retVoid === false)
    {
        sourceStr += '\treturn ' + genTypeConv(this.irFunction.retType, this.cRetType, 'r') + ';\n';
    }
    else
    {
        sourceStr += '\treturn;';
    }
    
    //sourceStr += 'return iir.icast(IRType.pint, 7);';

    sourceStr += '}\n';
    
    //print(sourceStr);

    // Compile the source string into an IR function
    var func = compileSrcString(sourceStr, config.hostParams);

    // Return the compiled function
    return func.childFuncs[0];
};

/**
Create a bridge to call a compiled Tachyon function through V8
*/
function makeBridge(
    irFunction,
    cArgTypes,
    cRetType
)
{
    // Generate a proxy for the function
    var proxy = new CProxy(
        irFunction,
        config.hostParams,
        cArgTypes,
        cRetType
    );
    
    var wrapper = proxy.genProxy();

    //print(wrapper);
    
    // Get pointer to entry point of compiled wrapper function
    var funcAddr = wrapper.linking.getEntryPoint('default').getAddr();

    // Callable bridge function
    function bridge(ctxPtr)
    {
        //print(ctxPtr);
        //print(funcAddr.getBytes());

        var argArray = [];
        for (var i = 1; i < arguments.length; ++i)
            argArray.push(arguments[i]);

        var result = callTachyonFFI.apply(
            null,
            [
                cArgTypes,
                cRetType,
                funcAddr.getBytes(),
                ctxPtr
            ].concat(argArray)
        );

        //print(result);

        return result;
    };

    return bridge;
}

/**
Initialize FFI functions for the current configuration
*/
function initFFI(params)
{
    function regFFI(ffiFunc)
    {
        params.ffiFuncs[ffiFunc.funcName] = ffiFunc;
        params.staticEnv.regBinding('ffi_' + ffiFunc.funcName, ffiFunc);
    }

    regFFI(new CFunction(
        'malloc', 
        ['int'], 
        'void*',
        params,
        [IRType.pint],
        IRType.rptr
    ));

    regFFI(new CFunction(
        'free', 
        ['void*'],
        'void',
        params,
        [IRType.rptr],
        IRType.none
    ));

    regFFI(new CFunction(
        'exit', 
        ['int'],
        'void',
        params
    ));

    regFFI(new CFunction(
        'printInt', 
        ['int'],
        'void',
        params
    ));

    regFFI(new CFunction(
        'printStr',
        ['char*'], 
        'void',
        params,
        [IRType.rptr],
        IRType.none
    ));

    regFFI(new CFunction(
        'sum2Ints', 
        ['int', 'int'], 
        'int',
        params
    ));
}

