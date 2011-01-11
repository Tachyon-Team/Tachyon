/**
@fileOverview
Implementation of the Foreign Function Interface (FFI) to interface with
the C/C++ code required by Tachyon.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010-2011 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/*
FFI version 0.1, minimal pour bootstrap
---------------------------------------

// Enregistrement d'une fonction C (prototype)
// Type descend de IRValue, comme IRFunction
// Placeholder, contient:
// - nom de fonction
// - types des arguments
// - type de retour
var ffiFuncObj = new ffi.CFunction('printBar', ['char*', 'int'], 'int')

// Appel vers C, linké statiquement plus tard
// Le backend sait que call_ffi utilise la convention d'appel C
var retVal = iir.call_ffi(ffiFuncObj, args...);

// Marc: tous arguments boxés, fixnums ou strings
// new ffi.CFunction crée des wrappers automatiquement?

// Wrapper généré dynamiquement
box printBar(box s, box i)
{
    rptr str = malloc_and_copy_str(s);
    i32 iv = unboxInt(i);

    i32 retval = iir.call_ffi(ffi_func, str, iv);

    free_str(s);

    return boxInt(retVal);
}

FFI version 0.2, introduction de callbacks
------------------------------------------

// On passe un pointeur vers une fonction JS
// Un proxy (code stub) appellable à partir de C est créé
ffi.regCallback(jsFuncObj, ['char*', 'int'], 'int');

FFI version 0.3
---------------

- Parsing de headers C
- Création de code C qui accède aux objets JS (classe C++)

*/

/*
var foo2 = import_foreign("foo", ["int", "char*"], "int");

print(foo2(1, "hello"));

void foreign_proxy(void)
{
  context *ctx = ...;
  int *a = &ctx.a;
  int (*f)(int,int,int,int,int) = fn_tbl[ctx.fn_id];
  a[0] = f( a[0], a[1], a[2], a[3], a[4] )
}
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

        case 'int':
        return IRType.pint;

        case 'char*':
        return IRType.rptr;

        case 'void':
        return IRType.none;

        default:
        error('unsupported C type: ' + cType);        
    }
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
        params instanceof CompParams
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

    /**
    Name of the C function
    @field
    */
    this.funcName = funcName;

    /**
    Argument types of the C function
    @field
    */
    this.cArgTypes = cArgTypes.map(cTypeToIRType);

    /**
    Return type of the C function
    @field
    */
    this.cRetType = cTypeToIRType(cRetType, params);

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
    function genTypeConv(inType, outType, inVar)
    {
        switch (inType)
        {
            case IRType.box:
            switch (outType)
            {
                case IRType.pint:
                return 'unboxInt(' + inVar + ')';

                case IRType.i16:
                return 'iir.icast(IRType.i16, unboxInt(' + inVar + '))';
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
        }

        assert (
            false,
            'cannot convert from ' + inType + ' to ' + outType
        );
    }

    // Source string to store the generated code
    var sourceStr = '';

    sourceStr += 'function ' + this.funcName + '(';

    for (var i = 0; i < this.tachArgTypes.length; ++i)
    {
        sourceStr += 'a' + i;        
        if (i != this.tachArgTypes.length - 1)
            sourceStr += ',';
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

    if (retVoid === false)
    {
        sourceStr += '\treturn ' + genTypeConv(this.cRetType, this.tachRetType, 'r') + ';\n';
    }

    sourceStr += '}\n';

    //print(sourceStr);

    // Return the generated code
    return sourceStr;
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
        'printInt', 
        ['int'], 
        'void',
        params
    ));

    regFFI(new CFunction(
        'sum2Ints', 
        ['int', 'int'], 
        'int',
        params
    ));

    regFFI(new CFunction(
        'printHello', 
        [], 
        'void',
        params
    ));

    regFFI(new CFunction(
        'print2Shorts', 
        ['short', 'short'], 
        'void',
        params
    ));
}

