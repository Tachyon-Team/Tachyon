/**
@fileOverview
Implementation of the Foreign Function Interface (FFI) to interface with
the C/C++ code required by Tachyon.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
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

/**
Convert from a C type name to the corresponding C type
*/
function cTypeToIRType(cType, params)
{
    switch (cType)
    {
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
function CFunction(funcName, argTypes, retType, params)
{
    this.funcName = funcName;


    this.argTypes = argTypes.map(cTypeToIRType);


    this.retType = cTypeToIRType(retType, params);


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
Initialize FFI functions for the current configuration
*/
function initFFI(params)
{
    var ffiPrintInt = new CFunction('printInt', ['int'], 'void');

    config.hostParams.staticEnv.regBinding('printInt', ffiPrintInt);
}

