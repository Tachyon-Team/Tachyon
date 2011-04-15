/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

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
Base class for mapping of Tachyon types to C types.
*/
function CTypeMapping()
{
    /**
    Name of the C type
    */
    this.cTypeName = null;

    /**
    IR type matching the C type
    */
    this.cIRType = null;

    /**
    IR type of the Tachyon value
    */
    this.jsIRType = null;

    /**
    Flag to indicate we should free produced values
    @field
    */
    this.freeAfterSnd = false;

    /**
    Flag to indicate we should free received values
    @field
    */
    this.freeAfterRcv = false;
}
CTypeMapping.prototype = {};

/**
Representation of the C void type.
*/
function CVoid()
{
    this.cTypeName = 'void';

    this.cIRType = IRType.none;

    this.jsIRType = IRType.none;
}
CVoid.prototype = new CTypeMapping();

/**
Conversion of boxed values to C string types and vice-versa.
*/
function CStringAsBox(freeRcv)
{
    // By default, C strings received as return values will be freed
    if (freeRcv === undefined)
        freeRcv = true;

    this.cTypeName = 'char*';

    this.cIRType = IRType.rptr;

    this.jsIRType = IRType.box;

    this.freeAfterSnd = true;

    this.freeAfterRcv = freeRcv;
}
CStringAsBox.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CStringAsBox.prototype.jsToC = function (inVar)
{
    return 'boxToCString(' + inVar + ')';
};

/**
Generate code for a conversion from a C value
*/
CStringAsBox.prototype.cToJS = function (inVar)
{
    return 'cStringToBox(' + inVar + ')';
};

/**
Generate to free a C string value
*/
CStringAsBox.prototype.free = function (inVar)
{
    return 'free(' + inVar + ')';
};

/**
C boolean to IR boolean type.
*/
function CBoolAsBool()
{
    this.cTypeName = 'bool';

    this.cIRType = IRType.bool;

    this.jsIRType = IRType.bool;
}
CBoolAsBool.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CBoolAsBool.prototype.jsToC = function (inVar)
{
    return inVar;
};

/**
Generate code for a conversion from a C value
*/
CBoolAsBool.prototype.cToJS = function (inVar)
{
    return inVar;
};

/**
Conversion of boxed integers to C integers and vice-versa.
*/
function CIntAsBox()
{
    this.cTypeName = 'int';

    this.cIRType = IRType.pint;

    this.jsIRType = IRType.box;
}
CIntAsBox.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CIntAsBox.prototype.jsToC = function (inVar)
{
    return 'unboxInt(' + inVar + ')';
};

/**
Generate code for a conversion from a C value
*/
CIntAsBox.prototype.cToJS = function (inVar)
{
    return 'boxInt(' + inVar + ')';
};

/**
C integer to raw IR integer type. The IR type can be specified on construction
or on conversion.
*/
function CIntAsInt(irIntType)
{
    assert (
        irIntType === undefined || 
        (irIntType instanceof IRType && irIntType.isInt()),
        'Invalid IR integer type specified'
    );

    this.cTypeName = 'int';

    this.cIRType = IRType.pint;

    this.jsIRType = irIntType;
}
CIntAsInt.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CIntAsInt.prototype.jsToC = function (inVar)
{
    return 'iir.icast(IRType.' + this.cIRType + ', ' + inVar + ')';
};

/**
Generate code for a conversion from a C value
*/
CIntAsInt.prototype.cToJS = function (inVar, jsIRType)
{
    jsIRType = (jsIRType !== undefined)? jsIRType:this.jsIRType;

    assert (
        jsIRType instanceof IRType && jsIRType.isInt(),
        'JS IR type not specified or non-integer'
    );

    return 'iir.icast(IRType.' + jsIRType + ', ' + inVar + ')';
};

/**
C pointer to raw IR pointer type.
*/
function CPtrAsPtr()
{
    this.cTypeName = 'void*';

    this.cIRType = IRType.rptr;

    this.jsIRType = IRType.rptr;
}
CPtrAsPtr.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CPtrAsPtr.prototype.jsToC = function (inVar)
{
    return inVar;
};

/**
Generate code for a conversion from a C value
*/
CPtrAsPtr.prototype.cToJS = function (inVar)
{
    return inVar;
};

/**
C pointer to box type.
*/
function CPtrAsBox()
{
    this.cTypeName = 'void*';

    this.cIRType = IRType.rptr;

    this.jsIRType = IRType.box;
}
CPtrAsBox.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CPtrAsBox.prototype.jsToC = function (inVar)
{
    return 'iir.icast(IRType.rptr, ' + inVar + ')';
};

/**
Generate code for a conversion from a C value
*/
CPtrAsBox.prototype.cToJS = function (inVar)
{
    return 'iir.icast(IRType.rptr, ' + inVar + ')';
};

/**
C pointer to unboxed reference type.
*/
function CPtrAsRef()
{
    this.cTypeName = 'void*';

    this.cIRType = IRType.rptr;

    this.jsIRType = IRType.ref;
}
CPtrAsRef.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CPtrAsRef.prototype.jsToC = function (inVar)
{
    return 'iir.icast(IRType.rptr, ' + inVar + ')';
};

/**
Generate code for a conversion from a C value
*/
CPtrAsRef.prototype.cToJS = function (inVar)
{
    return 'iir.icast(IRType.ref, ' + inVar + ')';
};

/**
C pointer to byte array mapping.
*/
function CPtrAsBytes()
{
    this.cTypeName = 'void*';

    this.cIRType = IRType.rptr;

    this.jsIRType = IRType.box;
}
CPtrAsBytes.prototype = new CTypeMapping();

/**
Generate code for a conversion to a C value
*/
CPtrAsBytes.prototype.jsToC = function (inVar)
{
    return 'byteArrayToPtr(' + inVar + ')';
};

/**
Generate code for a conversion from a C value
*/
CPtrAsBytes.prototype.cToJS = function (inVar)
{
    return 'ptrToByteArray(' + inVar + ')';
};

/**
Represents a C FFI function
*/
function CFunction(
    funcName,
    argTypes,
    retType,
    params
)
{
    assert (
        argTypes instanceof Array && retType !== undefined,
        'invalid arguments or return type'
    );
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    for (var i = 0; i < argTypes.length; ++i)
    {
        assert (
            argTypes[i].jsIRType instanceof IRType,
            'invalid argument type'
        );
    }

    assert (
        retType.jsIRType instanceof IRType,
        'invalid return type'
    );

    /**
    Name of the C function
    @field
    */
    this.funcName = funcName;

    /**
    Argument type mappings
    @field
    */
    this.argTypes = argTypes;

    /**
    Return type mapping
    @field
    */
    this.retType = retType;

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

    for (var i = 0; i < this.argTypes.length; ++i)
    {
        sourceStr += 'a' + i;        
        if (i !== this.argTypes.length - 1)
            sourceStr += ', ';
    }

    sourceStr += ')\n';
    sourceStr += '{\n';
    sourceStr += '\t"tachyon:static";\n';
    sourceStr += '\t"tachyon:noglobal";\n';
    sourceStr += '\t"tachyon:ret ' + this.retType.jsIRType + '";\n';

    for (var i = 0; i < this.argTypes.length; ++i)
    {
        var argType = this.argTypes[i].jsIRType;
        sourceStr += '\t"tachyon:arg a' + i + ' ' + argType + '";\n';
    }

    // Generate type conversions
    for (var i = 0; i < this.argTypes.length; ++i)
    {
        var argType = this.argTypes[i];
        var varName = 'a' + i;

        sourceStr += '\t' + varName + ' = ';
        sourceStr += argType.jsToC(varName) + ';\n';
    }
    
    var retVoid = (this.retType instanceof CVoid);

    sourceStr += '\t' + ((retVoid === true)? '':'var r = ') + 'iir.call_ffi(ffi_' + this.funcName;

    for (var i = 0; i < this.argTypes.length; ++i)
    {
        sourceStr += ', a' + i;
    }

    sourceStr += ');\n';

    // Free allocated C strings, if any
    for (var i = 0; i < this.argTypes.length; ++i)
    {
        var argType = this.argTypes[i];

        if (argType.freeAfterSnd !== true)
            continue;

        var varName = 'a' + i;

        sourceStr += '\t' + argType.free(varName) + ';\n';
    }

    // If we are returning a value
    if (retVoid === false)
    {
        // Convert the return value
        sourceStr += '\tvar rJS = ' + this.retType.cToJS('r') + ';\n';

        if (this.retType.freeAfterRcv === true)
        {
            sourceStr += '\t' + this.retType.free('r') + ';\n';
        }

        sourceStr += '\treturn rJS;\n';        
    }
    else
    {
        sourceStr += '\treturn;\n';
    }

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
    argTypes,
    retType,
    ctxVal
)
{
    assert (
        irFunction instanceof IRFunction,
        'expected IR function'
    );

    // The types presented to C must be specified
    assert (
        argTypes instanceof Array && retType !== undefined,
        'invalid C argument types or return type'
    );
    
    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    assert (
        irFunction.argTypes.length === argTypes.length,
        'C argument types do not match function argument types'
    );

    assert (
        ctxVal instanceof ConstValue ||
        ctxVal === undefined,
        'invalid context value'
    );

    // For now, assume the context is always passed as an argument
    assert (
        ctxVal === undefined,
        'cannot pre-specify fixed context'
    );

    // Find a free global name to call the function through
    var funcName = findFreeName(
        function (n) { return params.staticEnv.hasBinding(n); }, 
        irFunction.funcName + '_tproxy'
    );

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
    Argument types mappings
    @field
    */
    this.argTypes = argTypes;

    /**
    Return type mapping
    @field
    */
    this.retType = retType;

    /**
    Context pointer to be used. May be undefined if
    the context is to be passed by argument.
    @field
    */
    this.ctxVal = ctxVal;

    /**
    Compilation parameters to be used when compiling the proxy
    @field
    */
    this.params = params;
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
        sourceStr += 'ctxPtr';

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
    sourceStr += '\t"tachyon:ret ' + this.retType.cIRType + '";\n';

    if (this.ctxVal === undefined)
    {
        sourceStr += '\t"tachyon:arg ctxPtr rptr";\n';
    }

    for (var i = 0; i < this.irFunction.argTypes.length; ++i)
    {
        var argType = this.argTypes[
            this.argTypes.length - this.irFunction.argTypes.length + i
        ];

        sourceStr += '\t"tachyon:arg a' + i + ' ' + argType.cIRType + '";\n';
    }
    
    if (this.ctxVal === undefined)
    {
        sourceStr += '\tvar ctx = iir.icast(IRType.ref, ctxPtr);\n';
        sourceStr += '\tiir.set_ctx(ctx);\n';
    }

    // Get the global object from the context if available
    sourceStr += '\tvar global = ';
    if (this.ctxVal === undefined)
        sourceStr += '(ctxPtr !== NULL_PTR)? get_ctx_globalobj(ctx):UNDEFINED';
    else
        sourceStr += 'UNDEFINED';
    sourceStr += ';\n';

    // Convert the types of function arguments
    for (var i = 0; i < this.irFunction.argTypes.length; ++i)
    {
        var argType = this.argTypes[
            this.argTypes.length - this.irFunction.argTypes.length + i
        ];

        var jsIRType = this.irFunction.argTypes[
            this.irFunction.argTypes.length - this.argTypes.length + i
        ];

        var varName = 'a' + i;

        sourceStr += '\t' + varName + ' = ';
        sourceStr += argType.cToJS(varName, jsIRType) + ';\n';
    }
    
    var retVoid = (this.retType instanceof CVoid);

    sourceStr += '\t' + ((retVoid === true)? '':'var r = ') + 'iir.call(';
    sourceStr += this.funcName + ', UNDEFINED, global';

    for (var i = 0; i < this.irFunction.argTypes.length; ++i)
    {
        sourceStr += ', ' + 'a' + i;
    }

    sourceStr += ');\n';

    //sourceStr += '\tprintInt(13372);\n';

    if (retVoid === false)
    {
        sourceStr += '\treturn ' + this.retType.jsToC('r') + ';\n';
    }
    else
    {
        sourceStr += '\treturn;';
    }
    
    //sourceStr += 'return iir.icast(IRType.pint, 7);';

    sourceStr += '}\n';
    
    //print(sourceStr);

    // Compile the source string into an IR function
    var func = compileSrcString(sourceStr, this.params);

    // Return the compiled function
    return func.childFuncs[0];
};

/**
Create a bridge to call a compiled Tachyon function through V8
*/
function makeBridge(
    irFunction,
    params,
    argTypes,
    retType
)
{
    //print('entering makeBridge');

    assert (
        params instanceof CompParams,
        'expected compilation parameters'
    );

    // Generate a proxy for the function
    var proxy = new CProxy(
        irFunction,
        params,
        argTypes,
        retType
    );
    
    //print('generating C proxy');

    var wrapper = proxy.genProxy();

    //print(wrapper);
    
    // Get the C argument and return type names
    var cArgTypes = argTypes.map(function (t) { return t.cTypeName; });
    var cRetType = retType.cTypeName;

    //print('getting entry point for proxy');

    // Get pointer to entry point of compiled wrapper function
    var funcPtr = wrapper.linking.getEntryPoint('default').getAddr();

    // Callable bridge function
    function bridge(ctxPtr)
    {
        //print(ctxPtr);
        //print(funcPtr.getBytes());

        var argArray = [];
        for (var i = 1; i < arguments.length; ++i)
            argArray.push(arguments[i]);

        //print('calling callTachyonFFI');
        //print('Func ptr in Tachyon: ' + funcPtr.getBytes());

        var result = callTachyonFFI.apply(
            null,
            [
                cArgTypes,
                cRetType,
                funcPtr.getBytes(),
                ctxPtr
            ].concat(argArray)
        );

        //print('returned from callTachyonFFI');

        //print(result);

        return result;
    };

    return bridge;
}

// If we are running inside Tachyon
if (RUNNING_IN_TACHYON)
{
    /**
    Call a Tachyon function through a C FFI wrapper
    */
    var callTachyonFFI = function (cArgTypes, cRetType, funcPtrBytes, ctxPtrBytes)
    {
        "tachyon:noglobal";

        assert (
            boolToBox(boxIsArray(cArgTypes)),
            'argument types should be an array'
        );

        // Collect the call arguments into an array
        var argArray = [];
        for (var i = 4; i < arguments.length; ++i)
            argArray.push(arguments[i]);

        // Get raw pointer values for the function address and context pointer
        var funcPtr = byteArrayToPtr(funcPtrBytes);
        var ctxPtr = byteArrayToPtr(ctxPtrBytes);

        // Get the number of arguments
        var numArgs = unboxInt(cArgTypes.length);

        // Compute the size of the argument data
        var argDataSize = numArgs * PTR_NUM_BYTES;

        // Allocate memory for the argument data
        var argData = malloc(argDataSize);

        // Current offset into the argument data
        var curOffset = pint(0);

        // For each argument
        for (var i = pint(0); i < numArgs; ++i)
        {
            var cArgType = cArgTypes[boxInt(i)];
            var argVal = argArray[boxInt(i)];

            switch (cArgType)
            {
                case 'int':
                {
                    assert (
                        boolToBox(boxIsInt(argVal)),
                        'expected integer argument'
                    );

                    var arg = unboxInt(argVal);
                    iir.store(IRType.pint, argData, curOffset, arg);
                }
                break;

                case 'void*':
                {
                    assert (
                        boolToBox(boxIsArray(argVal)),
                        'expected byte array argument'
                    );

                    var arg = byteArrayToPtr(argVal);
                    iir.store(IRType.rptr, argData, curOffset, arg);
                }
                break;

                default:
                {
                    assert(
                        false,
                        'unsupported C argument type: "' + cArgType + '"'
                    );
                }
            };
            
            curOffset += PTR_NUM_BYTES;
        }

        //puts('calling FFI with rawCallTachyonFFI');
        //printPtr(funcPtr);
        //printPtr(ctxPtr);
        //printInt(numArgs);
        //printPtr(argData);

        // Call the function through the FFI interface
        var retValInt = rawCallTachyonFFI(
            funcPtr,
            ctxPtr,
            numArgs,
            argData
        );

        //puts('returned from FFI call');

        // Free the argument data
        free(argData);

        // Switch on the C return type
        switch (cRetType)
        {
            case 'int':
            {
                return boxInt(retValInt);
            }
            break;

            case 'void*':
            {
                return ptrToByteArray(iir.icast(IRType.rptr, retValInt));
            }
            break;

            default:
            {
                assert(
                    false,
                    'Unsupported C return type: "' + cRetType + '"'
                );
            }
        };
    };
}

/**
Initialize FFI functions for the current configuration
*/
function initFFI(params)
{
    log.trace('initFFI');

    function regFFI(ffiFunc)
    {
        params.ffiFuncs[ffiFunc.funcName] = ffiFunc;
        params.staticEnv.regBinding('ffi_' + ffiFunc.funcName, ffiFunc);
    }

    regFFI(new CFunction(
        'malloc', 
        [new CIntAsInt(IRType.pint)], 
        new CPtrAsPtr(),
        params
    ));

    regFFI(new CFunction(
        'free',
        [new CPtrAsPtr()],
        new CVoid(),
        params
    ));

    regFFI(new CFunction(
        'exit',
        [new CIntAsBox()],
        new CVoid(),
        params
    ));

    regFFI(new CFunction(
        'puts',
        [new CStringAsBox()], 
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'fopen',
        [new CStringAsBox(), new CStringAsBox()],
        new CPtrAsPtr(),
        params
    ));

    regFFI(new CFunction(
        'fclose',
        [new CPtrAsPtr()],
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'fputs',
        [new CStringAsBox(), new CPtrAsPtr()],
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'remove',
        [new CStringAsBox()],
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'printInt', 
        [new CIntAsInt(IRType.pint)],
        new CVoid(),
        params
    ));

    regFFI(new CFunction(
        'printPtr', 
        [new CPtrAsPtr()],
        new CVoid(),
        params
    ));

    regFFI(new CFunction(
        'sum2Ints', 
        [new CIntAsBox(), new CIntAsBox()],
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'getArgCount',
        [],
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'getArgVal',
        [new CIntAsBox()],
        new CStringAsBox(false),
        params
    ));

    regFFI(new CFunction(
        'writeFile',
        [new CStringAsBox(), new CStringAsBox()],
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'readFile',
        [new CStringAsBox()], 
        new CStringAsBox(),
        params
    ));

    regFFI(new CFunction(
        'shellCommand',
        [new CStringAsBox()], 
        new CStringAsBox(),
        params
    ));

    regFFI(new CFunction(
        'readConsole',
        [new CStringAsBox()], 
        new CStringAsBox(),
        params
    ));

    regFFI(new CFunction(
        'currentTimeMillis',
        [],
        new CIntAsBox(),
        params
    ));

    regFFI(new CFunction(
        'rawAllocMemoryBlock', 
        [new CIntAsInt(IRType.pint), new CBoolAsBool()], 
        new CPtrAsPtr(),
        params
    ));

    regFFI(new CFunction(
        'rawFreeMemoryBlock', 
        [new CPtrAsPtr(), new CIntAsInt(IRType.pint)], 
        new CVoid(),
        params
    ));

    regFFI(new CFunction(
        'gcCollect', 
        [new CPtrAsRef()],
        new CVoid(),
        params
    ));

    regFFI(new CFunction(
        'rawCallTachyonFFI', 
        [
            new CPtrAsPtr(),
            new CPtrAsPtr(),
            new CIntAsInt(IRType.pint),
            new CPtrAsPtr()
        ],
        new CIntAsInt(IRType.pint),
        params
    ));

    regFFI(new CFunction(
        'runtimeError',
        [new CStringAsBox(), new CIntAsBox()],
        new CVoid(),
        params
    ));

    regFFI(new CFunction(
        'getFuncAddr', 
        [new CStringAsBox()],
        new CPtrAsBytes(),
        params
    ));
}

