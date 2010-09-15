/**
@fileOverview
Code to describe and implement the memory layout of allocatable objects.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Represents a field specification for a memory-allocatable object
*/
function FieldSpec(name, type, typeSize, numElems, offset)
{
    assert (
        name,
        'must specify field name'
    );

    assert (
        type instanceof IRType || type instanceof ObjectLayout,
        'invalid field type'
    );

    assert (
        !(type instanceof IRType && typeSize != undefined),
        'type size cannot be set for IR values'
    );

    /**
    Field name
    @field
    */
    this.name = name;

    /**
    Field type
    May be IR type or sub-object layout
    @field
    */
    this.type = type;

    /**
    Size parameter of the sub-object
    Undefined if not applicable
    @field
    */
    this.typeSize = typeSize;

    /**
    Number of elements in this field
    Undefined if variable count
    @field
    */
    this.numElems = numElems;

    /**
    Memory offset
    @field
    */
    this.offset = offset;
}

/**
@class Represents the memory layout of allocatable objects
*/
function ObjectLayout(name, ptrType)
{
    // If no pointer type is specified, use the boxed type
    if (!ptrType)
        ptrType = IRType.box;

    /**
    Name of the layout
    @field
    */
    this.name = name;

    /**
    Pointer type used to refer to the object
    @field
    */
    this.ptrType = ptrType;

    /**
    List of fields
    @field
    */
    this.fields = [];

    /**
    Map of variable names to field objects
    @field
    */
    this.fieldMap = {};

    /**
    Indicates if new variables cannot be registered
    @field
    */
    this.finalized = false;
}
ObjectLayout.prototype = {}

/**
Get the current size of an object using this layout
*/
ObjectLayout.prototype.getSize = function (typeSize)
{
    assert (
        typeSize || lastField.numElems &&
        !(typeSize && lastField.numElems),
        'must specify type size for variable-length layouts'
    );

    // Get the last field of the layout
    var lastField = this.fields[this.fields.length - 1];
    
    // Compute the size of one element of this field
    var elemSize =
        (lastField.type instanceof ObjectLayout)?
        lastField.type.size:
        lastField.type.getSize(typeSize)
    ;
    
    // Get the number of elements in the last field
    var numElems = typeSize? typeSize:lastField.numElems;

    // Compute the total size of the object
    var size = lastField.offset + elemSize * numElems;

    // Return the object size
    return size;
}

/**
Add a new field specification
*/
ObjectLayout.prototype.addField = function(name, type, typeSize, numElems)
{
    assert (
        name,
        'must specify object layout name'
    );

    assert (
        this.fieldMap[name] === undefined,
        'layout field already registered: "' + name + '"'
    );

    assert (
        !this.finalized,
        'object layout is finalized'
    );

    assert (
        !(numElems === undefined && 
          this.fields.length > 0 &&
          this.fields[this.fields.length-1].numElems === undefined),
        'only the last field can have variable length'
    );

    // If the layout is empty
    if (this.fields.length == 0)
    {
        // The offset is zero
        var offset = 0;
    }
    else
    {
        // Compute the offset to be after the last field
        var offset = this.size();
    }

    //
    // TODO: alignment?
    //

    // Create a new field-specification object
    var newField = new FieldSpec(name, type, typeSize, numElems, offset);

    // Add the new field to the list
    this.fields.push(newField);

    // Add the new field to the map
    this.fieldMap[name] = newField;
}

/**
Get a field specification by name
*/
ObjectLayout.prototype.getField = function (name)
{
    assert (
        this.fieldMap[name] !== undefined,
        'layout variable not found: "' + name + '"'
    );

    return this.fieldMap[name];
}

/**
Lock the layout so that it can no longer be changed
*/
ObjectLayout.prototype.finalize = function ()
{
    this.finalized = true;
}

/**
Generate functions to manipulate a given layout
*/
ObjectLayout.prototype.genMethods = function ()
{
    assert (
        this.finalized,
        'layout must be finalized before generating methods'
    );

    /**
    Generate a function from a source string
    */
    function genIRFunc(sourceStr)
    {
        // Parse the source string
        var ast = parse_src_str(sourceStr);

        // Translate the AST to IR
        var ir = unitToIR(ast, true);

        // Lower the IR
        lowerIRFunc(ir);

        // Validate the IR
        ir.validate();

        // Return the IR of the function
        return ir.childFuncs[0];
    }

    // For each field
    for (var fname in this.fieldMap)
    {
        // Get the field specification for this field
        var spec = this.fieldMap[fname];

        //
        // TODO: allocator function
        // PROBLEM: will need to call function to allocate object
        //

        // Generate code for the getter function
        var getterStr = "";
        getterStr += "function " + this.name + "_get_" + fname;
        getterStr += "(objPtr" + (spec.numElems? ", index":"") + ")";
        getterStr += "{";
        getterStr += "\"tachyon:inline\";";
        getterStr += "\"tachyon:arg objPtr " + this.ptrType + "\";";
        if (spec.numElems)
            getterStr += "\"tachyon:arg index pint\";"
        if (spec.type instanceof ObjectLayout)
        {
            getterStr += "\"tachyon:ret rptr\";";
            getterStr += "var offset = iir.constant(IRType.pint, " + spec.offset + ");"
            if (spec.numElems)
                getterStr += "offset += index * " + spec.type.size(spec.type.typeSize) + ";";
            getterStr += "return objPtr + offset;"
        }
        else
        {
            getterStr += "\"tachyon:ret " + spec.type + "\";";
            getterStr += "var offset = iir.constant(IRType.pint, " + spec.offset + ");"
            if (spec.numElems)
                getterStr += "offset += index * " + spec.type.size + ";";
            getterStr += "return iir.load(IRType." + spec.type + ", objPtr, offset);";
        }
        getterStr += "}";

        // Generate IR for the getter function
        var getterFunc = genIRFunc(getterStr);

        // Register the getter function
        staticEnv.regBinding(getterFunc.funcName, getterFunc);

        // If there can be a setter for this field
        if (spec.type instanceof IRType)
        {
            // Generate code for the getter function
            var setterStr = "";
            setterStr += "function " + this.name + "_set_" + fname;
            setterStr += "(objPtr" + (spec.numElems? ", index":"") + ", value)";
            setterStr += "{";
            setterStr += "\"tachyon:inline\";";
            setterStr += "\"tachyon:arg objPtr " + this.ptrType + "\";";
            if (spec.numElems)
                setterStr += "\"tachyon:arg index pint\";"
            setterStr += "\"tachyon:arg value " + spec.type + "\";"
            setterStr += "var offset = iir.constant(IRType.pint, " + spec.offset + ");"
            if (spec.numElems)
                setterStr += "offset += index * " + spec.type.size + ";";
            setterStr += "iir.store(IRType." + spec.type + ", objPtr, offset, value);";
            setterStr += "}";

            // Generate IR for the setter function
            var setterFunc = genIRFunc(setterStr);
           
            // Register the setter function
            staticEnv.regBinding(setterFunc.funcName, setterFunc);
        }
    }
}

//
// TODO
//


var ctxLayout = new ObjectLayout('ctx');


ctxLayout.addField(
    'len',
    IRType.i32
);


ctxLayout.finalize();


//ctxLayout.genMethods();


/**
Generate an instruction to read a variable from the context
*/
/*
ContextLayout.prototype.genCtxLoad = function (ctxPtr, varName)
{
    // Get the corresponding context variable
    var ctxVar = contextLayout.getVar(varName);

    return new LoadInstr(
        ctxVar.type,
        ctxPtr,
        ConstValue.getConst(
            ctxVar.offset,
            IRType.pint
        )
    );
}
*/

/**
Generate an instruction to write a variable to the context
*/
/*
ContextLayout.prototype.genCtxStore = function (ctxPtr, varName, newVal)
{
    // Get the corresponding context variable
    var ctxVar = contextLayout.getVar(varName);

    return new StoreInstr(
        ctxVar.type,
        ctxPtr,
        ConstValue.getConst(
            ctxVar.offset,
            IRType.pint
        ),
        newVal
    );
}
*/

