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
    Size of an element of this field
    @field
    */
    this.elemSize = 
        (type instanceof IRType)?
        type.size:
        this.type.getSize(typeSize);

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
function ObjectLayout(name, ptrType, tagName)
{
    // Ensure that no layout with this name exists
    assert (
        !ObjectLayout.layoutMap[name],
        'an object layout with this name already exists'
    );

    // Ensure that a tag is specified for boxed references
    assert (
        !(ptrType === IRType.box && !tagName),
        'tag name must be specified for boxed references'
    );

    // Store the layout in the layout map
    ObjectLayout.layoutMap[name] = this;

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
    Tag used to tag the object (for boxed references only)
    @field
    */
    this.tagName = tagName;

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
ObjectLayout.prototype = {};

/**
Map of layout names to object layouts
*/
ObjectLayout.layoutMap = {};

/**
Source code for generated layout functions
*/
ObjectLayout.sourceStr = '';

/**
Get the current size of an object using this layout
*/
ObjectLayout.prototype.getSize = function (typeSize)
{
    // If there are no fields, the size is 0
    if (this.fields.length == 0)
        return 0;

    // Get the last field of the layout
    var lastField = this.fields[this.fields.length - 1];

    assert (
        typeSize || lastField.numElems &&
        !(typeSize && lastField.numElems == Infinity),
        'must specify type size for variable-length layouts'
    );
    
    // Get the number of elements in the last field
    var numElems = typeSize? typeSize:lastField.numElems;

    // Compute the total size of the object
    var size = lastField.offset + lastField.elemSize * numElems;

    // Return the object size
    return size;
};

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

    if (type instanceof IRType && numElems === undefined)
        numElems = 1;

    assert (
        !(numElems === Infinity && 
          this.fields.length > 0 &&
          this.fields[this.fields.length-1].numElems === Infinity),
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
        var offset = this.getSize();
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
};

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
};

/**
Lock the layout so that it can no longer be changed
*/
ObjectLayout.prototype.finalize = function ()
{
    this.finalized = true;
};

/**
Generate the offset computation to access a given field or sub-field
@arg query list of field name strings and index values
*/
ObjectLayout.prototype.genfieldAccessIR = function (context, query)
{
    assert (
        query.length > 0,
        'empty field access query'
    );

    assert (
        this.ptrType,
        'cannot generate access IR for layout, pointer type unspecified (' +
        this.name + ')'
    );

    // Declare a variable for the current offset value
    var curOffset = ConstValue.getConst(0, IRType.pint);

    // Initialize the current type
    var curType = this;

    // For each element of the query
    for (var i = 0; i < query.length; ++i)
    {
        var fieldName = query[i];
        var spec = curType.fieldMap[fieldName];

        assert (
            spec instanceof FieldSpec,
            'field not found: "' + fieldName + '"'
        );

        // Move to the layout of the field, if applicable
        var curType = spec.type;

        // Add the field offset to the total offset
        var fieldOffset = ConstValue.getConst(spec.offset, IRType.pint);
        curOffset = context.addInstr(new AddInstr(curOffset, fieldOffset));
        
        // If an index is supplied
        if (query[i+1] instanceof IRValue)
        {
            var fieldIndex = query[i+1];
            ++i;

            // Add the index offset to the total offset
            var fieldSize = ConstValue.getConst(spec.elemSize, IRType.pint);
            var idxOffset = context.addInstr(new MulInstr(fieldSize, fieldIndex));
            context.addInstr(new AddInstr(curOffset, idxOffset));
        }
    }

    // Return the computed offset value
    return { offset:curOffset, type:curType };
};

/**
Generate functions to manipulate a given layout
*/
ObjectLayout.prototype.genMethods = function ()
{
    assert (
        this.finalized,
        'layout must be finalized before generating methods'
    );

    assert (
        this.ptrType,
        'cannot generate methods for layout, pointer type unspecified (' +
        this.name + ')'
    );

    // Source string to store the generated code
    var sourceStr = '';

    // Get a reference to the last field
    var lastField = this.fields[this.fields.length-1];

    // Get the number of elements in the last field
    var numElems = lastField.numElems;

    // If the number of elements is not variable
    if (numElems !== Infinity)
    {
        // Compute the object size
        var objSize = lastField.offset + lastField.elemSize * numElems;

        // Generate code for the size function
        sourceStr += 'function get_size_' + this.name + '()\n';
        sourceStr += '{\n';
        sourceStr += '\t"tachyon:inline";\n';
        sourceStr += '\t"tachyon:ret pint";\n';
        sourceStr += '\treturn iir.constant(IRType.pint, ' + objSize + ');\n';
        sourceStr += '}\n';
        sourceStr += '\n';

        // Generate code for the allocation function
        sourceStr += 'function alloc_' + this.name + '()\n';
        sourceStr += '{\n';
        sourceStr += '\t"tachyon:inline";\n';
        sourceStr += '\t"tachyon:ret ' + this.ptrType + '";\n';
        sourceStr += '\tvar ptr = heapAlloc(get_size_' + this.name + '());\n';
        if (this.ptrType === IRType.box)
            sourceStr += '\treturn boxRef(ptr, ' + this.tagName + ');\n';
        else
            sourceStr += '\treturn ptr;\n';
        sourceStr += '}\n';
        sourceStr += '\n';
    }
    else
    {
        // Generate code for the size function
        sourceStr += 'function get_size_' + this.name + '(size)\n';
        sourceStr += '{\n';
        sourceStr += '\t"tachyon:inline";\n';
        sourceStr += '\t"tachyon:arg size pint";\n';
        sourceStr += '\t"tachyon:ret pint";\n';
        sourceStr += '\tvar baseSize = iir.constant(IRType.pint, ' + lastField.offset + ');\n';
        sourceStr += '\tvar elemSize = iir.constant(IRType.pint, ' + lastField.elemSize + ');\n';
        sourceStr += '\tvar objSize = baseSize + elemSize * size;\n';
        sourceStr += '\treturn objSize;\n';
        sourceStr += '}\n';
        sourceStr += '\n';

        // Generate code for the allocation function
        sourceStr += 'function alloc_' + this.name + '(size)\n';
        sourceStr += '{\n';
        sourceStr += '\t"tachyon:inline";\n';
        sourceStr += '\t"tachyon:arg size pint";\n';
        sourceStr += '\t"tachyon:ret ' + this.ptrType + '";\n';
        sourceStr += '\tvar ptr = heapAlloc(get_size_' + this.name + '(size));\n';
        if (this.ptrType === IRType.box)
            sourceStr += '\treturn boxRef(ptr, ' + this.tagName + ');\n';
        else
            sourceStr += '\treturn ptr;\n';
        sourceStr += '}\n';
        sourceStr += '\n';
    }

    // Generate code for the accessor functions for a given layout
    function genAccessFuncs(
        curLayout, 
        nameStr, 
        argStr, 
        numArgs,
        proStr, 
        offsetStr
    )
    {
        // For each field
        for (var fname in curLayout.fieldMap)
        {
            // Get the field specification for this field
            var spec = curLayout.fieldMap[fname];

            // Generate code for this field and sub-fields
            genAccessField(
                spec, 
                nameStr, 
                argStr, 
                numArgs, 
                proStr, 
                offsetStr
            );
        }        

        // Generate code for a given field and sub-fields
        function genAccessField(
            spec, 
            nameStr, 
            argStr,
            numArgs,
            proStr,
            offsetStr
        )
        {
            // Add the field name to the name string
            nameStr += '_' + fname;

            // Add the field offset to the current offset
            offsetStr += 'offset += iir.constant(IRType.pint, ' + spec.offset + ');\n';

            // If there are many elements, or a variable number of elements
            if (spec.numElems != 1)
            {
                // Create an index variable for this field
                var idxVar = 'idx' + (numArgs - 1);
                numArgs += 1;
                proStr += '"tachyon:arg ' + idxVar + ' pint";\n';

                // Integrate the index argument in the computation
                argStr += ', ' + idxVar;
                offsetStr +=
                    'offset += iir.constant(IRType.pint, ' + spec.elemSize +
                    ') * ' + idxVar + ';\n';
                ;
            }

            // If there can't be accessors for this field
            if (spec.type instanceof ObjectLayout)
            {
                // Recurse on the layout
                genAccessFuncs(
                    spec.type,
                    nameStr, 
                    argStr, 
                    numArgs, 
                    proStr, 
                    offsetStr
                );

                // Do not generate accessor methods
                return;
            }

            // Generate the getter method
            sourceStr += 'function get_' + nameStr + '(' + argStr + ')\n';
            sourceStr += '{\n';
            sourceStr += indentText(proStr);
            sourceStr += '\t"tachyon:ret ' + spec.type + '";\n';
            sourceStr += indentText(offsetStr);
            sourceStr += '\treturn iir.load(IRType.' + spec.type + ', obj, offset);\n';
            sourceStr += '}\n';
            sourceStr += '\n';

            // Generate the setter method
            sourceStr += 'function set_' + nameStr + '(' + argStr + ', val)\n';
            sourceStr += '{\n';
            sourceStr += indentText(proStr);
            sourceStr += '\t"tachyon:arg val ' + spec.type + '";\n';
            sourceStr += indentText(offsetStr);
            sourceStr += '\tiir.store(IRType.' + spec.type + ', obj, offset, val);\n';
            sourceStr += '}\n';
            sourceStr += '\n';
        }
    }

    // Generate the getter and setter functions
    genAccessFuncs(
        this, 
        this.name, 
        'obj', 
        1,
        '"tachyon:arg obj ' + this.ptrType + '";\n' +
        '"tachyon:inline";\n',
        'var offset = iir.constant(IRType.pint, 0);\n'
    );

    // Append the generated code to the object layout source string
    ObjectLayout.sourceStr += sourceStr;
};

