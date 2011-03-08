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
function FieldSpec(name, type, initVal, subSize, elemSize, numElems, offset)
{
    assert (
        name !== undefined,
        'must specify field name'
    );

    assert (
        type instanceof IRType || type instanceof MemLayout,
        'invalid field type'
    );

    assert (
        !(type instanceof IRType && subSize !== undefined),
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
    Field initialization value
    @field
    */
    this.initVal = initVal;

    /**
    Size parameter of the sub-object
    Undefined if not applicable
    @field
    */
    this.subSize = subSize;

    /**
    Size of an element of this field
    @field
    */
    this.elemSize = elemSize;

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
function MemLayout(name, ptrType, tagName, params)
{
    // Ensure that no layout with this name exists
    assert (
        params.memLayouts[name] === undefined,
        'an object layout with this name already exists'
    );

    // Ensure that a tag is specified for boxed references
    assert (
        !(ptrType === IRType.box && tagName === undefined),
        'tag name must be specified for boxed references'
    );

    // Ensure that the compilation parameters are valid
    assert (
        params instanceof CompParams,
        'compilation parameters needed'
    );

    // Store the layout in the layout map
    params.memLayouts[name] = this;

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
    Compilation parameters for this layout
    */
    this.params = params;

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

    // If this is a heap-allocated object
    if (ptrType === IRType.box || ptrType === IRType.ref)
    {
        // Add a header as the first layout field
        this.addField(
            'header',
            IRType.u32
        );

        // Assign a unique type identifier to this layout
        var typeId = 0;
        for (var f in params.memLayouts)
            ++typeId;
    }

    /**
    Integer type identifier for this layout. This only applies
    to heap-allocated objects.
    @field
    */
    this.typeId = typeId;
}
MemLayout.prototype = {};

/**
Create a new layout by extending another one
*/
MemLayout.extend = function (baseLayout, name, tagName)
{
    assert (
        baseLayout instanceof MemLayout,
        'invalid base layout'
    );
    assert (
        typeof name === 'string',
        'invalid layout name'
    );
    assert (
        !(baseLayout.ptrType === IRType.box && typeof tagName !== 'string'),
        'invalid tag name'
    );

    // Create an object for the new layout
    var newLayout = new MemLayout(
        name, 
        baseLayout.ptrType, 
        tagName, 
        baseLayout.params
    );

    assert (
        baseLayout.fields.length !== 0,
        'cannot extend empty layout'
    );

    assert (
        !(baseLayout.fields[baseLayout.fields.length-1].numElems === false),
        'cannot extend variable-length layouts'
    );

    assert (
        baseLayout.finalized,
        'can only extend finalized layouts'
    );

    // Copy the fields from the base layout
    newLayout.fields = baseLayout.fields.slice(0);

    // Copy the field names from the base layout
    for (var key in baseLayout.fieldMap)
        newLayout.fieldMap[key] = baseLayout.fieldMap[key];

    // Return the new layout object
    return newLayout;
};

/**
Get the current size of an object using this layout
*/
MemLayout.prototype.getSize = function (subSize)
{
    // If there are no fields, the size is 0
    if (this.fields.length === 0)
        return 0;

    // Get the last field of the layout
    var lastField = this.fields[this.fields.length - 1];

    assert (
        subSize !== undefined || lastField.numElems &&
        !(subSize !== undefined && lastField.numElems === false),
        'must specify type size for variable-length layouts'
    );

    // Get the number of elements in the last field
    var numElems = (subSize !== undefined) ? subSize : lastField.numElems;

    // Compute the total size of the object
    var size = lastField.offset + lastField.elemSize * numElems;

    // Return the object size
    return size;
};

/**
Add a new field specification
@param type type of the element(s) in this field.
@param subSize size parameter of the sub-object, for variable-size objects.
@param numElems number of elements in the field. Use value false for a
                variable-size field.
*/
MemLayout.prototype.addField = function(name, type, initVal, subSize, numElems)
{
    assert (
        name !== undefined,
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
        initVal === undefined || typeof initVal == 'string',
        'the initialization value, if specified, must be a string'
    );

    assert (
        !(numElems === false &&
          this.fields.length > 0 &&
          this.fields[this.fields.length-1].numElems === false),
        'only the last field can have variable length'
    );

    // If this is a variable-length field, add a size field before it
    if (numElems === false)
    {
        this.addField(
            'size',
            IRType.pint
        );
    }

    // If the layout is empty
    if (this.fields.length === 0)
    {
        // The offset is zero
        var offset = 0;
    }
    else
    {
        // Compute the offset to be after the last field
        var offset = this.getSize();

        // Align the offset to the pointer size of the platform
        var align = this.params.target.ptrSizeBytes;
        var rem = offset % align;
        if (rem !== 0)
        {
            offset += align - rem;
        }
    }

    // Compute the element size for this field
    var elemSize = 
        (type instanceof IRType)?
        type.getSizeBytes(this.params):
        type.getSize(subSize);

    // Create a new field-specification object
    var newField = new FieldSpec(
        name,
        type,
        initVal,
        subSize,
        elemSize,
        numElems,
        offset
    );

    // Add the new field to the list
    this.fields.push(newField);

    // Add the new field to the map
    this.fieldMap[name] = newField;
};

/**
Get a field specification by name
*/
MemLayout.prototype.getField = function (name)
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
MemLayout.prototype.finalize = function ()
{
    this.finalized = true;
};

/**
Compute a fixed offset to access a given field or sub-field
*/
MemLayout.prototype.getFieldOffset = function (query)
{
    assert (
        query.length > 0,
        'empty field access query'
    );

    assert (
        this.ptrType !== undefined,
        'cannot generate access IR for layout, pointer type unspecified (' +
        this.name + ')'
    );

    // Declare a variable for the current offset value
    var curOffset = 0;

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

        // Update the total offset
        curOffset += spec.offset;

        // If an index is supplied
        if ((typeof query[i+1]) === "number")
        {
            var fieldIndex = query[i+1];
            ++i;

            // Add the index offset to the total offset
            var fieldSize = spec.elemSize;
            var idxOffset = fieldSize * fieldIndex;
            curOffset += idxOffset;
        }
        else
        {
            // Ensure that an index is supplied if the current
            // field has variable size
            assert (
                spec.numElems !== false,
                'must specify index for variable size fields'
            );
        }
    }

    // Return the computed offset value
    return curOffset;
};

/**
Generate the offset computation to access a given field or sub-field
@arg query list of field name strings and index values
*/
MemLayout.prototype.genfieldAccessIR = function (context, query)
{
    assert (
        query.length > 0,
        'empty field access query'
    );

    assert (
        this.ptrType !== undefined,
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
        else
        {
            // Ensure that an index is supplied if the current
            // field has variable size
            assert (
                spec.numElems !== false,
                'must specify index for variable size fields'
            );
        }
    }

    // Return the computed offset value
    return { offset:curOffset, type:curType };
};

/**
Test if instances of objects can be generated using this layout
*/
MemLayout.prototype.isInstantiable = function ()
{
    return this.ptrType !== undefined;
};

/**
Apply a function to each field and sub-field of this layout, in order
*/
MemLayout.prototype.forEachField = function (fieldFunc)
{
    function forEachField(
        rootLayout,
        curLayout,
        fieldSpecs,
        fieldFunc
    )
    {
        // For each field in the current layout
        for (var fname in curLayout.fieldMap)
        {
            // Get the field specification for this field
            var spec = curLayout.fieldMap[fname];

            // If this is a sub-layout, not a leaf field
            if (spec.type instanceof MemLayout)
            {
                // Call this function recursively
                forEachField(
                    rootLayout,
                    spec.type,
                    fieldSpecs.concat(spec),
                    fieldFunc
                );
            }
            else
            {
                // Call the field function
                fieldFunc(
                    rootLayout,
                    spec,
                    fieldSpecs.concat(spec)
                );
            }
        }
    }

    forEachField(
        this,
        this,
        [],
        fieldFunc
    );
};

/**
Generate functions to manipulate a given layout
*/
MemLayout.prototype.genMethods = function ()
{
    assert (
        this.finalized,
        'layout must be finalized before generating methods'
    );

    assert (
        this.ptrType !== undefined,
        'cannot generate methods for layout, pointer type unspecified (' +
        this.name + ')'
    );

    // Source string to store the generated code
    var sourceStr = '';

    // Get a reference to the last field
    var lastField = this.fields[this.fields.length-1];

    // Get the number of elements in the last field
    var numElems = lastField.numElems;

    // Test if this layout has a variable size
    var varSize = (numElems === false);

    // Generate code for the size computation function
    sourceStr += 'function comp_size_' + this.name + '(' +
                 (varSize? 'size':'') + ')\n';
    sourceStr += '{\n';
    sourceStr += '\t"tachyon:inline";\n';
    sourceStr += '\t"tachyon:noglobal";\n';
    if (varSize)    
        sourceStr += '\t"tachyon:arg size pint";\n';
    sourceStr += '\t"tachyon:ret pint";\n';
    sourceStr += '\tvar baseSize = pint(' + lastField.offset + ');\n';
    sourceStr += '\tvar elemSize = pint(' + lastField.elemSize + ');\n';
    if (!varSize)
        sourceStr += '\tvar size = pint(' + numElems + ');\n';
    sourceStr += '\tvar objSize = baseSize + elemSize * size;\n';
    sourceStr += '\treturn objSize;\n';
    sourceStr += '}\n';
    sourceStr += '\n';

    // Generate code for the sizeof function
    sourceStr += 'function sizeof_' + this.name + '(obj' +
                 (varSize? ', size':'') + ')\n';
    sourceStr += '{\n';
    sourceStr += '\t"tachyon:inline";\n';
    sourceStr += '\t"tachyon:noglobal";\n';
    sourceStr += '\t"tachyon:arg obj ' + this.ptrType + '";\n';
    sourceStr += '\t"tachyon:ret pint";\n';
    if (varSize)
        sourceStr += '\tvar size = get_' + this.name + '_size(obj);\n';
    sourceStr += '\treturn comp_size_' + this.name + '(' +
                 (varSize? 'size':'') + ');\n';
    sourceStr += '}\n';
    sourceStr += '\n';

    // Function to generate the field initialization code
    function genInitCode(
        rootLayout,
        curLayout,
        fieldSpecs,
        args,
        fname
    )
    {
        // String for the generated code
        var initCode = '';

        // For each field in the current layout
        for (var field in curLayout.fieldMap)
        {
            // Get the field specification for this field
            var spec = curLayout.fieldMap[field];

            // By defaults, no new arguments are added
            var curArgs = args;

            // If there are many elements, or a variable number of elements
            if (spec.numElems !== 1)
            {
                var varName = 'i' + fieldSpecs.length;

                // Add the index variable to the argument string
                curArgs += ', ' + varName;
            }

            // Add the spec name to the field name
            var curFieldName = fname + '_' + spec.name;

            // String for the code generated at higher nesting levels
            var subSrc = '';

            // If this is a sub-layout, not a leaf field
            if (spec.type instanceof MemLayout)
            {
                // Call this function recursively
                subSrc = genInitCode(
                    rootLayout,
                    spec.type,
                    fieldSpecs.concat(spec),
                    curArgs,
                    curFieldName
                );
            }
            else
            {
                // If there is an initialization value for this field
                if (spec.initVal !== undefined)
                {
                    subSrc = 'set_' + rootLayout.name + curFieldName + 
                        '(' + curArgs + ', ' + spec.initVal + ');\n';
                }
            }

            // If code was generated at higher nesting levels
            if (subSrc)
            {
                // If there are many elements, or a variable number of elements
                if (spec.numElems !== 1)
                {
                    // Generate code to loop over each element
                    initCode += 'for (var ' + varName + ' = pint(0); ';
                    initCode += varName + ' < ';
                    initCode += (spec.numElems !== false)? 
                            'pint(' + spec.numElems + ')':'size'
                    initCode += '; ++' + varName + ')\n';
                    initCode += '{\n';

                    initCode += indentText(subSrc);

                    initCode += '}\n';
                }
                else
                {
                    initCode += subSrc;
                }
            }

        }

        // Return the generated code
        return initCode;
    }

    // Generate the field initialization code
    var initCode = genInitCode(
        this,
        this,
        [],
        'ptr',
        ''
    );

    // Function to generate the allocation function code
    function genAllocCode(namePrefix, layout, initCode)
    {
        var sourceStr = '';

        sourceStr += 'function ' + namePrefix + '_' + layout.name + '(' +
                     (varSize? 'size':'') + ')\n';
        sourceStr += '{\n';
        sourceStr += '\t"tachyon:inline";\n';
        sourceStr += '\t"tachyon:noglobal";\n';
        if (varSize)
            sourceStr += '\t"tachyon:arg size pint";\n';
        sourceStr += '\t"tachyon:ret ' + layout.ptrType + '";\n';

        sourceStr += '\tvar ptr = heapAlloc(comp_size_' + layout.name + '(' +
                     (varSize? 'size':'') + '));\n';

        // If this layout uses a boxed reference type, box the pointer
        if (layout.ptrType === IRType.box)
            sourceStr += '\tptr = boxPtr(ptr, ' + layout.tagName + ');\n';

        // If the layout has a variable size, set its size
        if (varSize)
            sourceStr += '\tset_' + layout.name + '_size(ptr, size);\n';

        // If this is a heap-allocated layout, set its type id
        if (layout.ptrType === IRType.box || layout.ptrType === IRType.ref)
            sourceStr += '\tset_' + layout.name + '_header(ptr, u32(' + layout.typeId + '));\n';

        // If initialization code was specified, include it
        if (initCode)
            sourceStr += indentText(initCode);

        sourceStr += '\treturn ptr;\n';
        sourceStr += '}\n';
        sourceStr += '\n';

        return sourceStr
    }

    // Generate the allocation function with initialization
    sourceStr += genAllocCode('alloc', this, initCode);

    // Generate the allocation function without initialization
    sourceStr += genAllocCode('alloc_noinit', this);

    // Generate code for the accessor functions for this layout
    this.forEachField(
        function (layout, spec, fieldSpecs)
        {
            // String for the accessor name
            var nameStr = layout.name;

            // String for the argument names
            var argStr = 'obj';

            // Number of arguments
            var numArgs = 1;

            // String for the prologue annotations
            var proStr = '';
            proStr += '"tachyon:arg obj ' + layout.ptrType + '";\n';
            proStr += '"tachyon:inline";\n';
            proStr += '"tachyon:noglobal";\n';

            // String for the offset computation
            var offsetStr = '';
            offsetStr += 'var offset = pint(0);\n';

            // For each field spec
            fieldSpecs.forEach(
                function (spec, idx)
                {
                    // Add the field name to the name string
                    nameStr += '_' + spec.name;

                    // Add the field offset to the current offset
                    offsetStr += 'offset += pint(' + spec.offset + ');\n';

                    // If there are many elements, or a variable number of elements
                    if (spec.numElems !== 1)
                    {
                        // Create an index variable for this field
                        var idxVar = 'idx' + (numArgs - 1);
                        numArgs += 1;
                        proStr += '"tachyon:arg ' + idxVar + ' pint";\n';

                        // Integrate the index argument in the computation
                        argStr += ', ' + idxVar;
                        offsetStr +=
                            'offset += pint(' + spec.elemSize +
                            ') * ' + idxVar + ';\n';
                        ;
                    }
                }
            );

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
    );

    // Return the generated code
    return sourceStr;
};

/**
Generate C code to manipulate this layout
*/
MemLayout.prototype.genCMethods = function ()
{
    //
    // TODO
    //




};

