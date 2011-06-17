/**
@class Low-level machine code block implementation.
Stores generated machine code, external references and exposed labels.
*/
function CodeBlock(size)
{
    /**
    @field Executable memory block
    */
    this.memBlock = allocMemoryBlock(size, true);

    /**
    @field Memory block size
    */
    this.size = size;

    /**
    @field Current writing position
    */
    this.writePos = 0;
}

/**
Print the code block as a string
*/
CodeBlock.prototype.toString = function ()
{
    var str = '';

    for (var i = 0; i < this.writePos; ++i)
    {
        var byteVal = readFromMemoryBlock(this.memBlock, i);

        var byteStr = byteVal.toString(16);
        byteStr = byteStr.toUpperCase();
        if (byteStr.length === 1)
            byteStr = '0' + byteStr;

        if (i !== 0)
            str += ' ';

        str += byteStr;
    }

    return str;
}

/**
Clear the contents of the code block
*/
CodeBlock.prototype.clear = function ()
{
    this.writePos = 0;
}

/**
Write a byte at a given position
*/
CodeBlock.prototype.writeByte = function (val)
{
    assert (
        this.memBlock,
        'invalid memory block'
    );

    assert (
        this.writePos + 1 <= this.size,
        'no room to write byte in code block'
    );

    assert (
        isNonNegInt(val) && val < 255,
        'invalid byte value: ' + val
    );

    writeToMemoryBlock(this.memBlock, this.writePos, val);

    this.writePos += 1;
}


//
// TODO: write 8, 32, 64bit values, byte by byte
//



