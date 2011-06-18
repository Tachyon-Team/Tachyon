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
        var byteVal = this.readByte(i);

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
Read a byte at a given position
*/
CodeBlock.prototype.readByte = function (idx)
{
    assert (
        idx < this.size,
        'invalid code block index'
    );

    return readFromMemoryBlock(this.memBlock, idx);
}

/**
Write a byte at the current position
*/
CodeBlock.prototype.writeByte = function (val)
{
    assert (
        this.memBlock,
        'invalid memory block'
    );

    assert (
        this.writePos + 1 <= this.size,
        'no space to write byte in code block ' +
        '(pos ' + this.writePos + '/' + this.size + ')'
    );

    assert (
        isNonNegInt(val) && val < 255,
        'invalid byte value: ' + val
    );

    writeToMemoryBlock(this.memBlock, this.writePos, val);

    this.writePos += 1;
}

/**
Write a word (16-bit) at the given position
*/
CodeBlock.prototype.writeWord = function (val)
{
    assert (
        num_ge(val, getIntMin(16)) && num_le(val, getIntMax(16)),
        'invalid word value: ' + val
    );

    this.writeByte((val & 0xFF00) >> 8);
    this.writeByte(val & 0xFF);
}


//
// TODO: write 8, 32, 64bit values, byte by byte
//

