/**
x86 namespace
*/
var x86 = x86 || {};

/**
@class Assembler to assemble a function or block of assembler code.
*/
x86.Assembler = function (x86_64)
{
    assert (
        x86_64 === true || x86_64 === false,
        'must set assembler x86-64 flag'
    );

    /**
    @field x86-64 mode
    */
    this.x86_64 = x86_64;

    /**
    @field First instruction in the block
    */
    this.firstInstr = null;

    /**
    @field Last instruction in the block
    */
    this.lastInstr = null;

    /**
    @field Number of instructions
    */
    this.numInstrs = 0;
}

/**
Produce a string representation of the code block being assembled
*/
x86.Assembler.prototype.toString = function (printEnc)
{
    var str = '';

    var codeBlock = new CodeBlock(256);

    for (var instr = this.firstInstr; instr !== null; instr = instr.next)
    {
        if (str != '')
            str += '\n';

        var line = instr.toString();

        if (printEnc)
        {
            line = rightPadStr(line, ' ', 30);            

            codeBlock.clear();
            instr.encode(codeBlock, this.x86_64);

            line += codeBlock.toString();
        }

        str += line;
    }

    return str;
}

/**
Add an instruction at the end of the block
*/
x86.Assembler.prototype.addInstr = function (instr)
{
    assert (
        instr instanceof x86.Instruction,
        ''
    );

    if (this.lastInstr === null)
    {
        this.firstInstr = instr;
        this.lastInstr = instr;

        instr.prev = null;
        instr.next = null;
    }
    else
    {
        this.lastInstr.next = instr;

        instr.prev = this.lastInstr;
        instr.next = null;

        this.lastInstr = instr;
    }
}

/**
Assemble a code block from the instruction list
@returns a code block object
*/
x86.Assembler.prototype.assemble = function ()
{
    // Total code length
    var codeLen = 0;

    // Compute the total encoding length
    for (var instr = this.firstInstr; instr !== null; instr = instr.next)
        codeLen += instr.getLength();

    // Allocate a new code block for the code
    var codeBlock = new CodeBlock(codeLen);

    // Encode the instructions into the code block
    for (var instr = this.firstInstr; instr !== null; instr = instr.next)
        instr.encode(codeBlock, this.x86_64);

    // Return the code block we assembled into
    return codeBlock;
};

/**
Anonymous function to initialize the assembler class
*/
(function ()
{
    // Create an assembler method for this instruction
    function makeInstrMethod(mnem)
    {
        x86.Assembler.prototype[mnem] = function ()
        {
            var opnds = [];

            for (var i = 0; i < arguments.length; ++i)
            {
                var opnd = arguments[i];

                if (!(opnd instanceof x86.Operand))
                {
                    if (opnd instanceof x86.Label)
                        opnd = new x86.LabelRef(opnd);
                    else
                        opnd = new x86.Immediate(opnd);
                }

                assert (
                    opnd instanceof x86.Operand,
                    'invalid operand argument: ' + opnd
                );

                opnds.push(opnd);
            }

            var instr = new x86.instrs[mnem](opnds, this.x86_64);

            this.addInstr(instr);
        };
    }

    // Create an assembler method for each instruction
    for (var instr in x86.instrs)
        makeInstrMethod(instr);

    // Create method to create labels on the assembler
    x86.Assembler.prototype.label = function (name)
    {
        var label = new x86.Label(name);

        this.addInstr(label);

        return label;
    }

    // Create an assembler field for each register
    for (var reg in x86.regs)
        x86.Assembler.prototype[reg] = x86.regs[reg];

    // Create a method to encode memory locations on the assembler
    x86.Assembler.prototype.mem = function (size, base, disp, index, scale)
    {
        return new x86.MemLoc(size, base, disp, index, scale);
    }

})();

