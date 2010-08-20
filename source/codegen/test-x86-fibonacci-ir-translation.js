var a = new x86.Assembler(x86.target.x86);
const reg = a.register;
const ESP = reg.esp;
const EAX = reg.eax;
const EBX = reg.ebx;
const $ = a.immediateValue;
const mem = a.memory;


const TRUE = $(1);
const FALSE = $(0);
const NULL = $(0);

const scratch = reg.edi;
const global = reg.esi;

const G_NEXT_OFFSET = 0;  // Offset for the cell containing 
                          // the next empty entry offset
const G_NEXT_OFFSET_WIDTH = 32;
const G_FIRST_OFFSET = 4; // Length value is 4 bytes
const G_KEY_OFFSET   = 0; // Key offset is 0 (we iterate over keys)
const G_KEY_WIDTH = 32;
const G_VALUE_OFFSET = 4; // Value offset is 4 (key length is 4 bytes)
const G_VALUE_WIDTH = 32;
const G_ENTRY_LENGTH = 8; // Key (4 bytes) Value (4 bytes)
var globalSize = 3;       // Maximum number of entries global 
                          // should be able to hold
const globalLabel = a.labelObj("GLOBAL_PRELUDE"); // Label for the position of global
const mainLabel = a.labelObj("MAIN");

// Assumptions:
// - dest is always a register

// Translation of IR instructions to x86 assembly
function processOpnds(opnds)
{
    // Replace strings, numbers, booleans and 
    // nulls by immediate integer values

    return;
};


a.ir_lt = function (opnds, dest)
{
    var cont = this.labelObj();

    if (opnds[0].type === x86.type.MEM &&
        opnds[1].type === x86.type.MEM)
    {
        this.
        mov(opnds[0], dest). 
        cmp(dest, opnds[1]); 
    } else
    {
        this.
        cmp(opnds[1], opnds[0]);
    }

    this.
    mov(TRUE, dest).
    jl(cont).
    mov(FALSE, dest).
    label(cont);

    return this;
};

a["ir_if"] = function (opnds, trueLabel, falseLabel)
{
    this.
    cmp(opnds[0], TRUE).
    je(trueLabel).
    jmp(falseLabel);

    return this;
};

a.ir_sub = function (opnds, dest)
{
    this.
    mov(opnds[0], dest).
    sub(opnds[1], dest);

    return this;
};

a.ir_add = function (opnds, dest)
{
    this.
    mov(opnds[0], dest).
    add(opnds[1], dest);

    return this;
};

a.ir_get_prop_val = function (opnds, dest)
{
    const obj = opnds[0];

    var cont = this.labelObj();

    assert(obj === global);

    this.
    /*
    mov($(0), dest).
    label(loop).
    mov(obj, scratch).
    add($(G_FIRST_OFFSET), scratch). // Retrieve address of first element
    add(dest, scratch).                // Add the current index 
    cmp(key, mem(G_KEY_OFFSET, scratch), G_KEY_WIDTH). // global[index] === key ?
    jne(notFound).
    mov(mem(G_VALUE_OFFSET, scratch), dest).  // return the current value
    jmp(cont).
    label(notFound).
    add($(G_ENTRY_LENGTH), dest).      // move to next value
    cmp(mem(G_NEXT_OFFSET, obj), dest).            // while there is values left
    jl(loop).
    mov(NULL, dest).        // no value found
    label(cont);
    */

    ir_get_prop_addr(opnds, scratch).
    cmp(NULL, scratch).
    je(cont).
    mov(mem(G_VALUE_OFFSET, scratch), scratch).

    label(cont).
    mov(scratch, dest);

    return this;

};

a.ir_get_prop_addr = function (opnds, dest)
{
    const obj = opnds[0];
    const key = opnds[1];

    // TODO: Ensure key is not a memory location

    var loop = this.labelObj();
    var end = this.labelObj();
    var notFound = this.labelObj();
    var cont = this.labelObj();

    assert(obj === global);

    this.
    mov(obj, scratch).
    add($(G_FIRST_OFFSET), scratch). // Retrieve address of first element
    add(mem(G_NEXT_OFFSET, obj), scratch). // Retrieve beginning of next
    sub($(G_ENTRY_LENGTH), scratch).       // Move to last element

    label(loop).                        // Loop from end to beginning
    sub($(G_FIRST_OFFSET), scratch).
    cmp(obj, scratch).           
    jl(end).

    add($(G_FIRST_OFFSET), scratch).       // Address of current item
    cmp(key, mem(G_KEY_OFFSET, scratch), G_KEY_WIDTH).   // global[index] === key ?
    je(cont).                         // Item found on equal!

    sub($(G_ENTRY_LENGTH), scratch).      // move to next value
    jmp(loop).

    label(end).
    mov(NULL, scratch).        // no value found

    label(cont).
    mov(scratch, dest);

    return this;
};

a.ir_put_prop_val = function (opnds, dest)
{
    const obj = opnds[0];
    const key = opnds[1];
    const value = opnds[2];

    // TODO: Ensure key and value is are not memory locations

    var loop = this.labelObj();
    var found = this.labelObj();

    assert(obj === global);

    this.
    ir_get_prop_addr(opnds, scratch).
    cmp(NULL, scratch).
    jne(found).
    mov(obj, scratch).
    add($(G_FIRST_OFFSET), scratch).          // Retrieve address of first element
    add(mem(G_NEXT_OFFSET, obj), scratch). // Retrieve address of next element 
    // Inc entry nb
    add($(G_ENTRY_LENGTH), mem(G_NEXT_OFFSET, global), G_NEXT_OFFSET_WIDTH). 
    mov(key, mem(G_KEY_OFFSET, scratch), G_KEY_WIDTH).     // Add entry key
    label(found).                          
    mov(value, mem(G_VALUE_OFFSET, scratch), G_VALUE_WIDTH); // Add/Update the entry value

    return this;
};

a.ir_dump_global_object = function ()
{
    const SELF = this.labelObj();
    this.
    label(globalLabel).
    ir_call_self().
    gen32(0); // Length
    
    for (var i=0; i < globalSize; ++i)
    {
        this.
        gen32(0). // Reserved space for key
        gen32(0); // Reserved space for value
    }
    this.genListing("GLOBAL_OBJECT");
    
    return this;
};

a.ir_call_self = function ()
{
    const SELF = this.labelObj();

    this.
    call(SELF).
    label(SELF).
    pop(EAX).
    add($(5),EAX).
    ret();

    return this;
};

a.ir_arg = function (opnds, dest, argIndex)
{
    if (dest === null)
    {
        return this;
    }

    

    return this;
};

a.ir_ret = function (opnds, dest)
{
    return this;
};

a.ir_call = function (opnds, dest, continue_label)
{
    return this;
};

a.ir_make_clos = function (opnds, dest, fctLabel)
{
    this.
    mov(EAX, scratch).
    call(fctLabel).
    mov(EAX, dest).
    mov(scratch, EAX);

    return this;
};

a.ir_get_global = function (opnds, dest)
{
    this.
    mov(global, dest);

    return this;
};

a.ir_init = function ()
{

    this.
    genListing("INIT").
    // Move global object address in global register 
    call(globalLabel).
    mov(EAX, global).

    // Jump to the main section
    jmp(mainLabel). 

    // Add the global object dump at the end of the init section
    ir_dump_global_object();

    return this;
};


a.ir_func_prelude = function (prelude_label, body_label)
{

    return this;
};



var FIB       = a.labelObj("FIB");
var BASE_CASE = a.labelObj("BASE_CASE");
var RECURSION = a.labelObj("RECURSION");

var found = a.labelObj("FOUND");

a.codeBlock.bigEndian = false;

a.
ir_init().
label(mainLabel).
ir_put_prop_val([global, $(1), $(23)]).
ir_put_prop_val([global, $(2), $(42)]).
ir_put_prop_val([global, $(1), $(66)]).
ir_get_prop_val([global, $(1)], EAX).
//ir_get_prop_addr([global, $(1)], EAX).
//mov(mem(G_NEXT_OFFSET,global), EAX).

ret();


a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
print(execMachineCodeBlock(block)); // execute the code generated
freeMachineCodeBlock(block);

