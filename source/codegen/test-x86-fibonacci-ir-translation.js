var a = new x86.Assembler(x86.target.x86);
const reg = a.register;
const ESP = reg.esp;
const EBP = reg.ebp;
const EAX = reg.eax;
const EBX = reg.ebx;
const ECX = reg.ecx;
const EDX = reg.edx;
const $ = a.immediateValue;
const mem = a.memory;


const TRUE = $(1);
const FALSE = $(0);
const NULL = $(0);
const UNDEFINED = $(0);

const STRINGS = {"f":$(0)};

const scratch = reg.edi;
const stack = reg.esp;

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


const physRegs = [EAX, EBX, ECX, EDX]; 

var FIB       = a.labelObj("FIB");
var BASE_CASE = a.labelObj("BASE_CASE");
var RECURSION = a.labelObj("RECURSION");
var MAIN      = a.labelObj("MAIN");

var found = a.labelObj("FOUND");
var ret = a.labelObj("RETURN");
var call_cont = a.labelObj("call_cont");
var call_cont1 = a.labelObj("call_cont1");
var call_cont2 = a.labelObj("call_cont2");
var call_cont3 = a.labelObj("call_cont3");
var if_false = a.labelObj("if_false");
var if_true = a.labelObj("if_true");

a.codeBlock.bigEndian = false;

// Assumptions:
// - dest is always a register

// Translation of IR instructions to x86 assembly
function processOpnds(opnds)
{
    // Replace strings, numbers, booleans and 
    // nulls by immediate integer values

    return;
};

function neg(opnd)
{
    assert(opnd.type === x86.type.IMM_VAL);
    return $(-opnd.value);
};

a.xchg = function (opnd1, opnd2)
{
    assert(!(opnd1.type === x86.type.MEM &&
             opnd2.type === x86.type.MEM));

    assert(!opnd1.type === x86.type.IMM_VAL);
    assert(!opnd2.type === x86.type.IMM_VAL);
    
    this.
    xor(opnd1, opnd2).
    xor(opnd2, opnd1).
    xor(opnd1, opnd2);
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

a.ir_if = function (opnds, trueLabel, falseLabel)
{
    this.
    cmp(TRUE, opnds[0]).
    je(trueLabel).
    jmp(falseLabel);

    return this;
};

a.ir_sub = function (opnds, dest)
{
    if (opnds[1] === dest && opnds[0].type !== x86.type.IMM_VAL)
    {
        this.
        xchg(opnds[1], opnds[0]).
        sub(opnds[1], dest).
        xchg(opnds[1], opnds[0]);
    } else if (opnds[1] === dest && opnds[0].type === x86.type.IMM_VAL)
    {
        this.
        mov(opnds[0], scratch).
        sub(opnds[1], scratch).
        mov(scratch, opnds[1]);
    } else if (opnds[0] === dest)
    {
        this.
        sub(opnds[1], dest);
    } else
    {
        this.
        mov(opnds[0], dest).
        sub(opnds[1], dest);
    }
    return this;
};

a.ir_add = function (opnds, dest)
{
    if (opnds[1] === dest)
    {
        this.
        add(opnds[0], dest);
    } else if (opnds[0] === dest)
    {
        this.
        add(opnds[1], dest);
    } else
    {
        this.
        mov(opnds[0], dest).
        add(opnds[1], dest);
    }

    return this;
};

a.ir_get_prop_val = function (opnds, dest)
{
    const obj = opnds[0];

    var cont = this.labelObj();

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

    this.
    mov(obj, scratch).
    add($(G_FIRST_OFFSET), scratch). // Retrieve address of first element
    add(mem(G_NEXT_OFFSET - G_FIRST_OFFSET, scratch), 
        scratch). // Retrieve beginning of next
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

    this.
    ir_get_prop_addr(opnds, scratch).
    cmp(NULL, scratch).
    jne(found).
    mov(obj, scratch).
    add($(G_FIRST_OFFSET), scratch).          // Retrieve address of first element
    add(mem(G_NEXT_OFFSET, obj), scratch). // Retrieve address of next element 
    // Inc entry nb
    add($(G_ENTRY_LENGTH), mem(G_NEXT_OFFSET, obj), G_NEXT_OFFSET_WIDTH). 
    mov(key, mem(G_KEY_OFFSET, scratch), G_KEY_WIDTH).     // Add entry key
    label(found).                          
    mov(value, mem(G_VALUE_OFFSET, scratch), G_VALUE_WIDTH); // Add/Update the entry value

    return this;
};

a.ir_dump_global_object = function ()
{
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

a.ir_call_self = function (offset)
{
    if (offset === undefined)
    {
        offset = 5;
    }
    const SELF = this.labelObj();

    this.
    call(SELF).
    label(SELF).
    pop(EAX).
    add($(offset),EAX).
    ret().
    genListing("ADDR RETRIEVAL");

    return this;
};

a.ir_arg = function (opnds, dest, argIndex)
{
    if (dest === null)
    {
        return this;
    }

    if (dest !== physRegs[argIndex])
    {
        error("ir_arg: dest register '" + dest + 
              "' unexpected for argument index '" + argIndex + "'");
    }
    

    return this;
};

a.ir_ret = function (opnds, dest, spillNb)
{
    if (spillNb === undefined)
    {
       spillNb = 0; 
    }

    this.add($(spillNb*4), stack);

    if (opnds[0] !== EAX)
    {
        this.mov(opnds[0], EAX);
    }
   
    //this.mov(mem(0,stack), EBX);
    //this.jmp(EBX);
    this.ret();
    return this;
};

a.ir_call = function (opnds, dest, continue_label)
{
    var spillNb = opnds.length - 4;
    var offset = 1;
    var i;
    const that = this;

    if (spillNb < 0)
    {
        spillNb = 0;
    }

    offset = (spillNb) * 4;

    // Move arguments in the right registers
    var map = allocator.mapping();

    for (i=0; i < 4 && i < opnds.length; ++i)
    {

        if (opnds[i] !== physRegs[i])
        {
            map.add(opnds[i], physRegs[i]);
        }
    }

    map.orderAndInsertMoves( function (move)
                             {
                                that.mov(move.uses[0], move.uses[1]);
                             }, scratch);

    // Add extra arguments on the stack
    if (spillNb > 0)
    {
        // TODO
    }

    this.
    // Add stack frame descriptor space
    // TODO

    // Add return address
    /*
    mov(EAX, scratch).
    ir_call_self(15).
    mov(EAX, mem(-(offset), stack)).
    mov(scratch, EAX).
    */

    // Decrement stack pointer
    sub($(offset), stack).

    // Call function address
    call(EAX).

    // Remove return address and extra args
    add($(offset), stack).

    // Jump to continue_label
    jmp(continue_label);

    return this;
};

a.ir_func_prelude = function (prelude_label)
{
    // Add the call self instructions to retrieve
    // the address of the function
    this.
    label(prelude_label).
    ir_call_self(9).
    
    // Reserve space for the global object associated
    // with this function
    gen32(0).
    genListing("FUNC GLOBAL OBJ");


    return this;
};

a.ir_func_init = function (spillNb)
{
    if (spillNb === undefined)
    {
        spillNb = 0;
    }

    this.sub($(spillNb*4), stack);
    return this;
};

a.ir_make_arg_obj = function (opnds, dest)
{
    assert(dest === null);
    // For now, let's ignore the argument object

    return this;
};

a.ir_make_clos = function (opnds, dest)
{
    assert(opnds[0].type === asm.type.LBL);

    if (dest === EAX)
    {
        this.
        call(opnds[0]).
        mov(opnds[1], mem(-4, EAX));
    } else 
    {
        this.
        mov(EAX, scratch).
        call(opnds[0]).
        mov(EAX, dest).
        mov(scratch, EAX).
        mov(dest, scratch).

        // Store the global object in the function prelude
        mov(opnds[1], mem(-4, scratch));
    }

    return this;
};

a.ir_get_global = function (opnds, dest)
{

    if (opnds[0].type === x86.type.REG)
    {
        this.
        mov(mem(-4, opnds[0]), dest);
    } else if (opnds[0].type === x86.type.MEM)
    {
        this.
        mov(opnds[0], scratch).
        mov(mem(-4, scratch), dest);
    }

    return this;
};

a.ir_init = function ()
{
    var ret = this.labelObj("MAIN RET");

    this.
    genListing("INIT").
    // Move global object address in global register 
    call(globalLabel).
    mov(EAX, EBX).

    // Setup the main function
    ir_make_clos([MAIN, EBX], EAX).

    // Call the main function
    ir_call([EAX, EBX], EAX, ret).

    // Return from the main function
    label(ret).
    ret().

    // Add the global object dump at the end of the init section
    ir_dump_global_object();

    return this;
};



a.
ir_init().

// Fibonacci function 
ir_func_prelude(FIB).
ir_func_init(3).
    // Body
    ir_arg([], EAX, 0).
    ir_arg([], ECX, 2).
    ir_make_arg_obj([EAX], null).
    ir_get_global([EAX], EBX).
    ir_lt([ECX, $(2)], EAX).
    ir_if([EAX], if_true, if_false).

label(if_false).
    ir_sub([ECX, $(1)], EAX).
    ir_get_prop_val([EBX, STRINGS["f"]], EDX).
    mov(EBX, mem(4, stack)).
    mov(ECX, mem(0, stack)).
    ir_call([EDX, EBX, EAX], EAX, call_cont2).

label(call_cont2).
    ir_sub([mem(0, stack), $(2)], ECX).
    ir_get_prop_val([mem(4, stack), STRINGS["f"]], EBX).
    mov(EAX, mem(8, stack)).
    ir_call([EBX, mem(4,stack), ECX], EAX, call_cont3).

label(call_cont3).
    ir_add([mem(8,stack), EAX], EAX).
    ir_ret([EAX], null, 3).

label(if_true).
    mov(ECX, mem(0,stack)).
    mov(mem(0,stack), EAX).
    ir_ret([EAX], null, 3).

// Main function
ir_func_prelude(MAIN).
ir_func_init(1).
    // Body
    ir_arg([], EAX, 0).
    ir_make_arg_obj([EAX], null).
    ir_get_global([EAX], EBX).
    ir_make_clos([FIB, EBX], EAX).
    ir_put_prop_val([EBX, STRINGS["f"], EAX]).
    ir_get_prop_val([EBX, STRINGS["f"]], EAX).
    mov(EBX, mem(0, stack)).
    ir_call([EAX, EBX, $(40)], EAX, call_cont).
label(call_cont).
    // TODO: Add print
    ir_ret([EAX], null, 1).

//ir_get_global([EAX], EAX).
//mov(global, EAX).
//ir_put_prop_val([global, $(1), $(23)]).
//ir_put_prop_val([global, $(2), $(42)]).
//ir_put_prop_val([global, $(1), $(66)]).
//ir_get_prop_val([global, $(1)], EAX).
//mov($(2), EAX).
//mov($(5), EBX).
//ir_sub([EAX, EBX], EAX).

ret();


a.codeBlock.assemble();

print(a.codeBlock.listingString());

var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
print(execMachineCodeBlock(block)); // execute the code generated
freeMachineCodeBlock(block);

