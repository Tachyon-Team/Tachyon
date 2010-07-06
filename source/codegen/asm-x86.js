/**
@fileOverview
Assembler for x86 machine code. Translates assembly code written
in a AT&T inspired syntax to binary code ready for execution.

The following code was inspired by the Gambit code 
generator written in Scheme.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

// TODO: Could we put those constructors on a global object named x86?
function x86_Assembler (target)
{
    this.listing   = true;
    this.codeBlock = new asm_CodeBlock(true, 0, this.listing);
    if (target) { this.target = target; } 
};

x86_Assembler.target = {};
x86_Assembler.target.x86    = 0; 
x86_Assembler.target.x86_64 = 1;

x86_Assembler.prototype.target = x86_Assembler.target.x86;
x86_Assembler.prototype.is64bitMode = function () 
{
    return this.target === x86_Assembler.target.x86_64;
};
x86_Assembler.prototype.assert64bitMode = function ()
{
    if (!(this.is64bitMode())) { throw "instruction only valid for x86-64"; };
};

(function () { // local namespace

// Alias
const x86 = x86_Assembler.prototype;

// error reporting
function error (message)
{
    var err = message;
    for (var i=1; i<arguments.length; i++)
    {
        err += arguments[i];
    }
    throw err;
}

function assert (bool, message)
{
    if (!bool) { error(message, Array.prototype.slice.call(arguments, 2)); } 
}

// utility functions
function isSigned8 (num)
{
   return (num >= -128 && num <= 127); 
}

// utility functions
function isSigned32 (num)
{
   return (num >= -2147483648 && num <= 2147483647); 
}

x86.widthSuffix = function (width)
{
    if      (width === 64) { return "q"; }
    else if (width === 32) { return "l"; }
    else if (width === 16) { return "w"; }
    else if (width === 8 ) { return "b"; }
    else                   { return "";  }
}

x86.regWidthSuffix = function (reg) { return this.widthSuffix(reg.width()); }

x86.gen8  = function (n) { this.codeBlock.gen8(n);  return this;};
x86.gen16 = function (n) { this.codeBlock.gen16(n); return this;};
x86.gen32 = function (n) { this.codeBlock.gen32(n); return this;};
x86.gen64 = function (n) { this.codeBlock.gen64(n); return this;};

x86._genImmNum = function (k, width)
{
    // TODO: Find out what behavior should the signed-lo have
    function signedLo(n, k) 
    {
        return (k & (Math.pow(2,n) - 1));
    };

    if (width === 8) 
    {
        this.gen8(signedLo(8,k));   
    } else if (width === 16) 
    {
        this.gen16(signedLo(16,k));
    } else if (width === 32)
    {
        this.gen32(signedLo(32,k));
    }
    else 
    {
        this.gen64(signedLo(64,k));
    }   
    return this;
};

x86.genImmNum = function (k, width)
{
    this._genImmNum(k, Math.min(32, width));
};

// Types to allow testing for object type on x86 related
// objects
x86.type = {};
x86.type.IMM = 0;
x86.type.REG = 1;
x86.type.MEM = 2;
x86.type.GLO = 3;
x86.type.LBL = 4;

// TODO: Add toString method for every type

// Immediate object to represent immediate value
x86.immediate = function (value)
{
    // Enforce that modifications of the that object
    // won't screw up the prototype by creating a
    // new object for each instance whether they
    // have non-default properties or not
    var that = Object.create(x86.immediate.prototype);

    // Minimize memory usage by storing default values 
    // for properties only on the prototype and avoid
    // duplicating default values in each instance    
    if (value)   { that.value = value; }

    return that;
}
x86.immediate.prototype.type  = x86.type.IMM;
x86.immediate.prototype.value = 0;

// Memory object operand representing an access 
// to memory through a Displacement, Scale, Index and Base
x86.memory = function ( disp, base, index, scale )
{
    var that = Object.create(x86.memory.prototype);

    if (disp)   { that.disp   = disp; }
    if (base)   { that.base   = base;   }
    if (index)  { that.index  = index;  }
    if (scale)  { that.scale  = scale;  }
    
    return that;
}

x86.memory.prototype.type   = x86.type.MEM;
// Let's use explicit null values to enforce
// presence of the properties while 
// being able to detect that these are defaults
x86.memory.prototype.base   = null;
x86.memory.prototype.index  = null;
// Idem for 0
x86.memory.prototype.disp   = 0;
x86.memory.prototype.scale  = 0;

x86.global = function ( name, offset )
{
    var that = Object.create(x86.global.prototype);
    if (name)   { that.name   = name; };
    if (offset) { that.offset = offset; };
    return that;
}
x86.global.prototype.type   = x86.GLO;
x86.global.prototype.name   = null;
x86.global.prototype.offset = 0;

x86.register = function ( name, value )
{
    var that = Object.create(x86.register.prototype);
    if (!name)  { throw "register: name property not supplied" }    
    that.name  = name;
    if (typeof value !== "number" && 
        !value) { throw "register: value property not supplied" }
    that.value = value;

    x86.register.names[value] = that;
    return that;
}

x86.register.names = []
x86.register.r8    = function (n) { return this.names [ 80 + n ]; }
x86.register.r16   = function (n) { return this.names [ 32 + n ]; }
x86.register.r32   = function (n) { return this.names [ 16 + n ]; }
x86.register.r64   = function (n) { return this.names [ n ]; }
x86.register.fpu   = function (n) { return this.names [ 48 + n ]; }

const reg = x86.register.prototype;
reg.type = x86.REG;
reg.isr8  = function () { return this.value >= 80; }
reg.isr8h = function () { return this.value >= 96; }
reg.isxmm = function () { return this.value >= 64 && this.value < 80;}
reg.ismm  = function () { return this.value >= 56 && this.value < 64;}
reg.isfpu = function () { return this.value >= 48 && this.value < 56;}
reg.isr16 = function () { return this.value >= 32 && this.value < 48;}
reg.isr32 = function () { return this.value >= 16 && this.value < 32;}
reg.isr64 = function () { return this.value <  16;}
// TODO: When getter setter are supported, refactor field and width to 
// act as properties instead of a method. This will make it more
// similar to the way base, index and disp are accessed on memory object
reg.field = function () { return this.value & 0xF }
reg.width    = function () 
{
    if      (this.value < 16) { return 64;} 
    else if (this.value < 32) { return 32;}
    else if (this.value < 48) { return 16;}
    else if (this.value < 64) { return 80;}
    else if (this.value < 80) { return 128;}
    else                      { return 8;}
}
 
x86.register.al    = x86.register("al",    80); 
x86.register.cl    = x86.register("cl",    81); 
x86.register.dl    = x86.register("dl",    82); 
x86.register.bl    = x86.register("bl",    83); 
x86.register.ah    = x86.register("ah",   100); 
x86.register.ch    = x86.register("ch",   101); 
x86.register.dh    = x86.register("dh",   102); 
x86.register.bh    = x86.register("bh",   103); 
x86.register.spl   = x86.register("spl",   84); 
x86.register.bpl   = x86.register("bpl",   85); 
x86.register.sil   = x86.register("sil",   86); 
x86.register.dil   = x86.register("dil",   87); 
x86.register.r8b   = x86.register("r8b",   88); 
x86.register.r9b   = x86.register("r9b",   89); 
x86.register.r10b  = x86.register("r10b",  90); 
x86.register.r11b  = x86.register("r11b",  91); 
x86.register.r12b  = x86.register("r12b",  92); 
x86.register.r13b  = x86.register("r13b",  93); 
x86.register.r14b  = x86.register("r14b",  94); 
x86.register.r15b  = x86.register("r15b",  95); 
x86.register.ax    = x86.register("ax",    32); 
x86.register.cx    = x86.register("cx",    33); 
x86.register.dx    = x86.register("dx",    34); 
x86.register.bx    = x86.register("bx",    35); 
x86.register.sp    = x86.register("sp",    36); 
x86.register.bp    = x86.register("bp",    37); 
x86.register.si    = x86.register("si",    38); 
x86.register.di    = x86.register("di",    39); 
x86.register.r8w   = x86.register("r8w",   40); 
x86.register.r9w   = x86.register("r9w",   41); 
x86.register.r10w  = x86.register("r10w",  42); 
x86.register.r11w  = x86.register("r11w",  43); 
x86.register.r12w  = x86.register("r12w",  44); 
x86.register.r13w  = x86.register("r13w",  45); 
x86.register.r14w  = x86.register("r14w",  46); 
x86.register.r15w  = x86.register("r15w",  47); 
x86.register.eax   = x86.register("eax",   16); 
x86.register.ecx   = x86.register("ecx",   17); 
x86.register.edx   = x86.register("edx",   18); 
x86.register.ebx   = x86.register("ebx",   19); 
x86.register.esp   = x86.register("esp",   20); 
x86.register.ebp   = x86.register("ebp",   21); 
x86.register.esi   = x86.register("esi",   22); 
x86.register.edi   = x86.register("edi",   23); 
x86.register.r8d   = x86.register("r8d",   24); 
x86.register.r9d   = x86.register("r9d",   25); 
x86.register.r10d  = x86.register("r10d",  26); 
x86.register.r11d  = x86.register("r11d",  27); 
x86.register.r12d  = x86.register("r12d",  28); 
x86.register.r13d  = x86.register("r13d",  29); 
x86.register.r14d  = x86.register("r14d",  30); 
x86.register.r15d  = x86.register("r15d",  31); 
x86.register.rax   = x86.register("rax",    0); 
x86.register.rcx   = x86.register("rcx",    1); 
x86.register.rdx   = x86.register("rdx",    2); 
x86.register.rbx   = x86.register("rbx",    3); 
x86.register.rsp   = x86.register("rsp",    4); 
x86.register.rbp   = x86.register("rbp",    5); 
x86.register.rsi   = x86.register("rsi",    6); 
x86.register.rdi   = x86.register("rdi",    7); 
x86.register.r8    = x86.register("r8",     8); 
x86.register.r9    = x86.register("r9",     9); 
x86.register.r10   = x86.register("r10",   10); 
x86.register.r11   = x86.register("r11",   11); 
x86.register.r12   = x86.register("r12",   12); 
x86.register.r13   = x86.register("r13",   13); 
x86.register.r14   = x86.register("r14",   14); 
x86.register.r15   = x86.register("r15",   15); 
x86.register.st    = x86.register("st",    48); 
x86.register.st_1  = x86.register("st_1",  49); 
x86.register.st_2  = x86.register("st_1",  50); 
x86.register.st_3  = x86.register("st_1",  51); 
x86.register.st_4  = x86.register("st_1",  52); 
x86.register.st_5  = x86.register("st_1",  53); 
x86.register.st_6  = x86.register("st_1",  54); 
x86.register.st_7  = x86.register("st_1",  55); 
x86.register.mm0   = x86.register("mm0",   56); 
x86.register.mm1   = x86.register("mm1",   57); 
x86.register.mm2   = x86.register("mm2",   58); 
x86.register.mm3   = x86.register("mm3",   59); 
x86.register.mm4   = x86.register("mm4",   60); 
x86.register.mm5   = x86.register("mm5",   61); 
x86.register.mm6   = x86.register("mm6",   62); 
x86.register.mm7   = x86.register("mm7",   63); 
x86.register.xmm0  = x86.register("xmm0",  64); 
x86.register.xmm1  = x86.register("xmm1",  65); 
x86.register.xmm2  = x86.register("xmm2",  66); 
x86.register.xmm3  = x86.register("xmm3",  67); 
x86.register.xmm4  = x86.register("xmm4",  68); 
x86.register.xmm5  = x86.register("xmm5",  69); 
x86.register.xmm6  = x86.register("xmm6",  70); 
x86.register.xmm7  = x86.register("xmm7",  71); 
x86.register.xmm8  = x86.register("xmm8",  72); 
x86.register.xmm9  = x86.register("xmm9",  73); 
x86.register.xmm10 = x86.register("xmm10", 74); 
x86.register.xmm11 = x86.register("xmm11", 75); 
x86.register.xmm12 = x86.register("xmm12", 76); 
x86.register.xmm13 = x86.register("xmm13", 77); 
x86.register.xmm14 = x86.register("xmm14", 78); 
x86.register.xmm15 = x86.register("xmm15", 79); 




//-----------------------------------------------------------------------------

// x86 instruction encoding.
x86.ret   = function ()  { return this.noOpndInstr(0xc3, "ret");};
x86.cmc   = function ()  { return this.noOpndInstr(0xf5, "cmc");};
x86.clc   = function ()  { return this.noOpndInstr(0xf8, "clc");};
x86.stc   = function ()  { return this.noOpndInstr(0xf9, "stc");};
x86.cli   = function ()  { return this.noOpndInstr(0xfa, "cli");};
x86.sti   = function ()  { return this.noOpndInstr(0xfb, "sti");};
x86.cld   = function ()  { return this.noOpndInstr(0xfc, "cld");};
x86.std   = function ()  { return this.noOpndInstr(0xfd, "std");};


x86.noOpndInstr = function (opcode, mnemonic)
{
    this.gen8(opcode);
    if (this.listing)
        this.genListing(this.instrFormat(mnemonic, ""));
    return this;
};


x86.genListing = function(text) { this.codeBlock.genListing(text); };
x86.instrFormat = function(mnemonic, suffix) 
{ 
    return "    " + mnemonic + suffix; 
};

x86.opndSizeOverridePrefix = function (width) 
{
    if (width === 16) { this.gen8(0x66); }; return this;
}

x86.addrSizeOverridePrefix = function (opnd)
{
    if (opnd.type === x86.type.MEM &&
        opnd.base !== null         &&
        this.is64bitMode() !== opnd.base.isr64())
    {
        this.gen8(0x67);
    }
}

x86.opndPrefix = function (width, field, opnd, forceRex)
{
    // TODO: Check that the logic for producing the REX byte
    //       is correct
    var rex = // If needed emit REX.W (64 bit operand size)
              (( width === 64 || ( opnd.type === x86.type.REG && opnd.isr64())) 
               ? 8 : 0) +
              // If needed emit REX.R (Extension of the ModR/M reg field)
              (( field >> 3) << 2);

    // finish setting the rex value
    switch (opnd.type)
    {
        case x86.type.REG:
            // If needed emit REX.B (Extension of the ModR/M r/m field,
            // SIB base field, or Opcode reg field)
            rex += (opnd.field() >> 3);
            break;

        case x86.type.GLO:
            break;

        case x86.type.MEM:
            const base = opnd.base; 
            if(base) 
            {
                assert((base.isr32() || (base.isr64() && this.is64bitMode())),
                       "invalid width base register",
                       base); 
                // If needed emit REX.B (Extension of the ModR/M r/m field,
                // SIB base field, or Opcode reg field
                rex += (base.field() >> 3);
                
                const index = opnd.index;
                if(index) 
                {
                    assert((base.isr32() ? index.isr32() : index.isr64()),
                           "index register must have the same width as base",
                           reg2);        
                    rex += ((index.field() >> 3) << 1);
                }
            }
            break;

        default:
            error("unknown operand", opnd);
    }

    this.opndSizeOverridePrefix(width); 
    this.addrSizeOverridePrefix(opnd);

    if (forceRex || (!rex === 0))
    {
        this.assert64bitMode();
        this.gen8(0x40 + rex);
        return true;    
    }             
    return false;
}

x86.opndModRMSIB = function (field, opnd)
{
    // TODO: Double check the logic
    const modrm_rf = (7 & field) << 3;
    const that = this;
    
    function absAddr ()
    {
        if (that.is64bitMode())
        {
            that.gen8(modrm_rf + 4); // ModR/M
            that.gen8(0x25);         // SIB
        } else
        {
            that.gen8(modrm_rf + 5); // ModR/M
        }
    }

    var modrm;
    switch (opnd.type)
    {
        case x86.type.REG:
            modrm = modrm_rf + (7 & opnd.field());
            this.gen8(0xc0 + modrm); // ModR/M
            break;

        case x86.type.GLO:
            // TODO: Implement when labels are supported
            error("opndModRMSIB: unimplemented for opnd of type global");
            break;

        case x86.type.MEM:
            const base  = opnd.base;
            const baseFieldLo = 7 & base.field();
            const index = opnd.index;
            const disp  = opnd.disp;
            const scale = opnd.scale;
            
            if (base) 
            {
                
                // index: Need a SIB when using an index
                // baseFieldLo: register or base = RSP/R12 
                if (index || (baseFieldLo === 4))
                {
                    // SIB Needed
                    modrm = modrm_rf + 4;
                    var sib   = baseFieldLo;
                    
                    if (index)
                    {
                        assert(!(index.field() === 4),
                               "SP not allowed as index", index);
                        sib += ((7 & index.field()) << 3) +
                               (scale << 6);
                    } else // !index
                    {
                        sib += 0x20;
                    }
                   
                    if (isSigned8(disp))
                    {
                        // use 8 bit displacement
                        if (!(disp === 0) ||        // non-null displacement
                              (base.field() === 5)) // or RBP    
                        {
                            this.gen8(0x40 + modrm);// ModR/M
                            this.gen8(sib);         // SIB
                            this.gen8(disp);
                        } else
                        {
                            this.gen8(modrm);      // ModR/M
                            this.gen8(sib);        // SIB
                        }
                    } else // !isSigned8(disp)
                    {
                        // use 32 bit displacement
                        this.gen8(0x80 + modrm);   // ModR/M
                        this.gen8(sib);            // SIB
                        this.gen32(disp);          
                    }
                } else // !index && !baseFieldLo === 4 
                {
                    // SIB Not Needed
                    modrm = modrm_rf + baseFieldLo;
                    if (isSigned8(disp))
                    {
                        if (!(disp === 0) ||       // non-null displacement
                            (baseFieldLo === 5))   // or RBP/R13
                        {
                            // use 8 bit displacement 
                            this.gen8(0x40 + modrm);// ModR/M
                            this.gen8(disp);
                        } else
                        {
                            this.gen8(modrm);       // ModR/M
                        }
                    } else // !isSigned8(disp)
                    {
                        // use 32 bit displacement
                        this.gen8(0x80 + modrm);
                        this.gen32(disp);
                    }
                } 
            } else // (!base)
            {
                // Absolute address, use disp32 ModR/M 
                absAddr();
                this.gen32(disp);
            }
            break;

        default:
            error("unkown operand", opnd);
    }
    return this;
}

x86.opndPrefixRegOpnd = function (reg, opnd)
{
    const width  = reg.width();
    const field  = reg.field();
    const isExtLo8 = ((width === 8) && (field >= 4) && (!reg.isr8h()));

    if (opnd.type === x86.type.REG)
    {
        const isExtLo8Reg2 = ((width === 8) && 
                              (opnd.field() >= 4) && 
                              (!opnd.isr8h()));
        var isRex;
        assert(((width === opnd.width) || 
                reg.isxmm()),             // for cvtsi2ss/cvtsi2sd instructions
               "registers are not of the same width",reg,opnd);

        isRex = this.opndPrefix(width, field, opnd, (isExtLo8 || isExtLo8Reg2));

        assert(!(isRex && (reg.isr8h() || opnd.isr8h())),
               "cannot use high 8 bit register here", reg, opnd);
        return isRex;
    } else  // opnd.type !== x86.type.REG
    {
        return this.opndPrefix(width, field, opnd, isExtLo8);
    }
}

x86.opndPrefixOpnd = function (width, opnd)
{
    if (opnd.type === x86.type.REG)
    {
        const field = opnd.field();
        const isExtLo8 = ((width === 8) && (field >= 4) && (!reg.isr8h()));
        return this.opndPrefix(width, 0, opnd, isExtLo8);
    } else // opnd.type !== x86.type.REG
    {
        return this.opndPrefix(width, 0, opnd, false);
    }
}

x86.opndModRMSIBRegOpnd = function (reg, opnd)
{
    return this.opndModRMSIB(reg.field(), opnd);
}

x86.opImm = function (op, mnemonic, k, dest, width) 
{
    const that = this;

    function listing (width,n)
    {
        //TODO
    }

    function accumulator (width)
    {
        that.
        opndSizeOverridePrefix(width).
        // opcode = #x04, #x0c, #x14, ..., #x3c (for AL)
        //       or #x05, #x0d, #x15, ..., #x3d (for AX/EAX/RAX)
        gen8(((width === 8) ? 0x04 : 0x05) + (op << 3));
        listing(width, that.genImmNum(k,width));
    }

    function general (width)
    {
        that.opndPrefixOpnd(width, dest);

        if (width === 8) 
        {
            that.
            gen8(0x80).            // opcode = 8 bit operation
            opndModRMSIB(op,dest); // ModR/M
            listing(width, that.genImmNum(k,8));
        } else if (isSigned8(k))
        {
            that.
            gen8(0x83).            // opcode = sign extended 8 bit imm
            opndModRMSIB(op,dest); // ModR/M
            listing(width, that.genImmNum(k,8));
        } else 
        {
            that.
            gen8(0x81).            // opcode = sign extended 16/32 bit imm
            opndModRMSIB(op,dest); // ModR/M
            listing(width, that.genImmNum(k,width));
        }
    }

    assert((dest.type === x86.type.REG) ? 
            (!width || (dest.width() === width)) : width,
            "missing or inconsistent operand width", width);

    if (dest.type === x86.type.REG)
    {
        if ((dest.field() === 0) && 
            (dest.width() === 8 || !isSigned8(k)))
        {
            accumulator(width);
        } else 
        {
            general(width);
        }
    } else // dest.type !== x86.type.REG
    {
        general(width);
    }
    return that;
};

x86.movImm = function (dest, k, width)
{

    const that = this;

    function listing (width,n)
    {
        //TODO
    }

    function register (width)
    {
        that.
        opndPrefixOpnd(width, dest). // prefix
        // opcode = #xb0-#xb7 (for 8 bit registers)
        //      or #xb8-#xbf (for 16/32/64 bit registers)
        gen8( ((width === 8) ? 0xb0 : 0xb8) + (7 & dest.field()) );
        listing(width, that.genImmNum(k, width));
    }

    function general (width)
    {
        that.opndPrefixOpnd(width, dest);
        that.
        gen8((width === 8) ? 0xc6 : 0xc7).  // opcode
        opndModRMSIB(0,dest); // ModR/M
        listing(width, that.genImmNum(k,width));
    }

    assert((dest.type === x86.type.REG) ? 
            (!width || (dest.width() === width)) : width,
            "missing or inconsistent operand width", width);

    if (dest.type === x86.type.REG)
    {
        if (dest.width() === 64 &&
            isSigned32(k))
        {
            general(width);
        } else 
        {
            register(width);
        }
    } else // dest.type !== x86.type.REG
    {
        general(width);
    }
    return this;
}

x86.op    = function (op, mnemonic, dest, src, width) 
{
    const that = this;

    function genOp (reg, opnd, isSwapped)
    {
        assert(!width || (reg.width() === width),
               "inconsistent operand width",width);
        that.opndPrefixRegOpnd(reg, opnd);
        that.
        gen8((op << 3) +
             (isSwapped ? 0 : 2) +
             (reg.isr8() ? 0 : 1)).
        opndModRMSIBRegOpnd(reg.opnd);

        if (this.listing)
        {
            // TODO
        }

    }

    if (src.type === x86.type.IMM)
    {
        if (op === 17)
        {
            this.movImm(dest, src.value, width);
        } else
        {
            this.opImm(op, mnemonic, dest, src.value, width);
        }
    } else if (src.type === x86.type.REG)
    {
        genOp(src, dest, true);
    } else if (dest.type === x86.type.REG)
    {
        genOp(dest, src, false);
    } else 
    {
        error("invalid operand combination", dest, src);
    }
   return this;
}

x86.add = function ( dest, src, width )
{
    return this.op(0,"add",dest,src,width);
}

})(); // end of local namespace

