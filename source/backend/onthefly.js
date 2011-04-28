/**
@fileOverview
Implementation of an on-the-fly register allocator

@author
Erick Lavoie   

@copyright
Copyright (c) 2010-2011 Erick Lavoie, All Rights Reserved
*/

const onthefly = {};

/**
@class Register mapping
*/
onthefly.regMapping = function (args)
{
    var that = Object.create(onthefly.regMapping.prototype);

    /**
    Functions to access and set value memory slot
    @field
    */
    that.getSlot = args.getSlot;

    /**
    Function to compute value hash code
    @field
    */
    that.hash = args.hash;

    /**
    Insert function 
    @field
    */
    that.insertFunc = args.insertFunc;

    /**
    Compilation parameters
    @field
    */
    that.params = args.params;

    /**
    Mapping from physical registers to values
    @field
    */
    that.physReg = args.params.target.backendCfg.physReg.map(function (reg)
    {
        return null;
    });

    /**
    Mapping from values to physical registers
    @field
    */
    that.actives = {};

    return that;
};
onthefly.regMapping.prototype = {};

/**
Value of register
*/
onthefly.regMapping.prototype.val = function (reg)
{
    return this.physReg[reg];
};

/**
Values of registers
*/
onthefly.regMapping.prototype.vals = function (regs)
{
    const that = this;
    var vals = [];

    regs.forEach(function (reg)
    {
        vals.push(that.val(reg));
    });
    return vals;
};

/**
Register for value
*/
onthefly.regMapping.prototype.reg = function (val)
{
    if (val === null)
    {
        return null;
    }

    const reg = this.actives[this.hash(val)];
    if (reg === undefined)
    {
        return null;
    } else 
    {
        return reg;
    }
};

/**
Registers for values
*/
onthefly.regMapping.prototype.regs = function (vals)
{
    const that = this;
    var regs = [];

    vals.forEach(function (val)
    {
        regs.push(that.reg(val));
    });

    return regs;
};

/**
Physical register value
*/
onthefly.regMapping.prototype.regOpnd = function (reg)
{
    return this.params.target.backendCfg.physReg[reg];
};


/**
Free a register
*/
onthefly.regMapping.prototype.free = function (reg)
{
    if (this.val(reg) !== null)
    {
        delete this.actives[this.hash(this.val(reg))];
        this.physReg[reg] = null;
    }
};

/**
Set a register to a value
*/
onthefly.regMapping.prototype.set = function (val, reg)
{
    this.free(reg);
    this.physReg[reg] = val;     
    this.actives[this.hash(val)] = reg;
};

/**
Store a register value on stack
*/
onthefly.regMapping.prototype.store = function (reg)
{
    assert(this.val(reg) !== null, "Invalid value in register");

    var slot = this.getSlot(this.val(reg));

    this.insertFunc(new MoveInstr(this.regOpnd(reg), slot));
};

/**
Load a value in a register, either from memory or another
register
*/
onthefly.regMapping.prototype.load = function (val, reg)
{
    const src = this.reg(val);
    if (src === null)
    {
        var slot = this.getSlot(val);
        this.set(val, reg);
        this.insertFunc(new MoveInstr(slot, this.regOpnd(reg)));
    } else if (src !== reg)
    {
        this.move(src, reg);
    }
};

/**
Move a value from the first register to the second 
*/
onthefly.regMapping.prototype.move = function (reg1, reg2)
{
    const val = this.val(reg1);
    assert(val !== null, "No value in first register");

    if (reg1 !== reg2)
    {
        this.insertFunc(new MoveInstr(this.regOpnd(reg1), this.regOpnd(reg2)));
        this.set(val, reg2);
    }
};


/**
List of unused registers
*/
onthefly.regMapping.prototype.unused = function ()
{
    var unused = [];

    this.physReg.forEach(function (val, index)
    {
        if (val === null)
        {
            unused.push(index);
        }
    });

    return unused;
};

/**
List of used registers
*/
onthefly.regMapping.prototype.used = function ()
{
    var used = [];

    this.physReg.forEach(function (val, reg)
    {
        if (val !== null)
        {
            used.push(reg);
        }
    });

    return used;
};

/**
List of differences between used registers of this mapping and rmap
*/
onthefly.regMapping.prototype.diff = function (rmap)
{
    const that = this;
    var diff = [];

    this.used().forEach(function (reg)
    {
        const val = rmap.val(reg);

        if (val !== that.val(reg))
        {
            diff.push(reg);
        }
    });

    return diff;
};

/**
Whether two register mappings are equal
*/
onthefly.regMapping.prototype.equal = function (rmap)
{
    return this.diff(rmap).length === 0;
};

/**
Return the true source location for value
*/
onthefly.regMapping.prototype.getSrc = function (val)
{
    if (!(val instanceof IRInstr))
    {
        return val;
    }

    const reg = this.reg(val);

    if (reg === null)
    {
        return this.getSlot(val); 
    } else
    {
        return this.params.target.backendCfg.physReg[reg];
    }
};

/**
Return the source for values
*/
onthefly.regMapping.prototype.getSrcs = function (vals)
{
    return vals.map(this.getSrc, this);
};


/**
Make a copy of the mapping
*/
onthefly.regMapping.prototype.copy = function ()
{
    var that = Object.create(this);

    that.physReg = this.physReg.slice(0);

    that.actives = {};

    for (var p in this.actives)
    {
        that.actives[p] = this.actives[p];
    }

    return that;
};

/**
Return a string representation of the mapping
*/
onthefly.regMapping.prototype.toString = function ()
{
    const that = this;
    var s = "rmap:\n";
    this.physReg.forEach(function (val, reg)
    {
        s += reg + ": " + val + " -- reg: " + that.reg(val) + " -- val: " + 
             that.val(reg) + " -- src: " + that.getSrc(val) + "\n";
    });

    s += "actives:\n";
    for (var p in this.actives)
    {
        s += p + " reg: " + this.actives[p] + "\n"; 
    }

    return s;
};

/**
Register allocation object for blocks
*/
onthefly.block = function (block)
{
    if (block.regAlloc === undefined)
    {
        block.regAlloc = Object.create(onthefly.block.prototype);
    } else
    {
        block.regAlloc = Object.create(block.regAlloc);
    }
};

onthefly.block.prototype = regAlloc.block();

/**
Register allocation initialization for instructions
*/
onthefly.instr = function (instr)
{
    instr.regAlloc = Object.create(instr.regAlloc);

    /**
    Stack slot for instruction when spilled
    */
    instr.regAlloc.slot = null;

    /**
    Operands after allocation
    */
    instr.regAlloc.slot = null;

    /**
    Destination after allocation
    */
    instr.regAlloc.dest = null;
};
onthefly.instr.prototype = regAlloc.instr();

/**
@class On the fly allocator
*/
onthefly.allocator = function (params)
{
    var that = Object.create(onthefly.allocator.prototype);

    /**
    Compilation parameters
    @field
    */
    that.params = params;

    /**
    Block insertion offset
    @field
    */
    that.insertPos = 0;

    /**
    Block insertion block
    @field
    */
    that.insertBlock = null;

    /**
    Insertion function 
    @field
    */
    that.insertFunc = function (instr) 
    { 
        that.insertBlock.addInstr(instr,"",that.insertPos++);
    };

    /**
    getSlot function 
    @field
    */
    that.getSlot = null;

    /**
    CFG
    */
    that.cfg = null;

    return that;
};

onthefly.allocator.prototype = {};

/**
Allocate register on the given cfg
*/
onthefly.allocator.prototype.allocCfg = function (cfg, spiller)
{
    this.getSlot = function (instr)
    {
        var slot = instr.regAlloc.slot;

        if (slot === null)
        {
            slot = spiller.newSlot();
            instr.regAlloc.slot = slot;
        }

        return slot;
    };
    this.cfg = cfg;

    const that = this;
    const visited = [];
    const stack = [{ pred:null, 
                    block:cfg.getEntryBlock(),
                    rmap:onthefly.regMapping({
                         getSlot:this.getSlot, 
                         hash:function (instr) {return instr.instrId;}, 
                         insertFunc:this.insertFunc,
                         params:this.params
                    })}];

    // DF walk through the CFG
    while (stack.length > 0)
    {
        var stackItem = stack.pop();
        var pred  = stackItem.pred;
        var block = stackItem.block;
        var rmap  = stackItem.rmap;

        if (this.params.printRegAlloc === true)
            print("Visiting: " + block.getBlockName());

        // Reinitialize insertion function parameters
        this.insertBlock = block;
        this.insertPos = 0;

        if (visited[block.blockId] === true)
        {
            // SSA Deconstruction
            if (block.preds.length > 1)
            {
                that.resolve(rmap, pred, block);
            }
            continue;
        }

        visited[block.blockId] = true;

        // Initialize regAlloc structure
        onthefly.block(block);
      
        // Handle merge blocks for the first time
        if (block.preds.length > 1)
        {
            var predRmap = rmap.copy();

            // Handle Phi instructions
            this.allocPhis(rmap, this.phis(block), pred);
            block.regAlloc.rmapIn = rmap.copy();
            that.resolve(predRmap, pred, block);
        }

       
        // Handle remaining instructions
        var instrs = block.instrs.slice(0);
        for (var i = 0; i < instrs.length; ++i)
        {
            if (!(instrs[i] instanceof PhiInstr))
            {
                that.allocInstr(rmap, instrs[i]);
            }
            this.insertPos++;

            // Preemptively store all instructions
            // results on stack to ensure result is
            // available
            if (rmap.reg(instrs[i]) !== null && i < instrs.length - 1)
            {
                rmap.store(rmap.reg(instrs[i]));
            }
        };


        // Handle successors
        block.succs.forEach(function (succ)
        {
            // Optimize away rmap copies when there is only a
            // successor
            if (block.succs.length > 1)
            {
                rmap = rmap.copy();
            }

            stack.push({
                pred:block, 
                block:succ, 
                rmap:rmap
            });
        });
    }

};

/**
Resolve differences in allocation for phi nodes
*/
onthefly.allocator.prototype.resolve = function (current, pred, block)
{
    if (this.params.printRegAlloc === true)
    {
        print("On-the-fly resolve between " + pred.getBlockName() + " and " + 
              block.getBlockName());
        print(pred);
    }

    const that = this;
    const target = block.regAlloc.rmapIn;
    const diffVal = target.diff(current).map(function (reg) 
    { 
        return target.val(reg); 
    });
    const phis = this.phis(block);
    const moves = allocator.mapping();

    //print("Current: " + current);
    //print("Target: " + target);


    function getInsertFct(block, pos) 
    { 
        var insertIndex = pos;
        return function (instr) { 
            block.addInstr(instr,"", insertIndex++); 
        };
    };


    // Add moves for phi instructions
    phis.forEach(function (instr)
    {
        var predVal = instr.getIncoming(pred);
        var srcSlot = current.getSrc(predVal); 

        // Assign opnd src
        instr.regAlloc.opnds = instr.regAlloc.opnds.map(function (use)
        {
            if (use === predVal)
            {
                return srcSlot;
            } else
            {
                return use;
            }
        });

        const tgtSlot = target.getSrc(instr);
       
        if (srcSlot !== tgtSlot)
        {
            // Add move from srcSlot to tgtSlot
            //print("add move " + srcSlot + " " + tgtSlot);
            moves.add(srcSlot, tgtSlot);
        }
    });

    arraySetRemAll(diffVal, phis);

    diffVal.forEach(function (val)
    {
        const srcSlot = current.getSrc(val);
        const tgtSlot = target.getSrc(val);

        // Add move from srcSlot to tgtSlot
        //print("add move " + srcSlot + " " + tgtSlot);
        moves.add(srcSlot, tgtSlot);
    });

    var insertBlock = pred;

    // Resolution needs to be done on its own block
    if (pred.succs.length > 1)
    {
        var newBlock = this.cfg.getNewBlock("ssa_dec");
        onthefly.block(newBlock);
        this.cfg.insertBetween(pred, block, newBlock);
        insertBlock = newBlock;
    }

    // Order and insert moves
    const params = this.params;
    const backendCfg = params.target.backendCfg;
    // TODO: Remove dependency to x86 assembly
    const mem = x86.Assembler.prototype.memory;

    var tempOffset = params.memLayouts.ctx.getFieldOffset(
        [backendCfg.tempName]
    );
    var temp = mem(tempOffset, backendCfg.context);

    //print(moves);
    moves.orderAndInsertMoves(
        getInsertFct(insertBlock, insertBlock.instrs.length - 1),
        temp
    );
};

/**
Allocate registers for regular instructions
*/
onthefly.allocator.prototype.allocInstr = function (rmap, instr)
{
    const that = this;

    if (this.params.printRegAlloc === true)
    {
        print("On-the-fly allocInstr for " + instr);
    }
    onthefly.instr(instr);

    //print("Before " + rmap);

    function getInstrs(uses)
    {
        var instrs = [];
        uses.forEach(function (use)
        {
            if (use instanceof IRInstr)
            {
                instrs.push(use);
            }
        });

        return instrs;
    }

    function getReg(reg)
    {
        return that.params.target.backendCfg.physReg[reg];
    }

    // Remember stored value to avoid storing them twice
    var stored = [];
    function spill(reg)
    {
        if (rmap.val(reg) === null)
        {
            //stored.push(reg);
            return;
        }

        if (!arraySetHas(stored, reg))
        {
            rmap.store(reg);
            stored.push(reg);
        }
        rmap.free(reg);
    }

    var used = rmap.used();
    var unused = rmap.unused();
    var opnds = rmap.getSrcs(instr.uses);
    
    //print("Uses: " + instr.uses.join(","));
    //print("Opnds: " + opnds.join(","));


    // Alloc operands
    if (instr.regAlloc.opndsRegRequired === true)
    {
        //print("opndsRegRequired");

        // Force operands to be in registers
        // unless they are immediate values

        // Used registers not occupied by operands
        var nonopnd = used.slice(0);
        // Remove all operands already in registers
        var regs = rmap.regs(getInstrs(instr.uses));
        arraySetRemAll(nonopnd, regs);

        opnds = opnds.map(function (opnd, index)
        {
            if (opnd.type === x86.type.MEM)
            {
                // Operand is located in memory,
                // move it in a register
                if (unused.length > 0)
                {
                    // Use an unused register
                    var reg = unused.pop();
                } else
                {
                    // Use a non-operand occupied register
                    // and spill it
                    var reg = nonopnd.pop(); 
                    spill(reg);
                }

                // Move operand in a register
                const val = instr.uses[index];
                rmap.load(val, reg);
                
                // Return the register
                return getReg(reg);
            } else
            {
                // Operand is either a register 
                // or an immediate value
                return opnd;
            }
        });

        //print(opnds);
    } 
    instr.regAlloc.opnds = opnds;

    // Alloc destination
    var dest = null;
    instr.regAlloc.dest = null;
    if (instr.hasDests())
    {
        
        // Retrieve the register hint
        dest = instr.regAlloc.outRegHint(instr, this.params);

        //print("outRegHint: " + dest);

        // Give priority to the register hint
        if (dest !== null)
        {
            spill(dest);
        } else
        {
            // Try picking a free register
            if (unused.length > 0)
            {
                dest = unused.pop();
            } else
            {
                assert(used.length > 0, "Unavailable registers");
               // Otherwise, pick the first used register
               // TODO: Use a better heuristic like LRU
               dest = used[0];
               spill(dest);
            }
        }
        instr.regAlloc.dest = getReg(dest);
    } 

    // Preserve values that will be overwritten
    var blocked = instr.regAlloc.usedRegisters(instr, this.params);

    // TODO: Refactor such that blocked === [] when none are used 
    if (blocked !== null)
    {
        //print("Blocked by " + instr.getValName());
        //print(rmap);

        blocked = arraySetIntr(blocked, used);
        blocked.forEach(function (reg)
        {
            if (rmap.val(reg) !== null)
            {
                spill(reg);
            }
        });
        //print("-----");

    }

    if (dest !== null)
    {
        rmap.set(instr, dest);
    }

    /*
    if (blocked !== null)
    {
        //print(rmap);
        //print("*****");
    }
    //print(rmap);
    */
};

/**
Allocate registers for phi instructions
*/
onthefly.allocator.prototype.allocPhis = function (rmap, phis, pred)
{
    const that = this;
    const unused = rmap.unused();
    const nonphi = rmap.used();

    phis.forEach(function (instr)
    {
        //print("On-the-fly allocPhi for " + instr + " with pred " + 
        //      pred.getBlockName());
        onthefly.instr(instr);

        // Source instruction for phi
        const val = instr.getIncoming(pred);

        /*
        if (val instanceof IRInstr)
        {
            // Current value location
            var src = rmap.getSrc(val);
            this.regAlloc.slot = src;
        } else
        {*/
            // Try picking a free register
            if (unused.length > 0)
            {
                var reg = unused.pop();
                rmap.set(instr, reg);
                var src = rmap.getSrc(instr);
            } else if (nonphi.length > 0)
            {
               // Otherwise, pick the next used register
               var reg = nonphi.pop();
               rmap.free(reg);
               rmap.set(instr, reg);
               var src = rmap.getSrc(instr);
            } else
            {
                // No other register available, use a memory location
               var src = rmap.getSrc(instr);
            }
        //}

        // Initialize operands 
        instr.regAlloc.opnds = instr.uses.map(function (use)
        {
            if (use === val)
            {
                return src;
            } else
            {
                return use;
            }
        });

        instr.regAlloc.dest = src;
    });
};

/**
List of phi instructions for a block
*/
onthefly.allocator.prototype.phis = function (block)
{
    var phis = [];
    for (var i = 0; i < block.instrs.length; ++i)
    {
        var instr = block.instrs[i];
        if (!(instr instanceof PhiInstr) && !(instr instanceof MoveInstr))
        {
            break;
        }

        if (instr instanceof PhiInstr)
        {
            phis.push(instr);
        }
    }


    return phis;
};


