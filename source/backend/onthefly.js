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

    // TODO: Change hash for ID
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
    Mapping from values hash to registers
    @field
    */
    that.actives = {};

    /**
    Mapping from values hash to memory slot 
    @field
    */
    that.stored = {};

    /**
    Mapping from values hash to values 
    @field
    */
    that.storedVal = {};


    return that;
};
onthefly.regMapping.prototype = {};

/**
Value of register
*/
onthefly.regMapping.prototype.val = function (reg)
{
    if (reg === null)
    {
        return null;
    }

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
Slot for value
*/
onthefly.regMapping.prototype.slot = function (val)
{
    if (val === null)
    {
        return null;
    }

    var slot = this.actives[this.hash(val)];
    if (slot !== undefined)
    {
        return slot;
    } else 
    {
        slot = this.stored[this.hash(val)];

        if (slot !== undefined)
            return slot;
        else
            return null;
    }
};

/**
Slots for values
*/
onthefly.regMapping.prototype.slots = function (vals)
{
    const that = this;
    var slots = [];

    vals.forEach(function (val)
    {
        slots.push(that.slot(val));
    });

    return slots;
};

/**
Test if a given slot is a register
*/
onthefly.regMapping.prototype.isReg = function (slot)
{
    return (typeof slot) === "number";    
};

/**
Register for value
*/
onthefly.regMapping.prototype.reg = function (val)
{
    var slot = this.slot(val);

    if (this.isReg(slot))
    {
        return slot;
    } else
    {
        return null;
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
Clear value from mapping, removing it from registers
and known memory locations
*/
onthefly.regMapping.prototype.clear = function (val)
{
    var reg = this.reg(val);    
    this.free(reg);
    
    var slot = this.slot(val);
    if (slot !== null)
    {
        delete this.stored[this.hash(val)];
        delete this.storedVal[this.hash(val)];
    }
};

/**
Set a slot to a value
*/
onthefly.regMapping.prototype.set = function (val, slot)
{
    if (this.isReg(slot))
    {
        this.free(slot);
        this.physReg[slot] = val;     
        this.actives[this.hash(val)] = slot;
    } else
    {
        // TODO: Check that a memory slot is not used by
        //       more than two temps at the same time 
        this.stored[this.hash(val)] = slot; 
        this.storedVal[this.hash(val)] = val;
    }
};

/**
Store a register value on stack
*/
onthefly.regMapping.prototype.store = function (reg)
{
    assert(this.val(reg) !== null, "Invalid value in register");
    
    var val = this.val(reg);
    var stored = this.stored[this.hash(val)];

    if (stored === undefined)
    {
        var slot = this.getSlot(this.val(reg));
        this.insertFunc(new MoveInstr(this.regOpnd(reg), slot));
        this.stored[this.hash(val)] = slot; 
        this.storedVal[this.hash(val)] = val;
    }
};

/**
Load a value in a register, either from memory or another
register
*/
onthefly.regMapping.prototype.load = function (val, reg)
{
    const slot = this.slot(val);

    assert(slot !== null, "Invalid slot for value " + val);

    if (!this.isReg(slot))
    {
        this.set(val, reg);
        this.insertFunc(new MoveInstr(slot, this.regOpnd(reg)));
    } else if (slot !== reg)
    {
        this.set(val, reg);
        this.insertFunc(new MoveInstr(this.regOpnd(slot), this.regOpnd(reg)));
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
List of live values
*/
onthefly.regMapping.prototype.live = function ()
{
    var live = [];

    for (var p in this.actives)
    {
        live.push(this.val(this.actives[p]));
    }

    for (var p in this.stored)
    {
        if (this.actives[p] === undefined)
        {
            live.push(this.storedVal[p]);
        }
    }

    return live;
}

/**
List of differences between memory and register slots for this mapping and rmap
*/
onthefly.regMapping.prototype.diff = function (rmap)
{
    const that = this;
    var diff = [];

    this.live().forEach(function (val)
    {
        const slot = rmap.slot(val);

        if (slot !== that.slot(val))
        {
            diff.push(val);
        }
    });

    return diff;
};

/**
Keep only values in live, clearing everything else
*/
onthefly.regMapping.prototype.keep = function (live)
{
    const that = this;
    var dead = this.live();
    arraySetRemAll(dead, live);

    dead.forEach(function(d)
    {
        that.clear(d);
    });
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

    const slot = this.slot(val);

    if (this.isReg(slot))
    {
        return this.regOpnd(slot);
    } else
    {
        return slot;
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

    that.stored = {};
    for (var p in this.stored)
    {
        that.stored[p] = this.stored[p];
    }

    that.storedVal = {};
    for (var p in this.storedVal)
    {
        that.storedVal[p] = this.storedVal[p];
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

    s += "stored:\n";
    for (var p in this.stored)
    {
        s += this.storedVal[p].getValName() + " slot: " + this.stored[p] + "\n"; 
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

    /**
    Block order computed while visiting
    */
    that.order = [];

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
        {
            //print("Visiting: " + block.getBlockName());
        }

        // Reinitialize insertion function parameters
        this.insertBlock = block;
        this.insertPos = 0;

        if (visited[block.blockId] === true)
        {
            // SSA Deconstruction and resolution of 
            // register allocation differences
            if (block.preds.length > 1)
            {
                that.resolve(rmap, pred, block);
            }
            continue;
        }

        visited[block.blockId] = true;

        // Initialize regAlloc structure
        onthefly.block(block);
      
        // Handle instructions
        that.allocInstrs(rmap, block, pred);


        // Handle successors

        // Optimization to favor the true branch
        // of conditional tests
        if (block.succs.length > 1)
        {
            // For branching instructions,
            // assume the true branch is usually the most frequent
            var last = block.getLastInstr();
            if (last.isBranch())
            {
                var succs = last.targets.slice(0).reverse();    
            } else
            {
                var succs = last.targets;
            }
        } else
        {
            var succs = block.succs;
        }

        succs.forEach(function (succ)
        {
            // Optimize away rmap copies when there is only a
            // successor
            if (succs.length > 1)
            {
                rmap = rmap.copy();
            }

            stack.push({
                pred:block, 
                block:succ, 
                rmap:rmap
            });
        });

        // Add to block order
        this.order.push(block);
    }

};

/**
Resolve differences in allocation for phi nodes
*/
onthefly.allocator.prototype.resolve = function (current, pred, block)
{
    if (this.params.printRegAlloc === true)
    {
        //print("On-the-fly resolve between " + pred.getBlockName() + " and " + 
        //      block.getBlockName());
    }

    const that = this;
    const target = block.regAlloc.rmapIn;
    const diff = target.diff(current);
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


    // Add moves for phi instructions and remove 
    // predecessors from mapping
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
            //print("phis add move " + srcSlot + " " + tgtSlot);
            moves.add(srcSlot, tgtSlot);
        }
    });

    arraySetRemAll(diff, phis);

    diff.forEach(function (val)
    {
        const srcSlot = current.getSrc(val);
        const tgtSlot = target.getSrc(val);

        // Add move from srcSlot to tgtSlot
        //print("diff add move " + srcSlot + " " + tgtSlot);
        moves.add(srcSlot, tgtSlot);
    });

    if (moves.length === 0)
    {
        return;
    }

    var insertBlock = pred;

    // Resolution needs to be done on its own block
    if (pred.succs.length > 1)
    {
        var newBlock = this.cfg.getNewBlock("ssa_dec");
        onthefly.block(newBlock);
        this.cfg.insertBetween(pred, block, newBlock);
        insertBlock = newBlock;
        this.order.push(newBlock);
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

    moves.orderAndInsertMoves(
        getInsertFct(insertBlock, insertBlock.instrs.length - 1),
        temp
    );
};

/**
Allocate registers for all regular instructions in the block
*/
onthefly.allocator.prototype.allocInstrs = function (rmap, block, pred)
{
    const that    = this;
    const instrs  = block.instrs;
    const lastIndex = instrs.length - 1;

    // This encoding method for distances avoids updating the usedist data structure
    // for each temporary value at each instruction.
    var usedist = {
        i:instrs.length,
        instrNb:instrs.length,
        usepos:new HashMap(),
        usedist:block.analysis.usedist,
        toRemove:[],
        // Returns the furthest used temporary from the given list
        furthestUsed: function (temps)
        {
            function argmax(arr)
            {
                if (arr.length === 0)
                {
                    return -1;
                }

                var max_i = 0;
                var max_v = arr[i];

                arr.forEach(function (v, i)
                {
                    if (v > max_v)
                    {
                        max_v = v;
                        max_i = i;
                    }
                });

                return max_i;
            }

            return temps[argmax(temps.map(this.dist, this))];
        },

        dist:function (temp)
        {
            if (this.usepos.hasItem(temp))
            {
                return this.usepos.getItem(temp) - this.i;
            } else if (this.usedist.hasItem(temp))
            {
                return this.usedist.getItem(temp) + this.instrNb - this.i;
            } else
            {
                return -1;
            }
        },
        
        // Returns the usedist for the previous instruction
        // given the current usedist
        previous:function (instr, i)
        {
            var that = Object.create(this);
            that.i = i;
            that.usepos = this.usepos.copy();
            that.usedist = this.usedist;
            that.toRemove = [];

            instr.uses.forEach(function (use)
            {
                if (use instanceof IRInstr)
                {
                    if (that.dist(use) < 0)
                    {
                        that.toRemove.push(use);
                    }

                    that.usepos.setItem(use, i);
                }
            });
        
            if (instr.hasDests())
            {
                // A negative number is used as a flag to indicate
                // that the temp is not live
                that.usepos.setItem(instr, -1);
            }
            
            return that;
        },

        live:function ()
        {
            const that = this;
            var keys =  arraySetUnion(this.usedist.getKeys(), this.usepos.getKeys());
            var live = [];

            keys.forEach(function (key)
            {
                if (that.dist(key) >= 0)
                {
                    live.push(key);
                }
            });

            return live;
        },

        toString:function ()
        {
            const that = this;
            var keys = arraySetUnion(this.usedist.getKeys(), this.usepos.getKeys());

            var s = keys.map(function(key)
            {
                return key.getValName() + ": " + that.dist(key);
            });

            return "usedist <" + s.join(", ") + "> toRemove: [" + 
                   this.toRemove.map(function (x) { 
                       return x.getValName(); 
                   }).join(",") + "]";

        }
    }
   
    // Iterate through all instructions of the block,
    // first backward to create the usedist structures
    // then forward to allocate registers
    function iter(instrs, i, usedist)
    {
        // Base case: Handle phi instructions,
        // update the rmap and resolve differences 
        // between the preceding block and the current one
        if (i < 0 || instrs[i] instanceof PhiInstr)
        {
            /*
            print("Block " + block.getBlockName() + " in [" + 
                  usedist.live().map(function (use) { return use.getValName(); }) 
                  + "]");
            print("out [" + block.analysis.usedist.getKeys()
                  .map(function (x) { return x.getValName(); }) + "]");
            print("pred rmap:");
            print(rmap);
            */

            // Handle merge blocks for the first time
            if (block.preds.length > 1)
            {
                var predRmap = rmap.copy();

                // Handle Phi instructions
                that.allocPhis(rmap, that.phis(block), pred, usedist);

                // Keep only temps live in this block
                rmap.keep(usedist.live());
                block.regAlloc.rmapIn = rmap.copy();

                // Resolution is needed to introduce moves for 
                // constant opnds of phi instructions
                that.resolve(predRmap, pred, block);

                //print("rmap:");
                //print(rmap);
            }

       
            that.insertPos = i+1;
            return;
        }

        var instr = instrs[i];
        var usedist = usedist.previous(instr, i);
        iter(instrs, i-1, usedist); 

        that.allocInstr(rmap, instr, usedist);     
        that.insertPos++;
    }

    //print("Block: " + block.getBlockName());
    //print(usedist.toString());

    // Initialize the backward iteration with the usedist from the 
    // end of the block
    iter(instrs, lastIndex, usedist);

    //print("Rmap at block end: " + rmap.toString());
}

/**
Allocate registers for regular instructions
*/
onthefly.allocator.prototype.allocInstr = function (rmap, instr, usedist)
{
    const that = this;

    if (this.params.printRegAlloc === true)
    {
        //print("On-the-fly allocInstr for " + instr + " " + usedist.toString());
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
            return;
        }

        if (!arraySetHas(stored, reg))
        {
            rmap.store(reg);
            stored.push(reg);
        }
        rmap.free(reg);
    }

    function condSpill(reg)
    {
        if (!arraySetHas(dead, reg))
        {
            spill(reg);
        } 
    }
    
    var dead = []; 
    rmap.regs(usedist.toRemove).forEach(function (r)
    {
        if (r !== null)
        {
            dead.push(r);
        }
    });
    var used = rmap.used();
    var unused = rmap.unused();
    var opnds = rmap.getSrcs(instr.uses);
    
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
                    var reg = usedist.furthestUsed(nonopnd);
                    arraySetRem(nonopnd, reg);
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

    unused = arraySetUnion(unused, dead);
    if (instr.hasDests())
    {
        
        // Retrieve the register hint
        dest = instr.regAlloc.outRegHint(instr, this.params);

        //print("outRegHint: " + dest);

        // Give priority to the register hint
        if (dest !== null)
        {
                 
            condSpill(dest);
        } else
        {
            // Try picking a free register
            if (unused.length > 0)
            {
                dest = unused.pop();
            } else
            {
                assert(used.length > 0, "Unavailable registers");
               var dest = usedist.furthestUsed(used);
               spill(dest);
            }
        }
        instr.regAlloc.dest = getReg(dest);
    } 

    // Preserve values that will be overwritten
    var blocked = instr.regAlloc.usedRegisters(instr, this.params);

    if (blocked !== null)
    {
        //print("Blocked by " + instr.getValName());
        //print(rmap);

        blocked = arraySetIntr(blocked, used);
        blocked.forEach(function (reg)
        {
            if (rmap.val(reg) !== null)
            {
                condSpill(reg);
            }
        });
        //print("-----");

    }

    if (dest !== null)
    {
        rmap.set(instr, dest);
    }

    // Clear all dead temps from registers
    usedist.toRemove.forEach(function (use)
    {
        //print("Clearing unused temp " + use.getValName());
        rmap.clear(use);
    });

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
onthefly.allocator.prototype.allocPhis = function (rmap, phis, pred, usedist)
{
    const that = this;
    const unused = rmap.unused();

    phis.forEach(function (instr)
    {
        //print("On-the-fly allocPhi for " + instr + " with pred " + 
        //      pred.getBlockName());
        onthefly.instr(instr);

        // Source instruction for phi
        const val = instr.getIncoming(pred);

        // Use the same slot as predecessor if it is 
        // an instruction and it is not used anymore
        if (val instanceof IRInstr && usedist.dist(val) < 0)
        {

            // Use the incoming register or memory location
            // as the destination for the phi instruction
            var slot = rmap.slot(val);
            rmap.clear(val);
            rmap.set(instr, slot);
            var src = rmap.getSrc(instr);
        } else
        {
            // Otherwise, use a new slot

            // Try picking a free register
            if (unused.length > 0)
            {
                var reg = unused.pop();
                rmap.set(instr, reg);
                var src = rmap.getSrc(instr);
            } else
            {
                // No other register available, use a memory location
               var src = rmap.getSlot(instr);
               // print("No other register available for " + instr.getValName() +
               //       " using " + src);
               rmap.set(instr, src);
            }
        }

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


