/**
@fileOverview

Register allocation using a linear scan algorithm from "Optimized interval 
splitting in a linear scan register allocator" by C Wimmer and H Mossenbock.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

/** @namespace */
var allocator = allocator || {};


//-----------------------------------------------------------------------------

// Generic Algorithms and Data Structures

// TODO: Move generic algorithms and data structures into their utility file

/** Returns the max index and the max value of an array */
allocator.max = function (a, accessFct)
{
    accessFct = accessFct || function (a) { return a; };

    var maxIndex = 0;
    var maxValue = 0;
    var i;

    for (i=0; i < a.length; ++i)
    {
        if (accessFct(a[i]) > maxValue)
        {
            maxIndex = i;
            maxValue = accessFct(a[i]); 
        }
    }
    
    return { index:maxIndex, value:maxValue }; 
};

/** Returns the min index and the min value of an array */
allocator.min = function (a, accessFct)
{
    accessFct = accessFct || function (a) { return a; };

    var minIndex = 0;
    var minValue = 0;
    var i;

    for (i=0; i < a.length; ++i)
    {
        if (accessFct(a[i]) > minValue)
        {
            minIndex = i;
            minValue = a[i]; 
        }
    }
    
    return { index:minIndex, value:minValue }; 
};

/** 
    Returns the index of the first item equals to value
    in the sorted array. 
    @param {Array} a sorted array
    @param value
    @param {Function} cmpFct comparison function used
*/
allocator.binSearch = function (a, value, cmpFct)
{
    var maxIndex = a.length;
    var minIndex = 0;
    var current  = (maxIndex + minIndex) >> 1;
    var cmp = 0;
    var i;

    while (maxIndex !== minIndex)
    {
        cmp = cmpFct(value, a[current]);
        if (cmp < 0)
        {
            maxIndex = current;
        } else if (cmp === 0)
        {
            break;
        } else
        {
            minIndex = current + 1;
        }
        current  = (maxIndex + minIndex) >> 1;
    }


    if (cmpFct(value, a[current]) !== 0)
    {
        return null;
    } else
    {
        // Check if a previous value would also be equal
        for (i=current; i >= 0; --i)
        {
            if (cmpFct(a[i], a[current]) === 0)
            {
                current = i;
            } 
        }

        return current;
    }
};

/** 
    Finds the first object in a given array whose access function returns
    a value with the identity equality comparison returning true. Returns
    null if no value can be found.
 
    @param {Array} a
    @param {Object} value
    @param {Function} accessFct optional, defaults to array object at index
*/
allocator.find = function (a, value, accessFct)
{
    accessFct = accessFct || function (x) { return x; };

    var i;
    for(i=0; i < a.length; ++i)
    {
        if (accessFct(a[i]) === value)
        {
            return a[i];
        }
    }
    return null;
};

/** 
    Finds the index of the first object in a given array whose access 
    function returns a value with the identity equality comparison returning 
    true. Returns null if no value can be found.
 
    @param {Array} a
    @param {Object} value
    @param {Function} accessFct optional, defaults to array object at index
*/
allocator.findPos = function (a, value, accessFct)
{
    accessFct = accessFct || function (x) { return x; };

    var i;
    for(i=0; i < a.length; ++i)
    {
        if (accessFct(a[i]) === value)
        {
            return i;
        }
    }
    return null;
};

/** 
    @class Set Object
    @param {Array} a optional, initial content of set must not contain
                   duplicate entries
*/
allocator.set = function (a)
{
    var that = Object.create(allocator.set.prototype);
    that.values = a;
    return that;
};
/** @private values for the set object */
allocator.set.prototype.values = [];
/** test if a given value is contained in set using the identity equality */
allocator.set.prototype.contains = function (value)  
{
    var i;
    for (i=0; i < this.values.length; i++)
    {
        if (this.values[i] === value)
        {
            return true;
        }
    }
    return false;
};
/** add a value to the set */
allocator.set.prototype.add = function (value)
{
    if (!this.contains(value))
    {
        this.values.push(value);
    }
};
/** remove a value from the set */
allocator.set.prototype.remove = function (value)
{
    var i;
    for (i=0; i < this.values.length; i++)
    {
        if (this.values[i] === value)
        {
            this.values.splice(i, 1);
        }
    }
    return value;
};
/** returns number of item in set */
allocator.set.prototype.length = function ()
{
    return this.values.length;
};
/** print a set */
allocator.set.prototype.toString = function ()
{
    
    var i;
    var s = [];
    for (i=0; i < this.values.length; i++)
    {
        s.push(String(this.values[i]));
    }
    return "Set{\n\t" + s.join(",\n\t") + "}";
};

/** 
    @class Priority queue that doesn't allow priorities of element 
           already inserted to be changed 
    @param {Array} a initial set of values in priority queue
    @param {Function} cmpFunction compare function for sorting
*/  
allocator.priorityQueue = function (a, cmpFunction)
{
    var that = Object.create(allocator.priorityQueue.prototype);
    that.innerArray = a || that.innerArray;
    that.cmpFunction = cmpFunction || that.cmpFunction;

    // Ensure sorted
    that.innerArray.sort(cmpFunction);

    return that;
};
/** @private inner array maintained in a sorted state */  
allocator.priorityQueue.prototype.innerArray = [];
/** @private compare function used to maintain the sorted property */   
allocator.priorityQueue.prototype.cmpFunction = 
    function (a,b) 
    { 
        return (a-b);
    }; 

/** Add a value to the priority queue. Two identical values
    will be retrieved in FIFO order.
  */ 
allocator.priorityQueue.prototype.enqueue = function (value)
{
    var a = this.innerArray;
    var maxIndex = this.innerArray.length;
    var minIndex = 0;
    var current  = (maxIndex + minIndex) >> 1;
    var cmp = 0;
    var i;

    while (maxIndex !== minIndex)
    {
        cmp = this.cmpFunction(value, a[current]);
        if (cmp < 0)
        {
            maxIndex = current;
        } else if (cmp === 0)
        {
            break;
        } else
        {
            minIndex = current + 1;
        }
        current  = (maxIndex + minIndex) >> 1;
    }

    // Maintain a FIFO order for equal values 
    for (i=current; i < maxIndex; i++)
    {
        if (this.cmpFunction(a[i], a[current]) !== 0)
        {
            break;
        }
    }
    current = i;

    // Add the element at the current position
    this.innerArray.splice(current, 0, value);
};

/** Remove the value with the highest priority from the priority queue   */ 
allocator.priorityQueue.prototype.dequeue = function ()
{
    return this.innerArray.shift();
};

/** Return the value with the highest priority from the priority queue,
    without removing it  */ 
allocator.priorityQueue.prototype.peek = function ()
{
    return this.innerArray[0];
};

/** Return the number of elements in the queue  */ 
// TODO: Refactor to getter when getter/setter are supported
allocator.priorityQueue.prototype.length = function ()
{
    return this.innerArray.length;
};

/** Print the priority queue */
allocator.priorityQueue.prototype.toString = function ()
{
    
    var i;
    var s = [];
    for (i=0; i < this.innerArray.length; i++)
    {
        s.push(String(this.innerArray[i]));
    }
    return "PriorityQueue [" + s.join(",\n") + "]";
};


//-----------------------------------------------------------------------------

// Allocator Data Structures


/** 
    @class Range for intervals 
    @param {Number} startPos optional, defaults to 0
    @param {Number} endPos optional, default to Infinity 
*/
allocator.range = function (startPos, endPos) 
{
    var that = Object.create(allocator.range.prototype);
    that.startPos = startPos || that.startPos;
    that.endPos   = endPos   || that.endPos;
    return that;
};
/** Start position of the range, inclusive */
allocator.range.prototype.startPos = 0;
/** End position of the range, inclusive */
allocator.range.prototype.endPos   = Infinity;
/** Returns a string representation of a range */
allocator.range.prototype.toString = function () 
{
    return "[" + this.startPos + "," + this.endPos + "[";
};
/** 
    Tells if a range covers a given position 
    @param {Number} pos position number
    @param {Boolean} isInclusive true if the start position 
                     boundary of a range should be included.
*/
allocator.range.prototype.covers = function (pos, isInclusive)
{
    if (isInclusive === undefined)
    {
        isInclusive = true;
    }
    if (isInclusive)
    {
        return pos >= this.startPos && pos < this.endPos;
    } else
    {
        return pos > this.startPos && pos < this.endPos;
    }
};
/** Compare a range with a position,
    returns -1 if position happens before range
             0 if position is covered by range
             1 if position happens after range

    Note that the end position is always excluded from the range,
    so that a position happening right at the end position
    is considered to happen after the range.

    @param {Number} pos position number
    @param {Boolean} isInclusive true if the start position 
                     boundary of a range should be included.
 */
allocator.range.prototype.cmp = function (pos, isInclusive)
{
    if (isInclusive === undefined)
    {
        isInclusive = true;
    }

    if (pos < this.startPos)
    {
        return -1;
    } else if (pos === this.startPos)
    {
        return isInclusive ? 0 : -1;
    } else if (this.covers(pos))
    {
        return 0;
    } else
    {
        return 1;
    }
};

/** 
    @class Usage position for intervals 
    @param {Number} pos
    @param {allocator.usePos.registerFlag} registerFlag, optional 
*/
allocator.usePos = function (pos, registerFlag)
{
    var that = Object.create(allocator.usePos.prototype);
    that.pos = pos || that.pos; 
    that.registerFlag = registerFlag || that.registerFlag;
    return that;
};
/** @namespace */
allocator.usePos.registerFlag = {};
/** A register would be preferred but is not required */
allocator.usePos.registerFlag.NONE      = "NONE";
/** Register is mandatory for position */
allocator.usePos.registerFlag.REQUIRED  = "REQUIRED";

/** Tells whether a register is mandatory, preferred or not required at position */
allocator.usePos.prototype.registerFlag = allocator.usePos.registerFlag.NONE;
/** Position being used */
allocator.usePos.prototype.pos = 0;
/** Returns a string representation of a use position */
allocator.usePos.prototype.toString = function ()
{
    return "(" + this.pos + "," + this.registerFlag + ")";
};

// TODO: modify implementation to use a LinkedList for ranges and iterators
// TODO: change startPos and endPos to 'from' and 'to'
/** 
    @class Interval object for register allocation 
    @param {Array} ranges
    @param {Array} usePositions optional, defaults to a WRITE for each startPos
                                and READ for each endPos of ranges. registerFlag
                                is set to NONE.
*/
allocator.interval = function (ranges, usePositions)
{
    var i, range;
    const NONE = allocator.usePos.registerFlag.NONE;

    var that = Object.create(allocator.interval.prototype);
    that.ranges = ranges || [];
    that.ranges.sort(function (r1,r2) { return r1.startPos - r2.startPos; });

    if (usePositions)
    {
        that.usePositions = usePositions;
    } else
    {   
        that.usePositions = [];
        

        for (i=0; i < that.ranges.length; ++i)
        {
            range = that.ranges[i];
            that.usePositions.push(allocator.usePos(range.startPos, NONE));
            that.usePositions.push(allocator.usePos(range.endPos, NONE));
        } 
    }
    
    that.id = allocator.interval.id++;
    return that;
};
/** @private unique name generator */
allocator.interval.id = 0;
/** @private Positions where the virtual register is being read or written */
allocator.interval.prototype.usePositions = [];
/** @private Disjoint set of all ranges representing the interval usage */ 
allocator.interval.prototype.ranges = [];
/** Allocated physical register or memory location */ 
allocator.interval.prototype.reg    = null;
/** Register hint for allocation */
allocator.interval.prototype.regHint = null;
/** Linked interval, for intervals that have been split */
allocator.interval.prototype.next   = null;
/** Linked interval, for intervals that have been split */
allocator.interval.prototype.previous   = null;
/** Unique identifier for this interval */
allocator.interval.prototype.id     = 0;
/** Virtual register */
allocator.interval.prototype.vreg   = null;
/** Instruction defining the interval */
allocator.interval.prototype.instr  = null;

/** Returns the first starting position of all ranges */ 
allocator.interval.prototype.startPos = function ()
{
    if (this.ranges.length > 0)
    {
        return this.ranges[0].startPos;
    } else
    {
        return null;
    }
};

/** Returns the last ending position of all ranges */
allocator.interval.prototype.endPos = function ()
{
    if (this.ranges.length > 0)
    {
        return this.ranges[this.ranges.length - 1].endPos;
    } else
    {
        return null;
    }
};

/** Returns the number of ranges */
allocator.interval.prototype.rangeNb = function ()
{
    return this.ranges.length;
};

/** Returns a string representation of an interval */
allocator.interval.prototype.toString = function ()
{
    var r = [];
    var u = [];
    var i;
    
    // Print all the ranges
    for (i=0; i < this.ranges.length; ++i)
    {
        r.push(String(this.ranges[i]));
    }

    // Print all the use positions
    for (i=0; i < this.usePositions.length; ++i)
    {
        u.push(String(this.usePositions[i]));
    }

    return "Interval { vreg:'" + this.vreg + "' ranges:( " + 
           r.join(",") + " ) usePositions:( " + u.join(",") + " ) reg:'" + 
           this.reg + "' }";
};

/** Tells if a given position is contained within one of the ranges
    of this interval or one of the child interval.
    @param {Number} pos position number
    @param {Boolean} isInclusive true if the start position 
                     boundary of a range should be included.
*/ 
allocator.interval.prototype.covers = function (pos, isInclusive)
{
    var interval = this;

    if (isInclusive === undefined)
    {
        isInclusive = true;
    }

    while (interval !== null)
    {
        var i;
        for (i=0; i < interval.ranges.length; ++i)
        {
            if (interval.ranges[i].covers(pos, isInclusive))
            {
                return true;
            }
        }
        interval = interval.next;
    }
    return false;
};

/** Give the position of the next intersection with interval,
    Infinity if no intersection occurs */  
allocator.interval.prototype.nextIntersection = function (interval)
{
    var i1 = 0;
    var i2 = 0;

    var rs1 = this.ranges;
    var rs2 = interval.ranges;

    var pos, range, cmp;
    
    /** @ignore */
    function swap ()
    {
        var tempr = i1;
        var temprs = rs1;

        i1 = i2;
        i2 = tempr; 

        rs1 = rs2;
        rs2 = temprs;
    }

    // Test for empty intervals, because if either one of the interval
    // is empty they cannot intersect
    if (rs1.length === 0 || 
        rs2.length === 0)
    {
        return Infinity;
    }

    // Start from further startPosition,
    // since the other cannot intersect
    // before the start of an interval
    if (this.startPos() < interval.startPos())
    {
        swap();
    }

    // While there is ranges to test against
    while (i2 < rs2.length)
    {
        pos = rs1[i1].startPos;
        range = rs2[i2];

        cmp = range.cmp(pos);

        if (cmp < 0)
        {
            // position happens before range,
            // swap, to use the other interval
            // range start position
            swap();
        } else if (cmp === 0)
        {
            // The start position intersect the other
            // range, return pos
            return pos;
        } else
        {
            // position happens after range,
            // try next range
            i2++;
        }
    }
     
    return Infinity;
};

/** Split before position.

    <p>
    In case the position happens right at the starting position
    of a range, splitting occurs just before that range.
    </p>

    <p>
    In case the position happens rigth at the ending position of
    a range, splitting occurs just after that range.
    </p>
 */ 
allocator.interval.prototype.split = function (pos)
{
    var i, r, range, newrange, u;
    var newranges = [];
    var newUsePositions = [];
    var newInterval;

    // There is no splitting to be done at the beginning 
    // of an interval
    if (pos === this.startPos()) 
    {
        throw "SplitError: position '" + pos + "' happens at the" + 
              " beginning of the interval";
    }

    // Find the first range intersecting at or after
    // pos
    for (r=0; r < this.ranges.length; ++r)
    {
        range = this.ranges[r];
        if (range.cmp(pos) < 1)
        {
            break;
        }
    }

    // There is no splitting to be done at the end of 
    // an interval
    if (r === this.ranges.length) 
    {
        throw "SplitError: position '" + pos + "' happens after last range";
    }

    // Handle both cases, where pos is either on an 
    // interval on in a hole. We want to split just before
    // the current position, so let's exclude the startPos.
    if (range.covers(pos, false))
    {
        // A new range is created with pos as startPos
        // and old endPos as endPos
        newrange = allocator.range(pos, range.endPos);

        // Replace the last range with a range having 
        // the same start position but pos as an end position
        this.ranges[r] = allocator.range(range.startPos, pos);

        // all the remaining ranges are moved into
        // the new interval
        newranges.push(newrange);
        for (i=r+1; i < this.ranges.length; ++i)
        {
            newranges.push(this.ranges[i]);
        }

        // remove from the ranges ranges that have been moved
        this.ranges.length = r + 1;

        // Move all the use positions after position
        // in the child.  We keep a use position at endPos
        // of a range in the parent.
        u = allocator.findPos(this.usePositions, true,
                              function (u) { return u.pos > pos; }); 

        if (u !== null) 
        { 
            // Move all the remaining use positions in the next
            // interval
            newUsePositions = this.usePositions.splice(u, 
                                   this.usePositions.length - u); 
        }
    } else 
    {
        // We are in a hole, simply move all the remaining
        // ranges in a new interval
        for (i=r; i < this.ranges.length; ++i)
        {
            newranges.push(this.ranges[i]);
        }

        // remove from the ranges of the initial
        this.ranges.length = r;

        // Move also all the use positions equal or greater than pos 
        // to the child interval.
        // We need to test for equality of position because a use position
        // might occur at startPos a range, in which case it should
        // be part of the child.
        u = allocator.findPos(this.usePositions, true,
                              function (u) { return u.pos >= pos; }); 
        if (u !== null) 
        { 
            // Move all the remaining use positions in the next
            // interval
            newUsePositions = this.usePositions.splice(u, 
                                   this.usePositions.length - u); 
        }

    }


    // Create the new interval
    newInterval = allocator.interval(newranges, newUsePositions);
    newInterval.vreg = this.vreg;

    // In some cases, an interval might be split again before the 
    // last split position. Let's insert the new interval in-between.
    if (this.next !== null)
    {
        this.next.previous = newInterval;
        newInterval.next = this.next;
    }

    this.next = newInterval;
    newInterval.previous = this;

    return newInterval;
};

/** Set the start position of the interval */
allocator.interval.prototype.setStartPos = function (pos)
{
    if (this.ranges.length === 0)
    {
        return;
    }
    
    this.ranges[0].startPos = pos;
};

/** 
    Add a range to an interval, merging ranges when they overlap.
    Ranges are assumed to be inserted in decreasing start position.

    @param {Number} startPos
    @param {Number} endPos
*/
allocator.interval.prototype.addRange = function (startPos, endPos)
{
    var i = 0;
    var current = null;
    var newRange = allocator.range(startPos, endPos);

    // Find the range before which this should be inserted
    for (i=0; i < this.ranges.length; ++i)
    {
        current = this.ranges[i];
        if (current.startPos >= newRange.startPos)
        {
            break; 
        } 
    }
   
    // No value could be found 
    if (i === this.ranges.length)
    {
        current = null;
    }

    // Merge or not 
    if (current && newRange.endPos >= current.startPos)
    {
        newRange.start = Math.min(newRange.startPos, current.startPos);
        newRange.endPos = Math.max(newRange.endPos, current.endPos);

        this.ranges.splice(i, 1, newRange);
    } else
    {
        // Add the new range without merging
        this.ranges.splice(i, 0, newRange);
    }
}

/** 
    Add a use position to an interval.
    @param {Number} pos
    @param {allocator.usePos.registerFlag} registerFlag, defaults to NONE
*/
allocator.interval.prototype.addUsePos = function (pos, registerFlag)
{
    // TODO: use positions should be sorted in a way that allows
    //       constant time element adding
    if ((this.usePositions.length > 0 && this.usePositions[0].pos > pos) ||
        this.usePositions.length === 0)
    { 
        this.usePositions.unshift(allocator.usePos(pos, registerFlag));
    } else
    {
        // TODO: We should insert at the right place
        throw "addUsePos: positions not inserted in decreasing order";
    }
};

/** Next used after or equal to given position, 
    satisfying flag property.

    @param {Number} pos defaults to 0 
    @param {allocator.usePos.registerFlag} registerFlag defaults
                                           to any flag 
*/ 
allocator.interval.prototype.nextUse = function (pos, registerFlag)
{
    var i;

    pos = pos || 0;

    for (i=0; i < this.usePositions.length; ++i)
    {
         if (this.usePositions[i].pos >= pos && 
             ( !registerFlag ||
             this.usePositions[i].registerFlag === registerFlag))
        {
            break;
        }
    }

    if (i === this.usePositions.length)
    {
        return Infinity;           
    } else
    {
        return this.usePositions[i].pos;
    }
};

/** 
    Returns the physical register or memory location assigned
    at a specific position on the interval.
    @param {Number} pos
*/
allocator.interval.prototype.regAtPos = function (pos)
{
    var interval = this;
    while (interval !== null)
    {
        if (pos >= interval.startPos() && pos <= interval.endPos())
        {
            return interval.reg;
        }
        interval = interval.next;
    } 

    return null;
};

//-----------------------------------------------------------------------------

// Block Ordering

/**
Produce a linear order for the blocks in a CFG
*/
allocator.orderBlocks = function (cfg)
{
    // Initialize the register allocation information for this block
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        if (cfg.blocks[i].regAlloc === undefined)
        {
            cfg.blocks[i].regAlloc = {
                // Number of forward branches to this block
                numForwBranch   : 0,    
                // Number of backward branches to this block
                numBackBranch   : 0,    
                // Number of exception branches to this block
                numExcpBranch   : 0,    

                // Loop identifier index
                loopIndex       : -1,   
                // Loop nesting depth
                loopDepth       : 0,    
                // For loop ends, corresponding loop header
                loopHeader      : null, 
                // For loop headers, last corresponding loop end block
                lastLoopEnd     : null, 
                // For loop blocks, loops to which it belongs
                loops           : [],   

                // Weight of a block in the block ordering
                weight          : 0,    
                // Operation number at the start of the block
                from            : -1,   
                // Operation number at the end of the block
                to              : -1,   
                // Live set at the block entry
                liveIn          : null  
            };
        } else
        {
            cfg.blocks[i].regAlloc.numForwBranch   = 0;    
            cfg.blocks[i].regAlloc.numBackBranch   = 0;    
            cfg.blocks[i].regAlloc.numExcpBranch   = 0;    

            cfg.blocks[i].regAlloc.loopIndex       = -1;   
            cfg.blocks[i].regAlloc.loopDepth       = 0;    
            cfg.blocks[i].regAlloc.loopHeader      = null; 
            cfg.blocks[i].regAlloc.lastLoopEnd     = null; 
            cfg.blocks[i].regAlloc.loops           = [];   

            cfg.blocks[i].regAlloc.weight          = 0;    
        }
    }

    // Next loop index to assign
    var nextLoopIndex = 0;
    
    // List of loop end blocks
    var loopEnds = [];

    // Stack for the CFG traversal
    var stack = [cfg.getEntryBlock()];

    // Arrays to mark visited and active blocks
    var visited = [];
    var active = [];

    // Until the stack is empty
    while (stack.length > 0)
    {
        var block = stack[stack.length - 1];

        // If we are done visiting this block
        if (visited[block.blockId])
        {
            active[block.blockId] = undefined;
            stack.pop();
            continue;
        }

        // Mark the block as visited and active
        visited[block.blockId] = true;
        active[block.blockId] = true;

        // Get the branch instruction at the end of the block
        var branchInstr = block.getLastInstr();

        // If the branch can throw to an exception handler, get its throw target
        var throwTarget = (branchInstr instanceof ExceptInstr)? branchInstr.getThrowTarget():null;

        // For each successor
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];

            if (!visited[succ.blockId])
                stack.push(succ);

            // If this is a backward branch
            if (active[succ.blockId])
            {
                // Assign the loop header a loop index
                succ.regAlloc.loopIndex = nextLoopIndex++;

                // Increment the number of backward branches to the block
                succ.regAlloc.numBackBranch++;

                // Set the loop header for this loop end
                block.regAlloc.loopHeader = succ;

                // Add this block to the loop end list
                loopEnds.push(block);
            }
            else
            {
                // Increment the number of forward branches to the block
                succ.regAlloc.numForwBranch++;
            }

            // If this is the throw target of the block
            if (succ === throwTarget)
            {
                // Increment the number of exception branches to the block
                succ.regAlloc.numExcpBranch++;
            }
        }
    }

    // For each loop end block
    for (var i = 0; i < loopEnds.length; ++i)
    {
        var loopEnd = loopEnds[i];

        // Get the loop header for this loop
        var loopHeader = loopEnd.regAlloc.loopHeader;

        // Array to mark visited blocks
        var visited = [];

        // Stack for the CFG traversal
        var stack = [loopEnd];

        // Until the stack is empty
        while (stack.length > 0)
        {
            var block = stack.pop();

            // Mark this block as visited
            visited[block.blockId] = true;

            // Update the loop set for this block
            block.regAlloc.loops.push(loopHeader);

            // Update the loop depth for this block
            block.regAlloc.loopDepth = block.regAlloc.loops.length;

            // If this is the loop header, don't visit predecessors
            if (block === loopHeader)
                continue;

            // For each predecessor
            for (var j = 0; j < block.preds.length; ++j)
            {
                var pred = block.preds[j];

                if (!visited[pred.blockId])
                    stack.push(pred);
            }
        }
    }

    // Function to compute a block's weight for the block ordering
    function blockWeight(block)
    {
        // Loop nesting increases the weight
        // Exception handlers get weighed down
        return (block.regAlloc.loopDepth) - (5 * block.regAlloc.numExcpBranch);
    }

    // Assign a weight to each block
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];
        block.regAlloc.weight = blockWeight(block);
    }

    // Function to sort blocks for the block order computation
    function blockSortFunc(b1, b2)
    {
        return b1.regAlloc.weight >= b2.regAlloc.weight;
    }

    // Final block order list
    var blockOrder = [];

    // Work list for the block order computation
    var workList = new LinkedList();

    // Add the entry block to the work list
    workList.addLast(cfg.entry);

    // Number of forward branches seen for each block
    var numForwSeen = [];

    // Until the work list is empty
    while (!workList.isEmpty())
    {
        // Remove the block with the highest weight
        var block = workList.remFirst();

        // Append the block to the block ordering
        blockOrder.push(block);

        // For each successor of the block
        for (var i = 0; i < block.succs.length; ++i)
        {
            var succ = block.succs[i];

            // Increment the number of incoming forward branches seen for this successor
            if (!numForwSeen[succ.blockId])
                numForwSeen[succ.blockId] = 1;
            else
                numForwSeen[succ.blockId]++;

            // If all forward branches have been seen
            if (numForwSeen[succ.blockId] == succ.regAlloc.numForwBranch)
            {
                // Add the block to the work list, sorted by decreasing weight
                workList.addSorted(succ, blockSortFunc);
            }
        }
    }

    // Compute the last loop end for header blocks
    for (var i = 0; i < blockOrder.length; ++i)
    {
        var block = cfg.blocks[i];

        if (block.regAlloc.loopHeader)
            block.regAlloc.loopHeader.regAlloc.lastLoopEnd = block;
    }

    /*
    print('Loop depth:');
    for (var i = 0; i < cfg.blocks.length; ++i)
    {
        var block = cfg.blocks[i];

        print(block.getBlockName());
        print(block.regAlloc.loopDepth);
    }

    print('Final block order:')
    for (var i = 0; i < blockOrder.length; ++i)
    {
        print(blockOrder[i].getBlockName());
        
    }
    */ 

    // Return the produced block order and the block information computed
    return blockOrder
};

/**
Perform instruction numbering on a control flow graph
*/
allocator.numberInstrs = function (cfg, order, config)
{
    var nextNo = 2;
    var inc = allocator.numberInstrs.inc;
    var instrNb;
    var i,j, block, instr;

    // For each block in the order
    for (i = 0; i < order.length; ++i)
    {
        block = order[i];

        // Set the operation number at the block start
        block.regAlloc.from = nextNo;

        // All phi instructions happen at the same time at the beginning of the
        // block so they should have the same instruction number as the beginning 
        // of the block
        for (j = 0; j < block.instrs.length; ++j)
        {
            instr = block.instrs[j];

            // Conceptually, phi instructions all happen at the
            // same time, so they should have the same instruction number
            if (instr instanceof PhiInstr)
            {
                // Since they all appear at the beginning of the block,
                // not incrementing should garantee an identical number
                instrNb = nextNo;

                // Create a register allocation info object for the instruction and
                // assign the instruction an operation number.
                // The regAlloc object is specific to the instance.
                instr.regAlloc = Object.create(instr.regAlloc);

                instr.regAlloc.id = instrNb; // Operation number
                instr.regAlloc.interval = allocator.interval(); // Live interval
                instr.regAlloc.interval.instr = instr;
            } 
            else
            {
                break;
            }   
        }

        nextNo = nextNo + inc;

        //print(block.regAlloc.from + ": label " + block.label);
        // Handle the remaining instructions
        for (; j < block.instrs.length; ++j)
        {
            instr = block.instrs[j];

            if (instr.regAlloc.usedRegisters(instr, config))
            {
                // For instructions using supplementary registers, 
                // we need to increment by twice the value
                // to later separate arguments use position from return value
                // use position so that a fixed interval can be introduced
                // between the two.
                instrNb = nextNo + inc;
                nextNo = nextNo + 2*inc;
            }
            else
            {
                // We got a regular instruction
                instrNb = nextNo;
                nextNo = nextNo + inc;
            }

            // Create a register allocation info object for the instruction and
            // assign the instruction an operation number.
            // The regAlloc object is specific to the instance.
            instr.regAlloc = Object.create(instr.regAlloc);

            instr.regAlloc.id = instrNb; // Operation number
            instr.regAlloc.interval = allocator.interval(); // Live interval
            instr.regAlloc.interval.instr = instr;
            //print(instr.regAlloc.id + ":    " + " (" + instr.instrId + ") " +
            //      instr );
        }

        // Set the operation number at the block end to be the same
        // as the last instruction
        block.regAlloc.to = nextNo;
    }
};
// Increment number between instructions
allocator.numberInstrs.inc = 2;
//-----------------------------------------------------------------------------

// Interval Calculation
// TODO: call sites for fixed positions
/**
Compute the live intervals for the temporaries of a CFG
*/
allocator.liveIntervals = function (cfg, order, config)
{
    var pos, it;

    // For each block in the order, in reverse order
    for (var i = order.length - 1; i >= 0; --i)
    {
        var block = order[i];

        // Variable for the currently live set
        var live = [];

        // For each successor
        for (var j = 0; j < block.succs.length; ++j)
        {
            var succ = block.succs[j];

            // For each instruction of the successor
            for (var k = 0; k < succ.instrs.length; ++k)
            {
                var instr = succ.instrs[k];

                // If this is not a phi instruction, stop
                if (!(instr instanceof PhiInstr))
                    break;

                // Add the phi node's input from this block to the live set
                if (!(instr.getIncoming(block) instanceof ConstValue))
                {
                    arraySetAdd(live, instr.getIncoming(block));
                }
            }

            // If this is a loop header, skip it
            if (succ.regAlloc.lastLoopEnd === block)
                continue;            

            // Add all live temps at the successor input to the live set
            live = arraySetUnion(live, succ.regAlloc.liveIn);
        }
        
        // For each instruction in the live set
        for (var j = 0; j < live.length; ++j)
        {
            var instr = live[j];
           
            // Add a live range spanning this block to its interval
            instr.regAlloc.interval.addRange(
                block.regAlloc.from,
                block.regAlloc.to  
            );
        }

        // For each instruction of the block, in reverse order
        for (var j = block.instrs.length - 1; j >= 0; --j)
        {
            var instr = block.instrs[j];

            // For instruction having an output
            if (instr.hasDests())
            {
                // The output of the instruction starts being live here
                instr.regAlloc.interval.setStartPos(instr.regAlloc.id);
                instr.regAlloc.interval.addUsePos(instr.regAlloc.id);
                    // TODO: Check in which cases we might need the required flag
                    //allocator.usePos.registerFlag.REQUIRED);

                //print( instr.instrId + " startPos: " + instr.regAlloc.id);
                //print( "new interval: " + instr.regAlloc.interval);
                instr.regAlloc.interval.regHint = 
                    instr.regAlloc.outRegHint(instr, config);

                // Remove the instruction from the live set
                arraySetRem(live, instr);

            }

            // Input operands for phi instructions are added to the live set
            // when traversing successors
            if (instr instanceof PhiInstr)
            {
                continue;
            }

            // For each input operand of the instruction
            for (var k = 0; k < instr.uses.length; ++k)
            {
                var use = instr.uses[k];

                if (!(use instanceof IRInstr))
                    continue;

                // Make the use live from the start of the block to this 
                // instruction
                //print( use.regAlloc.interval);

                if (instr.regAlloc.usedRegisters(instr, config))
                {
                    // For instructions using supplementary registers, 
                    // we separate argument position from return
                    // position to later introduce a fixed interval between 
                    // the two
                    pos = instr.regAlloc.id - allocator.numberInstrs.inc;
                    use.regAlloc.interval.addRange(
                        block.regAlloc.from,
                        pos
                    );
                } 
                else
                {
                    pos = instr.regAlloc.id;
                    use.regAlloc.interval.addRange(
                        block.regAlloc.from,
                        pos
                    );
                }

                use.regAlloc.interval.regHint = 
                    instr.regAlloc.opndsRegHint(instr, config, k);

                if (instr.regAlloc.opndsRegRequired)
                {
                    use.regAlloc.interval.addUsePos(pos,
                                        allocator.usePos.registerFlag.REQUIRED);
                } 
                else
                {
                    use.regAlloc.interval.addUsePos(pos);
                }

                //print( use.instrId + " from:" + block.regAlloc.from +
                //       " to:" + instr.regAlloc.id);
                //print( use.regAlloc.interval);

                // Add this input operand to the live set
                arraySetAdd(live, use);
            }
        }

        // Remove all phi instructions of this block from the live set
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            if (!(instr instanceof PhiInstr))
                break;

            arraySetRem(live, instr);
        }

        // Get the last loop end associated with this block, if any
        var lastLoopEnd = block.regAlloc.lastLoopEnd;

        // If this block is a loop header
        if (lastLoopEnd)
        {
            // For each temp in the live set at the block entry 
            // (live before the block)
            for (var j = 0; j < live.length; ++j)
            {
                var instr = live[j];

                // Add a live range spanning the whole loop
                instr.regAlloc.interval.addRange(
                    block.regAlloc.from,
                    lastLoopEnd.regAlloc.to
                );
            }
        }

        // Store the live temp set at the block entry
        block.regAlloc.liveIn = live;
    }

    var liveIntervals = [];
    // Extract live intervals
    for (var i = 0; i < order.length; ++i)
    {
        var block = order[i];

        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];
            instr.regAlloc.interval.vreg = instr.instrId;
            if (instr.regAlloc.interval.rangeNb() > 0)
            {
                liveIntervals.push(instr.regAlloc.interval);
            }
        }
    }
    return liveIntervals;
};

allocator.fixedIntervals = function (cfg, config)
{
    const physReg = config.physReg;
    var fixed = physReg.map(function () { return allocator.interval(); });
    var it;
    var argPos;
    var addFixed;

    for (it = cfg.getInstrItr(); it.valid(); it.next())
    {
        if (it.get().regAlloc.usedRegisters(it.get(), config))
        {
            // Add a fixed interval between the arguments position
            // and the return value position
            argPos = it.get().regAlloc.id - allocator.numberInstrs.inc; 
            addFixed = function (index) 
                       { 
                           fixed[index].addRange(argPos, argPos + 1); 
                       };
            it.get().regAlloc.usedRegisters(it.get(), config).forEach(addFixed);
        }
    }
    return fixed;
}

//-----------------------------------------------------------------------------

// Register allocation

/** 
    Assign a physical register or memory location to every interval.

    The mems object should have a newSlot() method returning 
    a new memory location for spilling.

    @param {Object} config   configuration containing platform specific
                             register information
    @param {Array} unhandled list of unassigned intervals
    @param {Object} mems     object allocating memory locations used 
                             for spilling
    @param {Array} fixed     list of intervals where registers
                             are unavailable
*/ 
allocator.linearScan = function (config, unhandled, mems, fixed)
{
    //       Interval registers are indexes into the pregs array during
    //       allocation, but are replaced by their pregs object
    //       at the end.
    const pregs = config.physReg;
    
    // Queue of all unhandled intervals, sorted by start position,
    // using a copy of unhandled
    var unhandledQueue = allocator.priorityQueue(unhandled.slice(0),
        function (it1, it2)
        {
            return (it1.startPos() - it2.startPos());
        });

    // TODO: Check if for performance reasons, activeSet and
    // inactiveSet should be sorted by register index

    // Set of all intervals live at the current position
    var activeSet   = allocator.set([]);
    // Set of all intervals in a "hole" at the current position
    var inactiveSet = allocator.set([]);
    // Set of all intervals in a "hole" at the current position
    var handledSet = allocator.set([]);

    // Instruction numbers where a range starts to cover a 
    // register again
    var freeUntilPos = new Array(pregs.length);

    // Instruction numbers where an interval uses the register
    // the next time inside a range
    var nextUsePos = new Array(pregs.length);

    // Initialize fixed if undefined
    if(fixed === undefined || fixed.length !== pregs.length)
    {
        fixed = [];
        fixed.length = pregs.length;
        for (i=0; i < pregs.length; ++i)
        {
            fixed[i] = allocator.interval(); 
        }
    }

    /** @ignore */
    function assignRegister(it)
    {
        if (typeof it.reg === "number")
        {
            it.reg = pregs[it.reg];
        }
    }

    /** @ignore */
    function assignSpillSlot(it)
    {
        var slot = null;
        var previous = it.previous;

        while(previous !== null)
        {
            if (typeof previous.reg !== "number")
            {
                // We have found an existing spill slot,
                // let's reuse it
                slot = previous.reg;
                break;
            }
            previous = previous.previous;
        }

        if (slot === null)
        {
            slot = mems.newSlot();
        }

        it.reg = slot;
    }
    // Iteration vars
    var current, position, it, i;

    /** @ignore */
    function tryAllocateFreeReg(current)
    {
        // Iteration vars
        var it, i, reg;
        
        // Reset freeUntilPos for all physical registers 
        for (i=0; i < freeUntilPos.length; ++i)
        {
            freeUntilPos[i] = Infinity;
        }

        // For each active, register is never available
        for (i=0; i < activeSet.length(); ++i)
        {
            it = activeSet.values[i];
            freeUntilPos[it.reg] = 0;
        }

        // For each inactive, register is available until next
        // intersection with current
        for (i=0; i < inactiveSet.length(); ++i)
        {
            it = inactiveSet.values[i];
            freeUntilPos[it.reg] = Math.min(it.nextIntersection(current),
                                            freeUntilPos[it.reg]);
        }

        // Note: Paper did not consider the occurence of fixed interval
        //       in the regular case
        // For fixed interval, register is available until next
        // intersection with current
        for (i=0; i < fixed.length; ++i)
        {
            it = fixed[i];
            freeUntilPos[i] = Math.min(it.nextIntersection(current),
                                       freeUntilPos[i]);
        }


        
        if (current.regHint !== null && typeof current.regHint === "number" &&
            freeUntilPos[current.regHint] > current.startPos())
        {
            // If the register hint is an index into the register
            // available for allocation, use it if it is free
            reg = current.regHint;
        } else if (current.regHint !== null && 
                   typeof current.regHint !== "number")
        {
            // If the register hint is not a number, use it as is, so
            // register unavailable for allocation might still be used directly.
            reg = current.regHint; 
        } else {
            // Either no register hint was given or the register is not
            // available, use one of the availables
            reg = allocator.max(freeUntilPos).index;
        }

        // Original algorithm said allocation failed if freeUntilPos[reg] === 0         // but it was too weak, allocation should fail unless a register
        // is free for a part of current.  This way it handles 
        // a fixed interval starting at the same position as current
        if (typeof reg !== "number")
        {
            // Always succeed when a register not available for allocation
            // is used
            current.reg = reg;
            return true;
        } else if(freeUntilPos[reg] <= current.startPos())
        {
            // No register available without spilling
            return false;
        } else if (current.endPos() <= freeUntilPos[reg])
        {
            // Register available for the whole interval
            current.reg = reg;
            return true;
        } else
        {
            // Split the current interval so that the first part
            // of the interval is in a register and the second
            // is added for further allocation
            current.reg = reg;
            unhandledQueue.enqueue(current.split(freeUntilPos[reg]));
            return true;
        }
    };

    /** @ignore */
    function allocateBlockedReg(current, position)
    {
        const regFlag = allocator.usePos.registerFlag;

        // Iteration vars
        var it, i, reg, max, pos, nextRegPos, next, fixedPos;
        
        // Reset nextUsePos for all physical registers 
        for (i=0; i < nextUsePos.length; ++i)
        {
            nextUsePos[i] = Infinity;
        }

        // For each active, find the next use after start of current 
        for (i=0; i < activeSet.length(); ++i)
        {
            it = activeSet.values[i];
            nextUsePos[it.reg] = it.nextUse(current.startPos());
        }
       
        // TODO: We might want to create a nextUseIntersection()
        //       so that its complexity is not quadratic on use positions 
        // For each inactive, find the next use intersecting with current 
        for (i=0; i < inactiveSet.length(); ++i)
        {
            it = inactiveSet.values[i];
            next = it.nextUse(current.startPos());
            while (!(next === Infinity || current.covers(next)))
            {
                next  = it.nextUse(next+1);
            }
            nextUsePos[it.reg] = Math.min(next, nextUsePos[it.reg]);
        }

        // Again, algorithm in paper did not really address the 
        // fixed interval completely so we will say that 
        // fixed intervals are used at the beginning of their 
        // intersection with current
        for (i=0; i < fixed.length; ++i)
        {
            it = fixed[i];
            nextUsePos[i] = Math.min(it.nextIntersection(current),
                                     nextUsePos[i]);
        }

        max = allocator.max(nextUsePos);
        reg = max.index;
        pos = max.value;

        // If first use of current is equal to pos,
        // we should split the other interval, according to the paper.
        // However, if we do that and the splitted interval
        // has a first use still equal to pos, we are screwed since
        // we will end up in the same situation the next time around
        // (See Test Case 2 of Linear Scan Allocation).
        // Let's spill current instead.
        if (current.nextUse() >= pos)
        {
            // All other intervals are used before current
            // so it is best to spill current itself
            assignSpillSlot(current);

            // If a register is required, we split the interval
            // just before its use
            nextRegPos = current.nextUse(0, regFlag.REQUIRED);
            if (nextRegPos !== Infinity)
            {
                unhandledQueue.enqueue(current.split(nextRegPos - 1));
            }
        } else 
        {
            // spill current interval that currently block reg
            current.reg = reg;

            // split active interval for reg at position
            it = allocator.find(activeSet.values, reg, 
                                function (it) { return it.reg; });

            unhandledQueue.enqueue(it.split(position));

            // split any inactive interval for reg at the end of its
            // lifetime hole
            for (i=0; i < inactiveSet.length(); ++i)
            {
                it = inactiveSet.values[i];
                if (it.reg === reg)
                {
                    unhandledQueue.enqueue(it.split(position)); 
                }
            }
           
            fixedPos = current.nextIntersection(fixed[reg]); 
            if (fixedPos !== Infinity)
            {
                // The register has a fixed interval, covering a part 
                // of current, we need to split
                unhandledQueue.enqueue(current.split(fixedPos));
            }
        }
    };


    while (unhandledQueue.length() > 0)
    {
        
        current  = unhandledQueue.dequeue();
        position = current.startPos();

        var values = activeSet.values.slice(0);

        // check for intervals in active that are handled or inactive
        for (i=0; i < values.length; ++i)
        {
            it = values[i];

            if (it.endPos() <= position)
            {
                activeSet.remove(it);
                handledSet.add(it);
                
            } else if (!it.covers(position))
            {
                activeSet.remove(it);
                inactiveSet.add(it);
            }
        }


        var values = inactiveSet.values.slice(0);

        // check for intervals in inactive that are handled or active
        for (i=0; i < values.length; ++i)
        {
            it = values[i];

            if (it.endPos() < position)
            {
                inactiveSet.remove(it);
                handledSet.add(it);
            } else if (it.covers(position))
            {
                inactiveSet.remove(it);
                activeSet.add(it);
            }
        }


        // find a register for current
        if (!tryAllocateFreeReg(current))
        {
            allocateBlockedReg(current, position);
        }

        // If current has been assigned to a register
        // add it to actives
        if (typeof current.reg  === "number") 
        {
            activeSet.add(current);
        }
    } 


    // Clean up data structures and 
    // assign physical registers to intervals
    for (i=0; i < activeSet.length(); ++i)
    {
        it = activeSet.values[i];
        assignRegister(it);
    }

    for (i=0; i < inactiveSet.length(); ++i)
    {
        it = inactiveSet.values[i];
        assignRegister(it);
    }

    for (i=0; i < handledSet.length(); ++i)
    {
        it = handledSet.values[i];
        assignRegister(it);
    }
};

/** 
    Assign registers and memory location to each instruction. Register
    allocation must have been done before calling assign.

    @param cfg          Control Flow Graph 
*/
allocator.assign = function (cfg, config)
{
    var it, instr, opndIt, opnds, dest, pos, opnd;
    for (it = cfg.getInstrItr(); it.valid(); it.next())
    {
        instr = it.get();
        opnds = []; 
        dest  = null; 
        pos = instr.regAlloc.id;
        opndPos = pos - allocator.numberInstrs.inc;

        for (opndIt = instr.getUseItr(); opndIt.valid(); opndIt.next())
        {
            opnd = opndIt.get(); 

            if (opnd instanceof IRInstr)
            {
                if (instr instanceof PhiInstr)
                {
                    // For phi instructions, make sure we used the same 
                    // register as used for SSA deconstruction
                    opnds.push(opnd.regAlloc.interval.
                              regAtPos(instr.preds[opndIt.index].regAlloc.to));
                } else if (instr.regAlloc.usedRegisters(instr, config))
                {
                    opnds.push(opnd.regAlloc.interval.regAtPos(opndPos));
                } else                 
                {
                    opnds.push(opnd.regAlloc.interval.regAtPos(pos));
                }
            } else 
            {
                opnds.push(opnd);
            }
        }

        if (instr.hasDests())
        {
            dest = instr.regAlloc.interval.regAtPos(pos);
        }

        instr.regAlloc.opnds = opnds;
        instr.regAlloc.dest = dest;
    }
};

/** 
    Resolve location differences introduced by splitting during
    register allocation and replace Phi instructions by equivalent 
    Move instructions.
    @param cfg          Control Flow Graph 
    @param intervals    Intervals after register allocation
    @param order        A Linear order of all the basic blocks
*/
allocator.resolve = function (cfg, intervals, order)
{
    function liveAtBegin (succ)
    {
        var pos = succ.regAlloc.from;
        return function (interval) { return interval.covers(pos); };
    };
    function withinBounds(pos, block)
    {
        return pos >= block.regAlloc.from && pos <= block.regAlloc.to;
    };

    function insertInstr(block, instr, pos)
    {
        var itr = block.getInstrItr();
        var insertPos = 0; 

        for (; itr.valid(); itr.next())
        {
            if (itr.get().regAlloc !== undefined &&
                itr.get().regAlloc.id >= pos)
            {
                break;
            } else
            {
                insertPos++;
            }
        }
        
        block.addInstr(instr, "", insertPos);
    };
    
    var edgeIt, intervalIt, moveIt, blockIt, edge, moveFrom, moveTo;
    var interval;
    var moves = [];
    var pos, offset, blockOffset;
    var mapping;
    var insertFct;
    var insertIndex;
    var opnd;
    var newblock;
    var blocksToInsert = [];
    var insertIt;

    // Insert Moves at split positions
    for (intervalIt = new ArrayIterator(intervals); 
         intervalIt.valid(); 
         intervalIt.next())
    {
        interval = intervalIt.get();
        while (interval !== null)
        {
            
            if (interval.previous !== null &&
                interval.previous.reg !== interval.reg)
            {
                moves.push([interval.previous.endPos(),
                            new MoveInstr(interval.previous.reg,
                                          interval.reg)]);
            }
            interval = interval.next;
        }
    }
    moves.sort(function (a,b) { return a[0] - b[0]; });


    moveIt = new ArrayIterator(moves);
    blockIt = new ArrayIterator(order); 

    while (moveIt.valid() && blockIt.valid())
    {
        blockOffset = blockIt.get().regAlloc.from;

        if (withinBounds(moveIt.get()[0], blockIt.get()))
        {
            // We do a linear search since the correspondance between
            // the insertion index and the instruction position varies
            // since some instructions occupies more than 2 positions 
            // (i.e. calls)
            insertInstr(blockIt.get(), moveIt.get()[1], moveIt.get()[0]);
            moveIt.next();
        } else
        {
            blockIt.next();
        }
    }

    // Resolve differences introduced by splitting in different
    // basic blocks
    for (edgeIt = cfg.getEdgeItr(); edgeIt.valid(); edgeIt.next())
    {
        mapping = allocator.mapping();
        edge = edgeIt.get();
        for (intervalIt = new FilterIterator(new ArrayIterator(intervals), 
                                                 liveAtBegin(edge.succ));
             intervalIt.valid(); 
             intervalIt.next())
        {
            if (intervalIt.get().startPos() === edge.succ.regAlloc.from)
            {
                assert(intervalIt.get().instr instanceof PhiInstr);

                // We got a Phi instruction
                opnd = intervalIt.get().instr.getIncoming(edge.pred);

                if (opnd instanceof ConstValue)
                {
                    moveFrom = opnd;
                } else
                {
                    moveFrom = opnd.regAlloc.interval.regAtPos(edge.pred.regAlloc.to);
                }
            } else
            {
                // We got a regular instruction
                moveFrom = intervalIt.get().regAtPos(edge.pred.regAlloc.to);
            }


            moveTo = intervalIt.get().regAtPos(edge.succ.regAlloc.from);


            if (moveFrom !== moveTo)
            {
               mapping.add(moveFrom, moveTo);
            }
        }

        if (mapping.length === 0)
        {
            continue;
        }

        // Order and insert moves
        if (edge.pred.succs.length === 1)
        {
            // Predecessor has only a successor, insert moves at
            // the end of predecessor
            insertFct = function (move) 
            { 
                if (edge.pred.hasBranch())
                {
                    edge.pred.addInstr(move, "", edge.pred.instrs.length - 1); 
                } else
                {
                    edge.pred.addInstr(move); 
                }
            };

        } else if (edge.succ.preds.length === 1)
        {
            // Successor has only a predecessor, insert moves
            // at the beginning of successor
            insertFct = function (move) 
                        { 
                            edge.succ.addInstr(move,"", insertIndex++);
                        };
            insertIndex = 0;

        } else
        {
            // We need to introduce an additional block to insert
            // the move instructions
            newblock = cfg.getNewBlock("ssa_dec");
            blocksToInsert.push({edge:edge, block:newblock});

            insertFct = function (move) 
                        { 
                            newblock.addInstr(move,"", insertIndex++);
                        };
            insertIndex = 0;
        }
        mapping.orderAndInsertMoves(insertFct);
    }

    for (insertIt = new ArrayIterator(blocksToInsert);
         insertIt.valid();
         insertIt.next())
    {
        cfg.insertBetween(insertIt.get().edge.pred,
                          insertIt.get().edge.succ,
                          insertIt.get().block);
    }

    // Reorder blocks, taking into account the new blocks inserted
    return allocator.orderBlocks(cfg);
};

/** 
    @private Mapping to order move instructions 

    Since move instructions read from only one register and 
    write only in one register and the same register cannot
    be the target of multiple moves, the graph of dependencies
    between move instructions should have the following properties:

    1. No nested cycles
    2. Each cycle is independent, i.e. no node part of a cycle
       has a dependency towards a node part of another cycle
    3. A cycle might be either simple (no path exist between
       a node part of the cycle and one not part of it)
       or has a leading 'arm' of nodes not part of it

    Note: The vocabulary used might not corresponds to the 
          correct terminology of graph theory. In the meantime,
          it might be useful to revisit it.

*/
allocator.mapping = function ()
{
    var that = Object.create(allocator.mapping.prototype);
    that.read = {};
    that.write = {};
    that.length = 0;
    return that;
};

allocator.mapping.prototype.add = function (from, to)
{
    var mov = new MoveInstr(from, to);

    if (this.read[from] === undefined)
    {
        this.read[from] = [];
    }
    this.read[from].push(mov);

    if (this.write[from] === undefined)
    {
        this.write[from] = null;
    }

    if (this.read[to] === undefined)
    {
        this.read[to] = [];
    }

    if (this.write[to] !== null && this.write[to] !== undefined)
    {
        error("Multiple moves to the same destination");
    }
    this.write[to] = mov;

    this.length++;
};

allocator.mapping.prototype.orderAndInsertMoves = function (insertFct, temp)
{

    // TODO: We might want to use the Gabow's or Tarjan's algorithm
    // for finding the strongly connected components of the graph instead

    var regName;
    var g = graph.adjencyList();
    var i;
    var readValue;
    var writeValue;
    var moveIt;
    var cycle;
    var cycles = [];
    var cycleIt;
    var move;
    var tempMove;

    for (regName in this.read)
    {
        if (!this.read.hasOwnProperty(regName)) 
        {
            continue;
        }
        readValue = this.read[regName];
        writeValue = this.write[regName];
        if (readValue.length > 0)
        {
            for (i=0; i < readValue.length; ++i)
            {
                if (writeValue !== null)
                {
                    g.addEdge(readValue[i], writeValue); 
                } else
                {
                    g.addNode(readValue[i]);
                }
            }
        } else if (writeValue !== null)
        {
            g.addNode(writeValue);
        }
    }

    cycle = g.removeCycle(true); 
    while (cycle !== null)
    {
        cycles.push(cycle);
        cycle = g.removeCycle(true);
    }

    for (moveIt = g.getNodeItr("topologicalSort"); 
        moveIt.valid();
        moveIt.next())
    {
        insertFct(moveIt.get());
    }

    if (cycles.length > 0)
    {
        assert(temp !== undefined, 
               "A temporary memory location should be provided");

        for (cycleIt = new ArrayIterator(cycles);
             cycleIt.valid();
             cycleIt.next())
        {
            // Break the cycle by moving the first value to a temporary
            // location
            move = cycleIt.get().pop();
            insertFct(new MoveInstr(move.uses[0], temp));

            // Store the last move
            moveTemp = new MoveInstr(temp, move.uses[1]);
            
            // Insert all other moves
            while (move !== undefined)
            {
                insertFct(cycleIt.get().pop());
                move = cycleIt.get().pop();
            }

            // Insert the last move
            insertFct(moveTemp);
        }

    }
};

allocator.mapping.prototype.toString = function ()
{
    var i;
    var s = "";

    for (p in this.read)
    {
        if (this.read.hasOwnProperty(p))
        {
            for(i=0; i < this.read[p].length; ++i)
            {
                s += this.read[p][i] + "\n";
            }
        }
    }

    return s;
};



