/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Interprocedural type analysis implementation.

@author
Maxime Chevalier-Boisvert
*/



// TODO:
// Keep map of SSA temp->type
// - For temps that have uses only
// - At uses, if it's the only use, remove the temp from the set


// TODO:
// If calling function, look it up in hash map, update types at its entry,
// continue analysis. Need global work list for functions?
//
// If output (ret) types of function change, need to restart analysis
// for the callees!
//
// Ideally, need to have just ONE worklist. If input types at a function
// entry change, put the entry block for that function in the global work list******
//
// Need a function to queue a function for analysis (queue its entry block) *****
//
// Other function to run some analysis iterations? ****



/**
@class Type propagation interprocedural analysis
*/
function TypeProp(params)
{
    /**
    Compilation parameters this analysis is associated with
    */
    this.params = params;

    /**
    Worklist of basic blocks queued to be analyzed
    */
    this.workList = new LinkedList();

    /**
    Set of blocks in the work list
    This is to avoid adding blocks to the work list twice
    */
    this.workSet = new HashSet();

    /**
    Map of type maps at block entries
    */
    this.blockTypes = new HashMap();




    // TODO: Need to describe global object's type
    this.globalType = undefined;


    // TODO: Need to map functions to type analysis information
    this.funcTypes = undefined;




    /**
    Total analysis iteration count
    */
    this.itrCount = 0;
}

/**
Queue a block to be (re)analyzed
*/
TypeProp.prototype.queueBlock = function (block)
{
    // If the block is already queued, do nothing
    if (this.workSet.has(block) === true)
        return;

    this.workList.addLast(block);

    this.workSet.add(block);
}

/**
Queue a function to be analyzed
*/
TypeProp.prototype.queue = function (irFunc)
{
    assert (
        irFunc instanceof IRFunction,
        'expected IR function'
    );

    var entry = irFunc.hirCFG.entry;



    // TODO: handle the function's argument types



    // TODO: create a type set for the entry block
    //if (this.blockTypes.has(entry) === false)
    //Create empty type set


    

    // Queue the function's entry block
    this.queueBlock(entry);
}

/**
Run the analysis for some number of iterations
*/
TypeProp.prototype.run = function (maxItrs)
{
    // Until the max iteration count is reached
    for (var numItrs = 0; maxItrs === undefined || numItrs < maxItrs; ++numItrs)
    {
        // If the work list is empty, stop
        if (this.workList.isEmpty() === true)
            break;

        // Run one analysis iteration
        this.iterate();
    }

    // Update the total iteration count
    this.itrCount += numItrs;

    // Return the number of iterations performed
    return numItrs;
}

/**
Run one type analysis iteration
*/
TypeProp.prototype.iterate = function ()
{
    assert (
        this.workList.isEmpty() === false,
            'empty work list'
    );

    // Remove a block from the work list
    var block = this.workList.remFirst();
    this.workSet.rem(block);

    // Get the type set at the block entry
    var typeSet = this.blockTypes.get(block);


    // TODO    
    // When doing succ handling, will want to merge in out-flowing types
    // This requires creating the flow set at that point if none is available***
    /*
    assert (
        typeSet !== HashMap.NOT_FOUND,
        'type set not found'
    );
    */


    // For each instruction
    for (var i = 0; i < block.instrs.length; ++i)
    {
        var instr = block.instrs[i];


        // TODO: don't add types to type set if an instruction has no uses


        // Process the instruction
        if (instr instanceof PhiInstr)
            this.phiFunc(instr, typeSet);
        else
            this.instrFunc(instr, typeSet);


    }



    // TODO: successor handling





}

/**
Flow function applied to instruction
*/
TypeProp.prototype.instrFunc = function (instr, inSet)
{
    // TODO


    print(instr);






    // TODO: call site handling
    // TODO: return handling


}

/**
Flow function applied to phi nodes
*/
TypeProp.prototype.phiFunc = function (phi, inSet)
{
    // TODO
}

/**
Create an empty type set for initialization
*/
TypeProp.prototype.typeSetInit = function ()
{
    // TODO
}

/**
Copy a type set
*/
TypeProp.prototype.typeSetCopy = function ()
{
    // TODO
}

/**
Merge type sets at a block entry
*/
TypeProp.prototype.typeSetMerge = function ()
{
    // TODO
}

