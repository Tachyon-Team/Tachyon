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
Analysis to approximate the distance (as a number of instructions) to the next 
use position of a given temp value.

@author
Erick Lavoie
*/

analysis = {};

analysis.usedist = function (cfg)
{
    // Returns the union of hm1 and hm2.
    // If an element exists in both hm1 and hm2,
    // the one with the smallest distance is kept.
    function union(hm1, hm2)
    {
        var hm = hm1.copy();

        for (var it = hm2.getItr(); it.valid(); it.next())
        {
            var key = it.get().key;
            var value = it.get().value;
            if ((!hm.hasItem(key)) || (hm.getItem(key) > value))
            {
                hm.setItem(key, value); 
            } 
        }

        return hm;
    }

    // Check that each element in hm1 is present in hm2 and 
    // has the same value 
    function equal(hm1, hm2)
    {
        for (var it = hm2.getItr(); it.valid(); it.next())
        {
            var key = it.get().key;
            var value = it.get().value;
            if ((!hm1.hasItem(key)) ||
                 (hm1.getItem(key) !== value))
            {
                return false;
            }
        }
        
        return true;
    }

    function init()
    {
        return new HashMap();
    }

    // Compute the use dist information at the beginning 
    // of the block
    function f(block, pred)
    {
        var out = block.analysis.usedist.copy();

        var phiNb = 0;
        for (var i = 0; i < block.instrs.length; ++i)
        {
            var instr = block.instrs[i];

            if (!(instr instanceof PhiInstr))
            {
                phiNb = i;
                break;
            }
        }
        var instrNb = block.instrs.length - phiNb;

        // Assume all live temps are never used in the block
        out.getKeys().forEach(function (key)
        {
            var dist = out.getItem(key);
            out.setItem(key, dist + instrNb);
        });


        // For each instruction, set temp dist to the nb of non-phi instructions 
        // from the beginning
        for (var i = block.instrs.length - 1; i >= 0; --i)
        {
            var instr = block.instrs[i];

            if (instr instanceof PhiInstr)
            {
                var use = instr.getIncoming(pred);
                if (use instanceof IRInstr)
                {
                    out.setItem(use, 0);
                }
            } else
            {
                instr.uses.forEach(function (use)
                {
                    if (use instanceof IRInstr)
                    {
                        out.setItem(use, i - phiNb);
                    }
                });
            }

            // Remove the temp created by this instruction since 
            // SSA garantees that there is a single point of definition
            if (out.hasItem(instr)) 
            {
                out.remItem(instr);
            }
        }

        return out;
    }

    // Find all blocks for which there is no successors
    var exits = [];
   
    // Initialize block information
    cfg.getBlockItr().forEach(function (block)
    {
        if (block.succs.length === 0)
        {
            exits.push(block);
        }

        if (block.analysis === undefined)
        {
            block.analysis = {};
        }

        // Initialize the distance set
        block.analysis.usedist = init();
        block.analysis.visited = false;
    });

    // Use an initial block ordering minimizing the number of 
    // iterations (reverse post-ordering of the reverse CFG, i.e. make sure 
    // successors are visited before the current node)
    var worklist = [];

    function DFS(block)
    {
        if (block.analysis.visited === true)
        {
            return;
        }

        block.analysis.visited = true;

        block.preds.forEach(function (pred)
        {
            DFS(pred);    
        });

        worklist.push(block);
    }

    exits.forEach(DFS);

    worklist.reverse();

    while (worklist.length > 0)
    {
        var b = worklist.shift();
        var totaleffect = init();
        b.succs.forEach(function (succ)
        {
            var effect = f(succ, b);
            totaleffect = union(totaleffect, effect);
        });

        if (!equal(b.analysis.usedist, totaleffect))
        {
            b.analysis.usedist = totaleffect;
            worklist.push(b);
        }
    }
};

analysis.usedistPrint = function (cfg)
{
    cfg.getBlockItr().forEach(function (block)
    {
        print(block.getBlockName());
        
        block.analysis.usedist.getKeys().forEach(function (key)
        {
            print(key.getValName() + " " + block.analysis.usedist.getItem(key));
        });

        print("");
    });
}
