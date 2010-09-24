/**
@fileOverview
Code related to common/redundant code elimination.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/*

Want to identify and eliminate redundant ops:
- Including loads
- Including get_ctx/set_ctx

Memory:
- Ops that write to memory kill ops that read from memory
- Ops that write to memory (and funcs that write) may not be combined
- In the case of get_ctx
  - Only set_ctx can kill this
- In the case of load/store
  - Non-overlapping store to same ptr doesn't kill... Ignore for now
  - Assume all writes kill reads
- Assume writes are never equivalent?
  - Writes kill all other writes
  - Unless exact same store... Ignore for now

Discovering two instrs congruent can possibly make their uses congruent as well
  - Should add their uses to work list?
  - Could handle this with congruence classes
    - Test if operands are same based on cong. classes/val numbers

Optimizing:
- Need work list on CFG to compute avail exprs
- Can only replace/simplify if we have must-reach
  - Can compute must reach out/in as analyzing
  - Can only replace use of a by b if b must reach
- If same expr on both side of branches, would like to move to end of pred
  - Can't if op reads and intervening writes
  - Can't if op writes
  - Can't if op has branches

Two questions:
- Which instrs computing the same thing?
- Which instrs must reach me

If ops computing same thing along all sides of branch, may want to try moving
them up to first common predecessor. This would reduce the amount of code.
- Try to move up instrs along all branches to end of pred
- Complicated if op has branches!
  - Can't really move to end of pred
- Basic case: op is in both immediate successors
- Must-be-computed relationship?
  - If equivalent op in all immediate successors, try to pull to pred?
  - Can merge on reach out set of succs?
  - Must add pred, succs to work list

Attempt #1

Algorithm:
- Uses CFG work list
- Traverse basic blocks, identify common exprs based on must-reach and equivalence
  - Simple must reach of exprs
  - For current op in current bb, does an equivalent op reach?
  - Does an op with the same val number reach?
  - Could have both must reach and cur reach hashes
- If expr must reach you, can immediately replace redundant exprs in current block
  - We know it must reach us at this point
  - Changes reach out set, must push succs on work list?
  - Its our dests that change
  - Info will trickle down to dests, we're no longer reaching them
  - Dests have new uses, will define new equiv classes
- If redundant exprs exist in succ, can try pulling up
  - Among ops defined *by succs*, are any the same in all succs
  - Set of congruence classes defined by all succs... Intersection

Need hash map of congruence classes to earliest instrs defining them
- Could possibly have global value number set
- Func takes instr as input, produces val number, manages equivalence classes
- Want to avoid recomputing val number, can map instrs by id to val nums

PROBLEM: not true that must reach at one point means must
reach before fixed point is over... We're using intersection here... GFP, not LFP

Attempt #2

1. Compute dominators using GFP first?
Essentially, must reach *blocks*
- Then, given an instr, can tell if it must reach from its parent block

2. Compute initial set of congruence classes
- Array mapping GVNs to sets of instrs

3. Can then use SSA work list
- Add all nodes to list that are in a class > 1

4. Take instr i out of work list
- Compute its current congruence class
- For all instrs in class, if any dominates, replace i by dominator
- Remove instr from its class
- Update all dests

PROBLEM: does not account for intervening kills of available expressions...

Attempt #3

Can only perform replacements once must reach instrs has stabilized
- Need to have inner loop operating on CFG
- Want to compute must reach at each instruction...
- Only care about the instrs in the same equivalence classes that reach

Possibly, we can compute value numbers for dests that are the same if they
use operands with the same equivalence classes.

ex:

c = a + b
d = a + b

e = c + 1
f = d + 1

e and f can get same equivalence class

Could avoid ever recomputing this information. Do fixed-point once only.


Can then add instrs to work list that have more than one in equiv class.


*/

/**
Perform common/redundant code elimination on a CFG
*/
function commElim(cfg)
{
    // Value number array, indexed by instruction id
    var valNos = [];

    // Function to get a value number for an instruction
    function getValNo(instr)
    {








    }

    // Sets of values reaching the exit basic blocks, indexed by instruction id
    var mustReachOut = [];

    // Work list of CFG blocks to examine
    var workList = [cfg.entry];

    // Until the work list is empty
    while (workList.length != 0)
    {
        var block = workList.pop();



        // TODO

    }








}

