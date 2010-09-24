/**
@fileOverview
Code related to common/redundant code elimination.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/*

- Want to identify and eliminate redundant ops
  - Including loads

- Memory
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

- Discovering two instrs congruent can possibly make their uses congruent as well
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

*/










