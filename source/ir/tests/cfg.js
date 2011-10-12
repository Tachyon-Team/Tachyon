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

Tests for cfg representation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

(function () { // local namespace
    tests.cfg = tests.testSuite();

    var t = tests.cfg;

    t.setup = function ()
    {
        var func = new IRFunction(
            'foobar', 
            ['foo', 'bar', 'bif'], 
            'foo\nbar\nbif'
        );

        this.cfg = new ControlFlowGraph(func);

        var entry = this.cfg.getEntryBlock();
        var l1 = this.cfg.getNewBlock('left');
        var l2 = this.cfg.getNewBlock('left');
        var r1 = this.cfg.getNewBlock('right');
        var merge = this.cfg.getNewBlock('merge');

        this.blocks = [entry, l1, l2, r1, merge];

        var instrs = [];
        this.instrs = instrs;
            
        function addInstr(instr, block, outName, index)
        {
            instrs.push(instr);
            block.addInstr(instr, outName, index);
        }

        // Entry block
        addInstr(
            new AddInstr(
                IRConst.getConst(1),
                IRConst.getConst(2)
            ),
            entry
        );
        addInstr(
            new IfInstr(
                [IRConst.getConst(true), IRConst.getConst(true)],
                'EQ', 
                l1, 
                r1
            ),
            entry
        );

        // L1 block
        addInstr(
            new AddInstr(
                IRConst.getConst(1), 
                IRConst.getConst(2)
            ),
            l1
        );
        addInstr(
            new SubInstr(
                IRConst.getConst(-1), 
                IRConst.getConst(2)
            ),
            l1
        );
        addInstr(
            new JumpInstr(l2),
            l1
        );

        // L2 block
        addInstr(
            new PhiInstr(
                [l1.instrs[1]],
                [l1]
            ),
            l2
        );
        addInstr(
            new ModInstr(
                l1.instrs[1],
                IRConst.getConst(7)
            ),
            l2
        );
        addInstr(
            new AddInstr(
                IRConst.getConst(3),
                IRConst.getConst(4)
            ),
            l2
        );
        addInstr(
            new SubInstr(
                IRConst.getConst(3),
                IRConst.getConst(4)
            ),
            l2
        );
        addInstr(
            new JumpInstr(merge),
            l2
        );

        // R1 block
        addInstr(
            new MulInstr(
                IRConst.getConst(7, IRType.pint),
                IRConst.getConst(8, IRType.pint)
            ),
            r1
        );
        addInstr(
            new ICastInstr(
                IRType.box,
                r1.instrs[0]
            ),
            r1
        );
        addInstr(
            new JumpInstr(merge),
            r1
        );

        // Merge block
        addInstr(
            new PhiInstr(
                [l1.instrs[0], r1.instrs[1]],
                [l2, r1]
            ),
            merge
        );
        addInstr(
            new NotInstr(merge.instrs[0]),
            merge
        );
        addInstr(
            new NotInstr(merge.instrs[1]),
            merge
        );
        addInstr(
            new RetInstr(IRConst.getConst(undefined)),
            merge
        );
        addInstr(
            new LsftInstr(
                IRConst.getConst(1, IRType.pint), 
                IRConst.getConst(2, IRType.pint)
            ),
            merge,
            'foo',
            1
        );

        this.cfg.validate();
        
        this.edges = [ {pred:entry, succ:l1},
                       {pred:entry, succ:r1},
                       {pred:l1, succ:l2},
                       {pred:l2, succ:merge},
                       {pred:r1, succ:merge} ];
    };

    t.blockIterator = function ()
    {
        for (var it = this.cfg.getBlockItr(); it.valid(); it.next())
        {
            arraySetRem(this.blocks, it.get());
        }

        assert(this.blocks.length === 0);
    };
    
    t.instructionIterator = function ()
    {
        for (var it = this.cfg.getInstrItr(); it.valid(); it.next())
        {
            arraySetRem(this.instrs, it.get());
        }

        assert(this.instrs.length === 0);
    };

    t.copy = function ()
    {
        this.cfg.copy().validate();
    };

    t.edgeIterator = function ()
    {
        function edgeEqual(e1, e2)
        {
            return e1.pred === e2.pred && e1.succ === e2.succ;
        }
        function remove(a, value)
        {
            var i;
            for (i=0; i < a.length; ++i)
            {
                if (edgeEqual(a[i], value))
                {
                    a.splice(i,1);   
                    return;
                }
            }
        }

        var edgeNumber = this.edges.length;
        var count = 0;
        for (var it = this.cfg.getEdgeItr(); it.valid(); it.next())
        {
            remove(this.edges, it.get());
            count++;
        }
        assert(this.edges.length === 0 && count === edgeNumber);
    };

    t.insertOnEdge = function ()
    {
        var edgeItr = this.cfg.getEdgeItr();

        while (true)
        {
            var edge = edgeItr.get();

            if (
                edge.pred === this.edges[0].pred &&
                edge.succ === this.edges[0].succ
            )
                break;

            edgeItr.next();
        }
        
        var block = this.cfg.getNewBlock('ins_block');

        block.addInstr(
            new ModInstr(
                IRConst.getConst(1),
                IRConst.getConst(2)
            )
        );
        
        this.cfg.insertOnEdge(edgeItr, block);

        this.cfg.validate();
    };

})(); // end of local namespace
