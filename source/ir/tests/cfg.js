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
        var func = new IRFunction('foobar', ['foo', 'bar', 'bif'], 
                                   'foo\nbar\nbif');

        this.cfg = new ControlFlowGraph(func);

        var entry = this.cfg.getEntryBlock();
        var l1 = this.cfg.getNewBlock('left');
        var l2 = this.cfg.getNewBlock('left');
        var r1 = this.cfg.getNewBlock('right');
        var merge = this.cfg.getNewBlock('merge');

        this.blocks = [entry, l1, l2, r1, merge];

        this.instrs = [];
        var ins;
    
        ins = new AddInstr(ConstValue.getConst(1), ConstValue.getConst(2));
        this.instrs.push(ins);
        entry.addInstr(ins);

        ins = new IfInstr(ConstValue.getConst(true), l1, r1);
        this.instrs.push(ins);
        entry.addInstr(ins);

        ins = new AddInstr(ConstValue.getConst(1), ConstValue.getConst(2));
        this.instrs.push(ins);
        l1.addInstr(ins, 'eee');

        ins = new GetPropValInstr(this.cfg.getThisArg(), 
                                  ConstValue.getConst(2));
        this.instrs.push(ins);
        l1.addInstr(ins);

        ins = new JumpInstr(l2);
        this.instrs.push(ins);
        l1.addInstr(ins);

        ins = new PhiInstr([l1.instrs[1]], [l1]);
        this.instrs.push(ins);
        l2.addInstr(ins);

        ins = new ModInstr(l1.instrs[1], ConstValue.getConst(7));
        this.instrs.push(ins);
        l2.addInstr(ins);
        
        ins = new AddInstr(ConstValue.getConst(3), 
                                 ConstValue.getConst(4));
        this.instrs.push(ins);
        l2.addInstr(ins);

        ins = new SubInstr(ConstValue.getConst(3), 
                                 ConstValue.getConst(4));
        this.instrs.push(ins);
        l2.addInstr(ins);

        ins = new JumpInstr(merge);
        this.instrs.push(ins);
        l2.addInstr(ins);

        ins = new MulInstr(ConstValue.getConst(7), ConstValue.getConst(8));
        this.instrs.push(ins);
        r1.addInstr(ins, 'eee');

        ins = new JumpInstr(merge);
        this.instrs.push(ins);
        r1.addInstr(ins);

        ins = new PhiInstr([l1.instrs[0], r1.instrs[0]], [l2, r1]);
        this.instrs.push(ins);
        merge.addInstr(ins);

        ins = new PutPropValInstr(entry.instrs[0], 
                           ConstValue.getConst('foo'), ConstValue.getConst(2));
        this.instrs.push(ins);
        merge.addInstr(ins);

        ins = new LogNotInstr(merge.instrs[0]);
        this.instrs.push(ins);
        merge.addInstr(ins);

        ins = new LogNotInstr(merge.instrs[2]);
        this.instrs.push(ins);
        merge.addInstr(ins);

        ins = new RetInstr(ConstValue.getConst(undefined));
        this.instrs.push(ins);
        merge.addInstr(ins);

        ins = new LsftInstr(ConstValue.getConst(1), 
                            ConstValue.getConst(2));
        this.instrs.push(ins);
        merge.addInstr(ins, 'foo', 1);

        this.cfg.validate();
        
        this.edges = [ {pred:entry, succ:l1},
                       {pred:entry, succ:r1},
                       {pred:l1, succ:l2},
                       {pred:l2, succ:merge},
                       {pred:r1, succ:merge} ];
    };

    t.blockIterator = function ()
    {
        for (var it = this.cfg.getBlockIterator(); !it.end(); it.next())
        {
            arraySetRem(this.blocks, it.get());
        }

        assert(this.blocks.length === 0);
    };
    
    t.instructionIterator = function ()
    {
        for (var it = this.cfg.getInstrIterator(); !it.end(); it.next())
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
        for (var it = this.cfg.getEdgeIterator(); !it.end(); it.next())
        {
            remove(this.edges, it.get());
            count++;
        }
        assert(this.edges.length === 0 && count === edgeNumber);
    };

})(); // end of local namespace
