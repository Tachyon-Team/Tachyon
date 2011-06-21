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

Tests for linear scan register allocation.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/
(function () { // local namespace

    tests.linearScan = tests.testSuite();

    const range = allocator.range;
    const interval = allocator.interval;
    const usePos = allocator.usePos;
    const NONE = allocator.usePos.registerFlag.NONE;
    const REQUIRED = allocator.usePos.registerFlag.REQUIRED;

    // Range tests
    tests.linearScan.range = tests.testSuite();
    var t = tests.linearScan.range;

    t.basic = function ()
    {
        var r1;
        var p1, p2, p3, p4, p5;

        p1 = 0;
        p2 = 1;
        p3 = 2;
        p4 = 4;
        p5 = 5;
        
        r1 = allocator.range(1,4);

        assert(r1.covers(p1, true) === false);
        assert(r1.covers(p1, false) === false);
        assert(r1.covers(p2, true) === true);
        assert(r1.covers(p2, false) === false);
        assert(r1.covers(p3, true) === true);
        assert(r1.covers(p3, false) === true);
        assert(r1.covers(p4, true) === false);
        assert(r1.covers(p4, false) === false);
        assert(r1.covers(p5, true) === false);
        assert(r1.covers(p5, false) === false);

        assert(r1.cmp(p1, true) < 0);
        assert(r1.cmp(p1, false) < 0);
        assert(r1.cmp(p2, true) === 0);
        assert(r1.cmp(p2, false) < 0);
        assert(r1.cmp(p3, true) === 0);
        assert(r1.cmp(p3, false) === 0);
        assert(r1.cmp(p4, true) > 0);
        assert(r1.cmp(p4, false) > 0);
        assert(r1.cmp(p5, true) > 0);
        assert(r1.cmp(p5, false) > 0);
    };


    // Interval tests
    tests.linearScan.intervals = tests.testSuite();
    t = tests.linearScan.intervals;

    t.setup = function ()
    {
        t.p1 = 0;
        t.p2 = 1;
        t.p3 = 2;
        t.p4 = 4;
        t.p5 = 5;

        t.r1 = range(0,1);
        t.r2 = range(2,3);
        t.r3 = range(4,5);
        t.r4 = range(2,4);
        t.r5 = range(3,5);
        t.r6 = range(4,7);
        t.r7 = range(5,6);
        t.r8 = range(4,6);
        t.r9 = range(5,7);
        t.r10 = range(0,2);
        t.r11 = range(1,3);
        t.r12 = range(0,3);
        t.r13 = range(1,2);
        t.r14 = range(3,4);
    };

    t.updateIntervalRange = function ()
    {
        var it1 = interval([t.r2, t.r1]);

        assert(it1.startPos() === t.r1.startPos);
        assert(it1.endPos() === t.r2.endPos);

        assert(it1.covers(t.p1));
        assert(!it1.covers(t.p5));

        // Range modification tests

        it1 = interval();
        assert(it1.startPos() === null);
        assert(it1.endPos() === null);
        assert(it1.rangeNb() === 0);

        it1.addRange(10, 12);
        assert(it1.startPos() === 10);
        assert(it1.endPos() === 12);
        assert(it1.rangeNb() === 1);

        it1.addRange(7, 9);
        assert(it1.startPos() === 7);
        assert(it1.endPos() === 12);
        assert(it1.rangeNb() === 2);

        it1.addRange(7,8);
        assert(it1.startPos() === 7);
        assert(it1.endPos() === 12);
        assert(it1.rangeNb() === 2);

        it1.addRange(4,8);
        assert(it1.startPos() === 4);
        assert(it1.endPos() === 12);
        assert(it1.rangeNb() === 2);

    };

    // Case 1: Interval 2 comprised in the hole of interval 1
    //
    //   Pos    0     1     2     3     4     5
    //   it1    +----+                  +----+
    //   it2                +----+
    t.case1 = function ()
    {
        assert(interval([t.r1, t.r3]).nextIntersection(interval([t.r2])) 
                === MAX_FIXNUM);
    };
    
    // Case 2: Split in the middle of a range 
    //
    //   Pos    0     1     2
    //   it1    +----------+
    //   split        x
    t.case2 = function ()
    {
        var it1 = interval([t.r10]);
        var it2 = it1.split(1);
        assert(it1.next === it2);
        assert(it2.next === null);
        assert(it1.startPos() === 0);
        assert(it1.endPos() === 1);
        assert(it1.nextUse(0) === 0);
        assert(it1.nextUse(1) === MAX_FIXNUM);
        assert(it2.startPos() === 1);
        assert(it2.endPos() === 2);
        assert(it2.nextUse(1) === 2);
    };
   
    // Case 3: Split at the end of a range
    //
    //   Pos    0     1     2
    //   it1    +----------+
    //   split              x
    t.case3 = function ()
    {
        try
        {
            var it2 = interval([t.r10]).split(2); 
            throw "TestFailed";
        } catch (e) {if(e === "TestFailed") { throw "Split Case 1 failed"; };}
    };

    // Case 4: At the end of the first range
    //
    //   Pos    0     1     2      3      4 
    //   it1    +----+             +-----+
    //   split        x
    t.case4 = function ()
    {
        var it1 = interval([t.r1, t.r14]);
        var it2 = it1.split(1);
        assert(it1.next === it2);
        assert(it2.next === null);
        assert(it1.startPos() === 0);
        assert(it1.endPos() === 1);
        assert(it1.ranges.length === 1);
        assert(it2.startPos() === 3);
        assert(it2.endPos() === 4);
    };

    // Case 5: Split in a hole 
    //
    //   Pos    0     1     2      3      4 
    //   it1    +----+             +-----+
    //   split              x
    t.case5 = function ()
    {
        var it1 = interval([t.r1, t.r14]);
        var it2 = it1.split(2);
        assert(it1.next === it2);
        assert(it2.next === null);
        assert(it1.startPos() === 0);
        assert(it1.endPos() === 1);
        assert(it1.nextUse(0) === 0);
        assert(it1.nextUse(1) === 1);
        assert(it2.startPos() === 3);
        assert(it2.endPos() === 4);
        assert(it2.nextUse(3) === 3);
        assert(it2.nextUse(4) === 4);
    };

    // Case 6: Split at the beginning of the second range
    //
    //   Pos    0     1     2      3      4
    //   it1    +----+             +-----+
    //   split                     x
    t.case6 = function ()
    {
        var it1 = interval([t.r1, t.r14]);
        var it2 = it1.split(3);
        assert(it1.next === it2);
        assert(it2.next === null);
        assert(it1.startPos() === 0);
        assert(it1.endPos() === 1);
        assert(it1.nextUse(0) === 0);
        assert(it1.nextUse(1) === 1);
        assert(it1.nextUse(2) === MAX_FIXNUM);
        assert(it2.startPos() === 3);
        assert(it2.endPos() === 4);
        assert(it2.nextUse(3) === 3);
        assert(it2.nextUse(4) === 4);
    };

    // Case 7: Split in the middle of the second range
    //
    //   Pos    0     1     2      3      4     5
    //   it1    +----+             +-----------+
    //   split                            x
    t.case7 = function ()
    {
        var it1 = interval([t.r1, t.r5]);
        var it2 = it1.split(4);
        assert(it1.next === it2);
        assert(it2.next === null);
        assert(it1.startPos() === 0);
        assert(it1.endPos() === 4);
        assert(it1.nextUse(0) === 0);
        assert(it1.nextUse(1) === 1);
        assert(it1.nextUse(2) === 3);
        assert(it1.nextUse(4) === MAX_FIXNUM);
        assert(it1.ranges.length === 2);
        assert(it2.startPos() === 4);
        assert(it2.endPos() === 5);
        assert(it2.nextUse(3) === 5);
    };


    // Interval tests
    tests.linearScan.allocation = tests.testSuite();
    t = tests.linearScan.allocation;
    // Linear Scan Register Allocation Test

    t.setup = function ()
    {
        t.mems = { slots:[], 
                   newSlot:function () 
                           { 
                              var s = "slot" + this.slots.length; 
                              this.slots.push(s);
                              return s;
                           } 
               }; 
        t.params = { target: { backendCfg: { physReg:["REG1", "REG2"]} }};
    };
   
    //   Case 1: Normal allocation, two physical registers
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +----+                    +----+ 
    //   it3          +------------------+
    t.normal = function ()
    {
        var it1 = interval([range(0,5)], [usePos(0, NONE),
                                      usePos(2, NONE),
                                      usePos(5, NONE)]);  

        var it2 = interval([range(0,1), range(4,5)]);
        var it3 = interval([range(1,4)]);

        allocator.linearScan(t.params, [it1, it2, it3], t.mems);

        assert(it1.reg !== null);
        assert(it2.reg !== null);
        assert(it3.reg !== null);
        assert(it1.reg !== it2.reg);
        assert(it2.reg === it3.reg);
    };

    //   Case 2: 2 intervals with the same range and
    //           use position, only a register available
    //   Pos    0     1     2  
    //   it1    +-----------+
    //   it2    +-----------+
    t.intervalTower = function ()
    {
        var it1 = interval([range(0,2)], [usePos(0, NONE),
                                      usePos(2, NONE)]);  

        var it2 = interval([range(0,2)]);

        var params = {target:{backendCfg:{physReg:["REG1"]}}};
        allocator.linearScan(params, [it1, it2], t.mems);

        assert(it1.reg !== null);
        assert(it2.reg !== null);
        assert((it1.reg === "REG1" && it2.reg === "slot0") ||
                    (it1.reg === "slot0" && it2.reg === "REG1"));
    };



    //   Case 3: it3 should be split and the last part spilled
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +----+                    +----+ 
    //   it3          +------------------------+
    t.partialOverlapping = function ()
    {
        var it1 = interval([range(0,5)], [usePos(0, NONE),
                                      usePos(2, NONE),
                                      usePos(5, NONE)]);  

        var it2 = interval([range(0,1), range(4,5)]);
        var it3 = interval([range(1,5)]);

        allocator.linearScan(t.params, [it1, it2, it3], t.mems);

        assert(it1.reg !== null);
        assert(it2.reg !== null);
        assert(it3.reg !== null);
        assert(it1.reg !== it2.reg);
        assert(it2.reg === it3.reg);
        assert(it3.next !== null);
        assert(it3.next.reg === "slot0");
    };

    //   Case 4: it2 should be spilled
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +------------------*-----------+ 
    //   it3          +------------------------+
    t.overlappingCurrentUsedBefore = function ()
    {
        var it1 = interval([range(0,5)], [usePos(0, NONE),
                                      usePos(2, NONE),
                                      usePos(5, NONE)]);  

        var it2 = interval([range(0,5)], [usePos(0, NONE),
                                      usePos(3, NONE),
                                      usePos(5, NONE)]);  
        var it3 = interval([range(1,5)]);

        allocator.linearScan(t.params, [it1, it2, it3], t.mems);

        assert(it1.reg !== null);
        assert(it2.reg !== null);
        assert(it3.reg !== null);
        assert(it1.reg !== it2.reg);
        assert(it2.reg === it3.reg);
        assert(it2.next !== null);
        assert(it2.next.reg === "slot0");
    }; 
    
    //   Case 5: it3 should be spilled
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +------------------*-----------+ 
    //   it3          --------------------*----+
    t.overlappingCurrentUsedAfter = function ()
    {
        var it1 = interval([range(0,5)], [usePos(0, NONE),
                                      usePos(2, NONE),
                                      usePos(5, NONE)]);  

        var it2 = interval([range(0,5)], [usePos(0, NONE),
                                      usePos(3, NONE),
                                      usePos(5, NONE)]);  
        var it3 = interval([range(1,5)], [usePos(4, NONE),
                                      usePos(5, NONE)]);  

        allocator.linearScan(t.params, [it1, it2, it3], t.mems);

        assert(it1.reg !== null);
        assert(it2.reg !== null);
        assert(it3.reg !== null);
        assert(it1.reg !== it2.reg);
        assert(it2.reg !== it3.reg);
        assert(it3.reg === "slot0");
    };

    //  Case 6: it3 spilled to multiple memory slots...
    //   Pos    0     1     2      3      4      5      6
    //   it1    +-----------*--------------------*-----+
    //   it2    +----+                           +-----+ 
    //   it3          +-------------------*------------+
    //   it4                +--------------------*-----+
    t.possiblymultipleslotspill = function ()
    {

        var it1 = interval([range(0,6)], [usePos(0, NONE),
                                      usePos(2, NONE),
                                      usePos(5, NONE),
                                      usePos(6, NONE)]);  

        var it2 = interval([range(0,1), range(5,6)], 
                           [usePos(0, NONE),
                            usePos(1, NONE),
                            usePos(6, NONE)]);  
        var it3 = interval([range(1,6)], 
                           [usePos(1, NONE),
                            usePos(4, NONE),
                            usePos(6, NONE)]);  
        var it4 = interval([range(2,6)], 
                           [usePos(2, NONE),
                            usePos(5, NONE),
                            usePos(6, NONE)]);  

        allocator.linearScan(t.params, [it1, it2, it3, it4], t.mems);

        assert(it1.reg === "REG1" || it1.reg === "REG2");
        assert(it2.reg === "REG1" || it2.reg === "REG2");
        assert(it1.reg !== it2.reg);
        assert(it3.reg === it2.reg);
        assert(it3.next.reg === "slot0");
        assert(it3.next.next.reg === "slot0");
        assert(it2.next.reg === "slot1");
        assert(it4.reg === it3.reg);

        assert(it1.next === null);
        assert(it2.next.next === null);
        assert(it3.next.next.next === null);
        assert(it4.next === null);
    };


    // Mappings tests
    t.mappingBasic = function ()
    {
        var map = allocator.mapping();
        var moves = [];
        var temp = "slot0";

        function insertMove(move)
        {
            moves.push(move);
        };

        map.add("A", "B");
        map.add("B", "A");

        map.add("A", "C");
        map.add("C", "D");

        map.orderAndInsertMoves(insertMove, temp);

        // NOTE: this test cannot fail, but it is kept as an 
        // example
        //print(moves);

    };

})(); // end of local namespace
