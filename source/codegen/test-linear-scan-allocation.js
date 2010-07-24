
/**
@fileOverview

Serie of tests for the linear scan register allocator.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved
*/

var test = test || {};

test.assert = function (bool, message) 
{
    message = message || "AssertFailed";
    if(!bool) { throw message; };
};

// Set test
(function () {


})();

// Priority Queue test
(function () {


})();

// Range test
(function () {
    var r1;
    var p1, p2, p3, p4, p5;

    p1 = 0;
    p2 = 1;
    p3 = 2;
    p4 = 4;
    p5 = 5;
    
    r1 = allocator.range(1,4);

    test.assert(r1.covers(p1, true) === false);
    test.assert(r1.covers(p1, false) === false);
    test.assert(r1.covers(p2, true) === true);
    test.assert(r1.covers(p2, false) === false);
    test.assert(r1.covers(p3, true) === true);
    test.assert(r1.covers(p3, false) === true);
    test.assert(r1.covers(p4, true) === false);
    test.assert(r1.covers(p4, false) === false);
    test.assert(r1.covers(p5, true) === false);
    test.assert(r1.covers(p5, false) === false);

    test.assert(r1.cmp(p1, true) < 0);
    test.assert(r1.cmp(p1, false) < 0);
    test.assert(r1.cmp(p2, true) === 0);
    test.assert(r1.cmp(p2, false) < 0);
    test.assert(r1.cmp(p3, true) === 0);
    test.assert(r1.cmp(p3, false) === 0);
    test.assert(r1.cmp(p4, true) > 0);
    test.assert(r1.cmp(p4, false) > 0);
    test.assert(r1.cmp(p5, true) > 0);
    test.assert(r1.cmp(p5, false) > 0);

})();

// Use position test
(function () {


})();

// Interval test
(function () {
    var it1, it2, it3;
    var r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, r14;
    var p1, p2, p3, p4, p5;
    var pregs;
    var mems;

    const range = allocator.range;
    const interval = allocator.interval;
    const usePos = allocator.usePos;
    const NONE = allocator.usePos.registerFlag.NONE;
    const REQUIRED = allocator.usePos.registerFlag.REQUIRED;

    p1 = 0;
    p2 = 1;
    p3 = 2;
    p4 = 4;
    p5 = 5;

    r1 = range(0,1);
    r2 = range(2,3);
    r3 = range(4,5);
    r4 = range(2,4);
    r5 = range(3,5);
    r6 = range(4,7);
    r7 = range(5,6);
    r8 = range(4,6);
    r9 = range(5,7);
    r10 = range(0,2);
    r11 = range(1,3);
    r12 = range(0,3);
    r13 = range(1,2);
    r14 = range(3,4);

    it1 = interval([r2, r1]);

    test.assert(it1.startPos() === r1.startPos);
    test.assert(it1.endPos() === r2.endPos);

    test.assert(it1.covers(p1));
    test.assert(!it1.covers(p5));

    // Range modification tests

    it1 = interval();
    test.assert(it1.startPos() === null);
    test.assert(it1.endPos() === null);
    test.assert(it1.rangeNb() === 0);

    it1.addRange(10, 12);
    test.assert(it1.startPos() === 10);
    test.assert(it1.endPos() === 12);
    test.assert(it1.rangeNb() === 1);

    it1.addRange(7, 9);
    test.assert(it1.startPos() === 7);
    test.assert(it1.endPos() === 12);
    test.assert(it1.rangeNb() === 2);

    it1.addRange(7,8);
    test.assert(it1.startPos() === 7);
    test.assert(it1.endPos() === 12);
    test.assert(it1.rangeNb() === 2);

    it1.addRange(4,8);
    test.assert(it1.startPos() === 4);
    test.assert(it1.endPos() === 12);
    test.assert(it1.rangeNb() === 2);

    // Intersection tests

    // Case 1: Interval 2 comprised in the hole of interval 1
    //
    //   Pos    0     1     2     3     4     5
    //   it1    +----+                  +----+
    //   it2                +----+
    test.assert(interval([r1, r3]).nextIntersection(interval([r2])) 
                === Infinity, "Case 1 failed");

    // Case 2: Interval 2 start in the hole of interval 1 but 
    //         intersect the second part
    //
    //   Pos    0     1     2     3     4     5
    //   it1    +----+            +----------+
    //   it2                +----------+
    test.assert(interval([r1, r5]).nextIntersection(interval([r4])) 
                === 3, "Case 2 failed");

    // Case 3: Interval 2 has 1 range in hole of interval 1 and 
    //         a second range comprised in the second range of
    //         interval 1 
    //
    //   Pos    0     1     2     3     4     5     6     7
    //   it1    +----+                  +----------------+
    //   it2                +----+            +----+
    test.assert(interval([r1, r6]).nextIntersection(interval([r2, r7])) 
                === 5, "Case 3 failed");

    
    // Case 4: Interval 2 has 1 range in hole of interval 1 and 
    //         a second range starting in the second range of
    //         interval 1 but ending after it 
    //
    //   Pos    0     1     2     3     4     5     6     7
    //   it1    +----+                  +----------+
    //   it2                +----+            +----------+
    test.assert(interval([r1, r8]).nextIntersection(interval([r2, r9])) 
                === 5, "Case 4 failed");

    
    // Case 5: Interval 2 starting in interval 1 but ending after it
    //
    //   Pos    0     1     2     3 
    //   it1    +----------+                 
    //   it2          +----------+          
    test.assert(interval([r10]).nextIntersection(interval([r11])) 
                === 1, "Case 5 failed");
    
    // Case 6: Interval 2 comprised in interval 1
    //
    //   Pos    0     1     2     3 
    //   it1    +----------------+                 
    //   it2          +----+          
    test.assert(interval([r12]).nextIntersection(interval([r13])) 
                === 1, "Case 6 failed");

    // Case 7: Interval 2 starts at end of interval 1
    //
    //   Pos    0     1     2
    //   it1    +----+                 
    //   it2          +----+          
    test.assert(interval([r1]).nextIntersection(interval([r13])) 
                 === Infinity, "Case 7 failed");

    // Case 8: Interval 2 starts and ends during hole in
    //         interval 1
    //
    //   Pos    0     1     2     3
    //   it1    +----+      +----+            
    //   it2          +----+          
    test.assert(interval([r1, r2]).nextIntersection(interval([r13])) 
                === Infinity, "Case 8 failed");


    // Case 9: Interval 2 and 1 exactly covers the same range
    //
    //   Pos    0     1
    //   it1    +----+
    //   it2    +----+          
    test.assert(interval([r1]).nextIntersection(interval([r1])) 
                === 0, "Case 9 failed");

    // Case 10: Interval 2 and 1 starts at the same place but do 
    //          not cover the same range
    //
    //   Pos    0     1     2
    //   it1    +----------+
    //   it2    +----+          
    test.assert(interval([r10]).nextIntersection(interval([r1])) 
                === 0, "Case 10 failed");

    // Next Use test
    
     
    //   Pos    0     1     2      3      4     5
    //   it1    +-------------------------------+
    //   use    *           *                   *
    it1 = interval([range(0,5)], [usePos(0, NONE),
                                  usePos(2, REQUIRED),
                                  usePos(5, NONE)]); 


    test.assert(it1.nextUse(0) === 0);
    test.assert(it1.nextUse(0, REQUIRED) === 2);
    test.assert(it1.nextUse(1) === 2);
    test.assert(it1.nextUse(3) === 5);


    // Split Before test

    // Case 1: Split at the beginning of the first range 
    //
    //   Pos    0     1     2
    //   it1    +----------+
    //   split  x
    try
    {
        it2 = interval([r10]).split(0); 
        throw "TestFailed";
    } catch (e) { if(e === "TestFailed") { throw "Split Case 1 failed"; };}

    // Case 2: Split in the middle of a range 
    //
    //   Pos    0     1     2
    //   it1    +----------+
    //   split        x
    it1 = interval([r10]);
    it2 = it1.split(1);
    test.assert(it1.next === it2);
    test.assert(it2.next === null);
    test.assert(it1.startPos() === 0);
    test.assert(it1.endPos() === 1);
    test.assert(it1.nextUse(0) === 0);
    test.assert(it1.nextUse(1) === Infinity);
    test.assert(it2.startPos() === 1);
    test.assert(it2.endPos() === 2);
    test.assert(it2.nextUse(1) === 2);
    
    // Case 3: Split at the end of a range
    //
    //   Pos    0     1     2
    //   it1    +----------+
    //   split              x
    try
    {
        it2 = interval([r10]).split(2); 
        throw "TestFailed";
    } catch (e) {if(e === "TestFailed") { throw "Split Case 1 failed"; };}

    // Case 4: At the end of the first range
    //
    //   Pos    0     1     2      3      4 
    //   it1    +----+             +-----+
    //   split        x
    it1 = interval([r1, r14]);
    it2 = it1.split(1);
    test.assert(it1.next === it2);
    test.assert(it2.next === null);
    test.assert(it1.startPos() === 0);
    test.assert(it1.endPos() === 1);
    test.assert(it1.ranges.length === 1);
    test.assert(it2.startPos() === 3);
    test.assert(it2.endPos() === 4);

    // Case 5: Split in a hole 
    //
    //   Pos    0     1     2      3      4 
    //   it1    +----+             +-----+
    //   split              x
    it1 = interval([r1, r14]);
    it2 = it1.split(2);
    test.assert(it1.next === it2);
    test.assert(it2.next === null);
    test.assert(it1.startPos() === 0);
    test.assert(it1.endPos() === 1);
    test.assert(it1.nextUse(0) === 0);
    test.assert(it1.nextUse(1) === 1);
    test.assert(it2.startPos() === 3);
    test.assert(it2.endPos() === 4);
    test.assert(it2.nextUse(3) === 3);
    test.assert(it2.nextUse(4) === 4);

    // Case 6: Split at the beginning of the second range
    //
    //   Pos    0     1     2      3      4
    //   it1    +----+             +-----+
    //   split                     x
    it1 = interval([r1, r14]);
    it2 = it1.split(3);
    test.assert(it1.next === it2);
    test.assert(it2.next === null);
    test.assert(it1.startPos() === 0);
    test.assert(it1.endPos() === 1);
    test.assert(it1.nextUse(0) === 0);
    test.assert(it1.nextUse(1) === 1);
    test.assert(it1.nextUse(2) === Infinity);
    test.assert(it2.startPos() === 3);
    test.assert(it2.endPos() === 4);
    test.assert(it2.nextUse(3) === 3);
    test.assert(it2.nextUse(4) === 4);

    // Case 7: Split in the middle of the second range
    //
    //   Pos    0     1     2      3      4     5
    //   it1    +----+             +-----------+
    //   split                            x
    it1 = interval([r1, r5]);
    it2 = it1.split(4);
    test.assert(it1.next === it2);
    test.assert(it2.next === null);
    test.assert(it1.startPos() === 0);
    test.assert(it1.endPos() === 4);
    test.assert(it1.nextUse(0) === 0);
    test.assert(it1.nextUse(1) === 1);
    test.assert(it1.nextUse(2) === 3);
    test.assert(it1.nextUse(4) === Infinity);
    test.assert(it1.ranges.length === 2);
    test.assert(it2.startPos() === 4);
    test.assert(it2.endPos() === 5);
    test.assert(it2.nextUse(3) === 5);



    // Linear Scan Register Allocation Test
   
    //   Case 1: Normal allocation, two physical registers
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +----+                    +----+ 
    //   it3          +------------------+
    it1 = interval([range(0,5)], [usePos(0, NONE),
                                  usePos(2, NONE),
                                  usePos(5, NONE)]);  

    it2 = interval([range(0,1), range(4,5)]);
    it3 = interval([range(1,4)]);

    pregs = ["REG1", "REG2"];
    mems = []; 
    allocator.linearScan(pregs, [it1, it2, it3], mems);

    test.assert(it1.reg !== null);
    test.assert(it2.reg !== null);
    test.assert(it3.reg !== null);
    test.assert(it1.reg !== it2.reg);
    test.assert(it2.reg === it3.reg);

    //   Case 2: 2 intervals with the same range and
    //           use position, only a register available
    //   Pos    0     1     2  
    //   it1    +-----------+
    //   it2    +-----------+
    it1 = interval([range(0,2)], [usePos(0, NONE),
                                  usePos(2, NONE)]);  

    it2 = interval([range(0,2)]);

    pregs = ["REG1"];
    mems = { slots:[], 
             newSlot:function () 
                     { 
                        var s = "slot" + this.slots.length; 
                        this.slots.push(s);
                        return s;
                     } 
           }; 
    allocator.linearScan(pregs, [it1, it2], mems);

    test.assert(it1.reg !== null);
    test.assert(it2.reg !== null);
    test.assert((it1.reg === "REG1" && it2.reg === "slot0") ||
                (it1.reg === "slot0" && it2.reg === "REG1"));



    //   Case 3: it3 should be split and the last part spilled
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +----+                    +----+ 
    //   it3          +------------------------+
    it1 = interval([range(0,5)], [usePos(0, NONE),
                                  usePos(2, NONE),
                                  usePos(5, NONE)]);  

    it2 = interval([range(0,1), range(4,5)]);
    it3 = interval([range(1,5)]);

    pregs = ["REG1", "REG2"];
    mems = { slots:[], 
             newSlot:function () 
                     { 
                        var s = "slot" + this.slots.length; 
                        this.slots.push(s);
                        return s;
                     } 
           }; 
    allocator.linearScan(pregs, [it1, it2, it3], mems);

    test.assert(it1.reg !== null);
    test.assert(it2.reg !== null);
    test.assert(it3.reg !== null);
    test.assert(it1.reg !== it2.reg);
    test.assert(it2.reg === it3.reg);
    test.assert(it3.next !== null);
    test.assert(it3.next.reg === "slot0");

    //   Case 4: it2 should be spilled
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +------------------*-----------+ 
    //   it3          +------------------------+
    it1 = interval([range(0,5)], [usePos(0, NONE),
                                  usePos(2, NONE),
                                  usePos(5, NONE)]);  

    it2 = interval([range(0,5)], [usePos(0, NONE),
                                  usePos(3, NONE),
                                  usePos(5, NONE)]);  
    it3 = interval([range(1,5)]);

    pregs = ["REG1", "REG2"];
    mems = { slots:[], 
             newSlot:function () 
                     { 
                        var s = "slot" + this.slots.length; 
                        this.slots.push(s);
                        return s;
                     } 
           }; 
    allocator.linearScan(pregs, [it1, it2, it3], mems);

    test.assert(it1.reg !== null);
    test.assert(it2.reg !== null);
    test.assert(it3.reg !== null);
    test.assert(it1.reg !== it2.reg);
    test.assert(it2.reg === it3.reg);
    test.assert(it2.next !== null);
    test.assert(it2.next.reg === "slot0");
    
    
    //   Case 5: it3 should be spilled
    //   Pos    0     1     2      3      4     5
    //   it1    +-----------*------------------+
    //   it2    +------------------*-----------+ 
    //   it3          --------------------*----+
    it1 = interval([range(0,5)], [usePos(0, NONE),
                                  usePos(2, NONE),
                                  usePos(5, NONE)]);  

    it2 = interval([range(0,5)], [usePos(0, NONE),
                                  usePos(3, NONE),
                                  usePos(5, NONE)]);  
    it3 = interval([range(1,5)], [usePos(4, NONE),
                                  usePos(5, NONE)]);  

    pregs = ["REG1", "REG2"];
    mems = { slots:[], 
             newSlot:function () 
                     { 
                        var s = "slot" + this.slots.length; 
                        this.slots.push(s);
                        return s;
                     } 
           }; 
    allocator.linearScan(pregs, [it1, it2, it3], mems);

    test.assert(it1.reg !== null);
    test.assert(it2.reg !== null);
    test.assert(it3.reg !== null);
    test.assert(it1.reg !== it2.reg);
    test.assert(it2.reg !== it3.reg);
    test.assert(it3.reg === "slot0");

})();

// Block Ordering Test
(function () {

})();

// Live intervals Test
(function () {

})();

// Full Register Allocation
(function () {

})();
