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
Unit tests for AST->IR translation code

@author
Maxime Chevalier-Boisvert
*/

/**
Test suite for constant propagation
*/
tests.constprop = tests.testSuite();

/**
Helper functions for this test suite
*/
tests.constprop.helpers = {};

/**
Test that only a constant value is returned
*/
tests.constprop.helpers.returnsCst = function (ir, constVal)
{
    tests.ir.helpers.forEachInstr(
        ir,
        function (instr)
        {
            if (instr instanceof RetInstr)
            {
                if (instr.uses[0] !== constVal)
                    error('wrong constant returned');
            }
        },
        false
    );
};

/**
Generate a constant propagation test
*/
tests.constprop.helpers.genTest = function (testCode, constVal)
{
    return function ()
    {
        ir = tests.ir.helpers.testSource(
            "                                       \
                function foo(a)                     \
                {                                   \
                    'tachyon:ret pint';             \
                " + testCode + "                    \
                }                                   \
            ",
            false,
            true
        );

        tests.constprop.helpers.returnsCst(ir, constVal);
    }
}

/**
Addition test
*/
tests.constprop.add = tests.constprop.helpers.genTest(
    'return pint(6) + pint(4);',
    IRConst.getConst(10, IRType.pint)
);

/**
Addition test
*/
tests.constprop.sub = tests.constprop.helpers.genTest(
    'return pint(6) - pint(3);',
    IRConst.getConst(3, IRType.pint)
);

/**
Multiplication test
*/
tests.constprop.mul = tests.constprop.helpers.genTest(
    'return pint(6) * pint(3);',
    IRConst.getConst(18, IRType.pint)
);

/**
Division test
*/
tests.constprop.div = tests.constprop.helpers.genTest(
    'return pint(6) / pint(3);',
    IRConst.getConst(2, IRType.pint)
);

/**
Reciprocal test
*/
tests.constprop.divmul = tests.constprop.helpers.genTest(
    'return (pint(6) / pint(3)) * pint(3);',
    IRConst.getConst(6, IRType.pint)
);

/**
Complex arithmetic expression test
*/
tests.constprop.arithexpr = tests.constprop.helpers.genTest(
    'return ((pint(6) / pint(3)) + pint(2)) * pint(3);',
    IRConst.getConst(12, IRType.pint)
);

/**
Bitwise AND operator
*/
tests.constprop.and = tests.constprop.helpers.genTest(
    'return pint(3) & pint(1);',
    IRConst.getConst(1, IRType.pint)
);

/**
Bitwise OR operator
*/
tests.constprop.or = tests.constprop.helpers.genTest(
    'return pint(5) | pint(2);',
    IRConst.getConst(7, IRType.pint)
);

/**
Bitwise XOR operator
*/
tests.constprop.xor = tests.constprop.helpers.genTest(
    'return pint(5) ^ pint(3);',
    IRConst.getConst(6, IRType.pint)
);

/**
Left shift test
*/
tests.constprop.lsft = tests.constprop.helpers.genTest(
    'return pint(3) << pint(1);',
    IRConst.getConst(6, IRType.pint)
);

/**
Right shift test
*/
tests.constprop.rsft = tests.constprop.helpers.genTest(
    'return pint(8) >> pint(1);',
    IRConst.getConst(4, IRType.pint)
);

/**
Unsigned right shift test
*/
tests.constprop.ursft = tests.constprop.helpers.genTest(
    'return pint(8) >> pint(1);',
    IRConst.getConst(4, IRType.pint)
);

/**
If conditional test
*/
tests.constprop.condIf = tests.constprop.helpers.genTest(
    'if (true) return pint(2); else return pint(3);',
    IRConst.getConst(2, IRType.pint)
);

/**
Conditional operator test
*/
tests.constprop.condOp = tests.constprop.helpers.genTest(
    'return false? pint(2):pint(3);',
    IRConst.getConst(3, IRType.pint)
);

/**
Integer cast test
*/
tests.constprop.icast1 = tests.constprop.helpers.genTest(
    'return iir.icast(IRType.pint, iir.icast(IRType.u16, pint(5)));',
    IRConst.getConst(5, IRType.pint)
);

/**
Redundant integer cast test
*/
tests.constprop.icast2 = tests.constprop.helpers.genTest(
    'return iir.icast(IRType.pint, pint(7));',
    IRConst.getConst(7, IRType.pint)
);

