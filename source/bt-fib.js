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

function measurePerformance(msg, fct)
{
    //print(msg);
    fct();
}

function btfib(is64bit)
{
    printTachyonState();

    print("Running after initialization of boostrap code");
    //print("x86: " + x86);
    //print("asm: " + asm);
    //print("IRType: " + IRType);
    //print("IRType.box: " + IRType.box);

    print('Initializing Tachyon');

    // Initialize Tachyon in minimal mode
    initialize(false, is64bit);

    printTachyonState();

    print("Fib: compiling source code");
    //config.hostParams.printRegAlloc = true;
    //config.hostParams.printASM = true;
    var ir = compileSrcString("function fib(n) { if (n < 2) { return n; } else { return fib(n-1) + fib(n-2); } }; printInt(unboxInt(fib(10)));", config.hostParams);
    //var ir = compileSrcString("puts('hello world'); printInt(unboxInt((function (n) { return n; })(10)));", config.hostParams);
    //var ir = compileSrcString("(function () { function fib(n) { if (n < 2) return n; else return fib(n-1) + fib(n-2); } printInt(unboxInt(fib(10))); })();", config.hostParams);

    //var ir = compileSrcFile('bt-test.js', config.hostParams);

    //config.hostParams.printRegAlloc = false;
    //config.hostParams.printASM = false;
    //var ir = compileSrcString("printInt(unboxInt(1));", config.hostParams);
    //var fibCB = backend.compileIRToCB(fibIR, config.hostParams); 

    //print("Fib listing:");
    //print(backend.listing(fibCB));
    
    print('Fib: Creating bridge');
    var bridge = makeBridge(
        ir,
        config.hostParams,
        [],
        new CIntAsBox()
    );

    print("Fib: Executing");
    print(config.hostParams.ctxPtr);
    bridge(config.hostParams.ctxPtr);
}
