//(function () { // local namespace
   // function test ()
    //{
        var a = new x86_Assembler();
        const reg = a.register;
        const ESP = reg.esp;
        const EAX = reg.eax;
        const $   = a.immediateValue;
        const mem = a.memory;
        const _   = function (reg) { return mem(0,reg); };
        const _12   = function (reg) { return mem(12,reg); };
        const _16   = function (reg) { return mem(16,reg); };

        a.ld_handlers   = function () {return this.
                                              movl(_16(ESP), EAX);};
        a.call_print    = function () {return this.
                                              ld_handlers().
                                              call(_12(EAX));};
        a.call_add      = function () {return this.
                                              ld_handlers().
                                              call(_16(EAX));};

        a.print         = function (v) { return this.
                                                addl    ($(-8),   ESP).
                                                push    (v).
                                                call_print().
                                                addl    ($(12),   ESP);};

        // Duplicate top of stack
        a.tos_dup       = function () {return this.
                                              movl(_(ESP),EAX).
                                              addl($(-4),ESP).
                                              movl(EAX,_(ESP));};
        a.tos_add       = function () {return this.
                                              movl(_(ESP),EAX).
                                              addl($(4),  ESP).
                                              addl(EAX,   _(ESP));};

        var trueLabel = a.codeBlock.label("IF_TRUE");
        var falseLabel = a.codeBlock.label("IF_FALSE");

        a.
        movb($(1), reg.al).
        cmpb($(0), reg.al).
        je(trueLabel).
        
        label(falseLabel).
        print($(2)).
        ret     ().

        label(trueLabel).
        print($(1)).
        ret().

        addl(reg.eax, reg.ebx);



        /*
        addl    ($(-8),   ESP).
        push    ($(42)).
        tos_dup ().
        tos_add ().
        call_print().
        addl    ($(12),   ESP).
        */

        a.codeBlock.assemble();

        print(a.codeBlock.listingString());
        /*
        for (var i =0; i< a.codeBlock.code.length; ++i)
        {
            write("0x" + a.codeBlock.code[i].toString(16) + " ");
        }
        print();
        */

        //var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
        //print(execMachineCodeBlock(block)); // execute the code generated
        //freeMachineCodeBlock(block);
   // }
    
    //test();
//})();

