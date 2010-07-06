(function () { // local namespace
    function test ()
    {
        var a = new x86_Assembler();
        const reg = a.register;
        const ESP = reg.esp;
        const EAX = reg.eax;
        const $   = a.immediate;
        const mem = a.memory;
        const _   = function (reg) { return mem(0,reg); };
        const _16   = function (reg) { return mem(16,reg); };

        a.ld_handlers   = function () {return this.gen8(0x8b).gen8(0x44).gen8(0x24).gen8(0x10);};
        a.call_print    = function () {return this.
                                              ld_handlers().
                                              gen8(0xff).gen8(0x50).gen8(0x0c);};
        a.call_add      = function () {return this.
                                              ld_handlers().
                                              gen8(0xff).gen8(0x50).gen8(0x10);};
        // Duplicate top of stack
        a.tos_dup       = function () {return this.
                                              movl(_(ESP),EAX).
                                              addl($(-4),ESP).
                                              movl(EAX,_(ESP));};
        a.tos_add       = function () {return this.
                                              movl(_(ESP),EAX).
                                              addl($(4),  ESP).
                                              addl(EAX,   _(ESP));};

        a.
        addl    ($(-8),   ESP).
        push    ($(123)).
        movl    (_16(ESP), EAX).
        call_print().
        addl    ($(12),   ESP).

        addl    ($(-8),   ESP).
        push    ($(42)).
        tos_dup ().
        tos_add ().
        call_print().
        addl    ($(12),   ESP).

        ret     ();



        for (var i =0; i< a.codeBlock.code.length; ++i)
        {
            write("0x" + a.codeBlock.code[i].toString(16) + " ");
        }
        print();

        var block = a.codeBlock.assembleToMachineCodeBlock(); // assemble it
        print(execMachineCodeBlock(block)); // execute the code generated
        freeMachineCodeBlock(block);
    }
    
    test();
})();

