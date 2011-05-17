function testFP()
{
    var a = alloc_float();
    set_float_f0(a, u16(0)); // Store 1.5
    set_float_f1(a, u16(0));
    set_float_f2(a, u16(0));
    set_float_f3(a, u16(0x3ff8));

    var b = alloc_float(); 
    set_float_f0(b, u16(0)); // Store 1.5
    set_float_f1(b, u16(0));
    set_float_f2(b, u16(0));
    set_float_f3(b, u16(0x3ff8));

    var r = alloc_float();
    r = iir.fadd(a,b,r);

    r = boxInt(iir.ftoi(IRType.pint, r));

    print(r);
}
