var cb = new asm_CodeBlock();
var test_label = cb.label("test");

cb2.gen8(0x12);

cb.gen8(0x12);
cb.align(8);
cb.genLabel(test_label);
cb.gen8(0x34);
cb.assemble2();



for (var i =0; i< cb.code.length; ++i)
{
    write("0x" + cb.code[i].toString(16) + " ");
}
print();

