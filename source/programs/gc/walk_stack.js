function walk_it(verb)
{
    const slotSize = PTR_NUM_BYTES;

    if (verb === true)
        print('\n** starting walk');

    var ra = iir.get_ra();
    var bp = iir.get_bp();

    if (ra === NULL_PTR)
        return 1;

    if (bp === NULL_PTR)
        return 2;

    // Loop variable declarations
    var sp = NULL_PTR;
    var frameSize = pint(0);

    // For each stack level
    for (var numLevels = 0;; ++numLevels)
    {
        if (verb === true)
            iir.trace_print('* stack frame');

        // Note: 8 byte offset to data

        var magic = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(8)));

        if (verb === true)
            print('magic     : ' + boxInt(magic));

        if (magic !== pint(1337))
        {
            if (verb === true)    
                iir.trace_print("magic does't match");

            break;
        }

        var padSpace = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(10)));

        var numSlots = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(12)));

        var raSlot = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(14)));

        // Compute the size of this frame
        var frameSize = numSlots * slotSize;

        if (verb === true)
        {
            print('pad space : ' + boxInt(padSpace));
            print('num slots : ' + boxInt(numSlots));
            print('ra slot   : ' + boxInt(raSlot));
            print('frame size: ' + boxInt(frameSize));
        }

        // If this frame uses dynamic alignment
        if (padSpace === pint(0xFFFF))
        {
            if (verb === true)
                iir.trace_print('dynamic alignment');

            // Load the sp at the base pointer
            var sp = iir.load(IRType.rptr, bp, pint(0));
        }
        else
        {
            // Compute the sp at the moment of the call
            var sp = bp + padSpace;
        }

        //print('num kind bytes: ' + boxInt(numBytes));

        var kindByte = pint(0);

        // For each stack slot, from top to bottom
        for (var i = pint(0); i < numSlots; ++i)
        {
            if (i % pint(4) === pint(0))
            {
                var offset = pint(16) + (i / pint(4));
                kindByte = iir.load(IRType.u8, ra, offset);
                kindByte = iir.icast(IRType.pint, kindByte);

                //print('read kind byte ' + boxInt(kindByte) + ' offset = ' + boxInt(offset));
            }

            var kind = kindByte & pint(3);
            kindByte >>>= pint(2);

            if (verb === true)
                print('slot idx : ' + boxInt(i));
            if (verb === true)
                print('slot kind: ' + boxInt(kind));

            var disp = i * slotSize;

            //print('disp: ' + boxInt(disp));

            // Ref
            if (kind === pint(2))
            {
                var refVal = iir.load(IRType.ref, sp, disp);

                assert (
                    ptrInHeap(iir.icast(IRType.rptr, refVal)) === true,
                    'ref val points out of heap'
                );
            }

            // Box
            if (kind === pint(3))
            {
                var val = iir.load(IRType.box, sp, disp);

                //print('val tag  : ' + boxInt(val & pint(7)));

                if (verb === true)
                {
                    //if (boxIsExtObj(val) === true)
                    //    print('box is obj ext');

                    //if (boxIsFunc(val) === true)
                    //    print('val is func');

                    print('box val: ' + val);
                }

                if (boxIsRef(val) === true)
                {
                    var refVal = unboxRef(val);

                    assert (
                        ptrInHeap(iir.icast(IRType.rptr, refVal)) === true,
                        'ref val points out of heap'
                    );
                }
            }
        }

        // Load the return address for the next frame down
        ra = iir.load(IRType.rptr, sp, raSlot * slotSize);

        // Compute the base pointer for this frame
        bp = sp + frameSize;
    }

    return numLevels;
}

function f_caller(verb)
{
    return walk_it(verb);
}

function f_many_args(verb, a1, a2, a3, a4, a5)
{
    return walk_it(verb);
}

function f_arguments(verb)
{
    var s = 0;
    for (var i = 0; i < arguments.length; ++i)
        s += arguments[i];

    walk_it(verb);

    return s;
}

function f_apply(verb)
{
    return walk_it.apply(null, [verb]);
}

function test()
{
    var verb = false;

    var na = walk_it(verb);
    if (na < 1)
        return 1;

    var nb = f_caller(verb);
    if (nb <= na)
        return 2;

    f_many_args(verb, 2, 3, 4, 5, 'foo');

    // Test with arguments object
    f_arguments(verb, 1, 2, 3, 4, 5);

    // Test with apply, dynamic stack alignment
    f_apply(verb);

    return 0;
}

