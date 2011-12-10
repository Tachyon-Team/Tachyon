function walk_the_walk(verb)
{
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

        var align = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(10)));

        if (verb === true)
            print('align     : ' + boxInt(align));

        var numSlots = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(12)));

        if (verb === true)
            print('num slots : ' + boxInt(numSlots));

        var raDisp = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(14)));

        if (verb === true)
            print('ra disp   : ' + boxInt(raDisp));

        // TODO: handle dynamic alignment
        // Compute the sp at the moment of the call
        var sp = bp + align;
        
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

            var disp = i * PTR_NUM_BYTES;

            //print('disp: ' + boxInt(disp));

            // Box
            if (kind === pint(3))
            {
                var val = iir.load(IRType.box, sp, disp);

                //print('val tag  : ' + boxInt(val & pint(7)));

                if (verb === true)
                {
                    //if (boxIsObjExt(val) === true)
                    //    print('box is obj ext');

                    //if (boxIsFunc(val) === true)
                    //    print('val is func');

                    print('box val: ' + val);
                }
            }
        }

        // Load the return address for the next frame down
        ra = iir.load(IRType.rptr, sp, raDisp);

        // Compute the size of this frame
        var frameSize = numSlots * PTR_NUM_BYTES;

        if (verb === true)
            print('frame size: ' + boxInt(frameSize));

        // Compute the base pointer for this frame
        bp = sp + frameSize;
    }

    return numLevels;
}

function f_caller(verb)
{
    return walk_the_walk(verb);
}

function f_many_args(verb, a1, a2, a3, a4, a5)
{
    return walk_the_walk(verb);
}

function test()
{    
    var na = walk_the_walk(true);
    if (na < 1)
        return 1;

    var nb = f_caller(true);
    if (nb <= na)
        return 2;

    f_many_args(true, 2, 3, 4, 5, 'foo');

    // TODO: test with arguments

    // TODO: test with apply

    return 0;
}

