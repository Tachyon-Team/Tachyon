function walk_the_walk()
{
    var ra = iir.get_ra();
    var bp = iir.get_bp();

    if (ra === NULL_PTR)
        return 1;

    if (bp === NULL_PTR)
        return 2;

    for (var numLevels = 0;; ++numLevels)
    {
        iir.trace_print('* stack frame');

        printPtr(ra);

        // Note: 8 byte offset to data

        var magic = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(8)));
        print('magic     : ' + boxInt(magic));

        if (magic !== pint(1337))
        {
            iir.trace_print("magic does't match");
            break;
        }

        var align = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(10)));
        print('align     : ' + boxInt(align));

        var numSlots = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(12)));
        print('num slots : ' + boxInt(numSlots));

        var raDisp = iir.icast(IRType.pint, iir.load(IRType.u16, ra, pint(14)));
        print('ra disp   : ' + boxInt(raDisp));

        // TODO: handle dynamic alignment
        // Compute the sp at the moment of the call
        var sp = bp + align;

        // Load the return address for the next frame
        ra = iir.load(IRType.rptr, sp, raDisp);

        // Compute the size of this frame
        var frameSize = numSlots * PTR_NUM_BYTES;
        print('frame size: ' + boxInt(frameSize));

        // Compute the base pointer for this frame
        bp = sp + frameSize;
    }

    return numLevels;
}

function test()
{
    // TODO: verbose arg?

    if (walk_the_walk() < 2)
        return 1;

    return 0;
}

