function strBufWrite(idx, chVal)
{
    "tachyon:static";
    "tachyon:noglobal";
    "tachyon:arg idx pint";
    "tachyon:arg chVal u16";

    // Get a pointer to the context
    var ctx = iir.get_ctx();

    // Get a reference to the string buffer
    var strbuf = get_ctx_strbuf(ctx);

    // Get the string length and capacity
    var cap = get_strbuf_cap(strbuf);

    // If the capacity needs to be extended
    if (idx >= cap)
    {
        // Allocate a string buffer with twice the current capacity
        var newbuf = alloc_strbuf(pint(2));

        // Copy the characters from the old buffer to the new
        for (var i = pint(0); i < pint(10); ++i)
        {
            var ch = get_strbuf_data(strbuf, i);
            set_strbuf_data(newbuf, i, ch);
        }
    }

    // Update the string length if necessary
    if (idx >= pint(10))
    {
    }
}

