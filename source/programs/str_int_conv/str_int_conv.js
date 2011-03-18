function test()
{
    if ('0000' !== 0 + '000')
        return 1;

    if (0 + '' !== '0')
        return 2;

    return 0;
}
