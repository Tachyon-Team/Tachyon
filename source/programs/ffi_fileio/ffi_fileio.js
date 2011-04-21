function test()
{
    var fileName = 'programs/ffi_fileio/test.txt';

    //
    // Write the file
    //
    
    var file = fopen(fileName, 'w');
    if (file === NULL_PTR)
        return 1;

    var str1 = 'foo';
    var str2 = 'bar';
    var strcat = str1 + str2;

    var r = fputs(str1, file);
    if (r < 0)
        return 2;
    var r = fputs(str2, file);
    if (r < 0)
        return 3;

    var r = fclose(file);
    if (r !== 0)
        return 4;

    //
    // Read the file
    //

    var file = fopen(fileName, 'r');
    if (file === NULL_PTR)
        return 5;

    var read = fgets(255, file);
    if (read === null)
        return 6;

    if (read !== strcat)
        return 7;

    var r = fclose(file);
    if (r !== 0)
        return 8;

    //
    // Delete the file
    //

    var r = remove(fileName);
    if (r !== 0)
        return 9;

    return 0;
}

