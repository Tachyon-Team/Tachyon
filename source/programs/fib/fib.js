function fib(x)
{
    if (x < 2)
        return x;
    else
        return fib(x-1) + fib(x-2);

}

//return fib(20);

/*
function timeFib()
{
    var startTimeMs = new Date().getTime();

    fib(40);

    var endTimeMs = new Date().getTime();
    var timeS = (endTimeMs - startTimeMs) / 1000;
    print('time: ' + timeS + ' s');
}
timeFib();
*/
