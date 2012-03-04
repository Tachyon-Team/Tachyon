function argsMax()
{
    var m = 0;

    for (var i = 0; i < arguments.length; ++i)
        if (arguments[i] > m)
            m = arguments[i];

    return m;
}

var m = argsMax(1, 2, 3, 4);

// FIXME: should also be able to infer that m is not undef

typeAssert(m, '"int"');

/*
box function argsMax() []
{
    entry:
    box funcObj = arg 0;
    pint numArgs = get_num_args;
    ref argTable = get_arg_table;
    box $tachyon$13$argsObj = call <fn "makeArgObj">, undef, undef, funcObj, numArgs, argTable;
    jump loop_test;
    
    loop_test:
    box i = phi [box:0 entry], [i_1 if_join];
    box m = phi [box:0 entry], [m_1 if_join];
    box $t = js_get_prop $tachyon$13$argsObj, "length";
    box $t_1 = js_lt i, $t;
    if $t_1 === true then loop_body else loop_exit;
    
    loop_exit:
    ret m;
    
    loop_body:
    box $t_2 = js_get_prop $tachyon$13$argsObj, i;      *********************
    box $t_3 = js_gt $t_2, m;
    if $t_3 === true then if_true else if_join;
    
    if_true:
    box m_2 = js_get_prop $tachyon$13$argsObj, i;       *********************
    jump if_join;
    
    if_join:
    box m_1 = phi [m_2 if_true], [m loop_body];
    box i_1 = js_add i, box:1;
    jump loop_test;
}
*/

