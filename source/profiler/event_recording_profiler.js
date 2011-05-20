/**
Initialize the profiler via 'prof_init' function defined in /runtime/primitives.js
*/
function initProfiler(params){
    var prof_init = params.staticEnv.getBinding('prof_init');
    
    execUnit(prof_init, params);
}

function prof_compilationTimeReport(){
    
    data.compilation_time_report +=
        "\n------- PROFILING: COMPILATION TIME REPORT -------\n\n" +
        "      Parsing time: " + data.parsing_time + "ms\n" +
        "      Compile AST time: " + data.compileAst_time + "ms\n" +
        "\n";
    print(data.compilation_time_report);
}

/**
Produce profiling report in console and text file
*/
function profilerReport(params){
    var prof_allocReport = params.staticEnv.getBinding('prof_allocReport');
    var prof_propAccessReport = params.staticEnv.getBinding('prof_propAccessReport');
    var prof_fileReport = params.staticEnv.getBinding('prof_fileReport');
    var prof_funcCallReport = params.staticEnv.getBinding('prof_funcCallReport');
    var prof_testReport = params.staticEnv.getBinding('prof_testReport');

    //Producing profiling report in console
    execUnit(prof_allocReport, params);
    execUnit(prof_propAccessReport, params);
    execUnit(prof_funcCallReport, params);
    execUnit(prof_testReport, params);

    //Producing profiling report in text file "/src/profiler/profiling_report.txt"
    execUnit(prof_fileReport, params);
}


