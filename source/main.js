/* _________________________________________________________________________
 *
 *             Tachyon : A Self-Hosted JavaScript Virtual Machine
 *
 *
 *  This file is part of the Tachyon JavaScript project. Tachyon is
 *  distributed at:
 *  http://github.com/Tachyon-Team/Tachyon
 *
 *
 *  Copyright (c) 2011, Universite de Montreal
 *  All rights reserved.
 *
 *  This software is licensed under the following license (Modified BSD
 *  License):
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *    * Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *    * Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *    * Neither the name of the Universite de Montreal nor the names of its
 *      contributors may be used to endorse or promote products derived
 *      from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHApNTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL UNIVERSITE DE
 *  MONTREAL BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * _________________________________________________________________________
 */

/**
@fileOverview
Code specific to the JavaScript VM's entry point, handling of
command-line arguments, etc.

@author
Maxime Chevalier-Boisvert
Marc Feeley
*/

/**
Entry point for the Tachyon VM.
*/
function main()
{
    // If we are running in bootstrap mode
    if (RUNNING_IN_TACHYON)
    {
        // Initialize the Tachyon configuration
        initConfig(PTR_NUM_BYTES === pint(8), log.level(undefined));

        // Perform a minimal Tachyon compilation
        bootstrap(config.hostParams, false, false);

        // Call the Tachyon read-eval-print loop
        tachyonRepl();

        return;
    }

    // Parse the command-line arguments
    var args = parseCmdLine();

    // Get the verbosity command-line option value
    var verbosity = log.level(args.options['v']);

    // Get the 64-bit mode command-line option value
    var x86_64 = args.options['x86_64'];

    // Initialize the Tachyon configuration
    initConfig(x86_64, verbosity);

    // If bootstrap compilation is requested
    if (args.options['bootstrap'])
    {
        // Perform a full bootstrap without writing an image
        bootstrap(config.bootParams, true, false);

        // ???
        // Profit        
    }

    // If we are to write an executable image
    else if (args.options['image'])
    {
        // Perform a full bootstrap and write the image
        // FIXME: for now, not compiling all code, for testing purposes
        bootstrap(config.bootParams, false, true);
    }

    // If gc code generation is requested
    else if (args.options['gc'])
    {
        // Generate the GC code
        genGCCode(config.hostParams);
    }

    // If source files or inline source are provided    
    else if (args.files.length > 0 || args.options['e'])
    {
        // Perform a minimal Tachyon compilation
        bootstrap(config.hostParams, false, false);

        config.hostParams.printAST = args.options["ast"];
        config.hostParams.printHIR = args.options["hir"];
        config.hostParams.printLIR = args.options["lir"];
        config.hostParams.printASM = args.options["asm"];

        // Profiling: configuration
        if (args.options['eventrec']) {

            // The "files" parameter used to identify the user files
            config.hostParams.files = args.files;

            config.hostParams.eventrec = true;
        }

        if (args.options['e'])
        {
            var ir = compileSrcString(args.options['e'] + ";", config.hostParams);
            var bridge = makeBridge(
                ir,
                config.hostParams,
                [],
                new CIntAsBox()
            );

            bridge(config.hostParams.ctxPtr);
        }

        for (var i = 0; i < args.files.length; i++)
        {
            if (args.options["time"])
            {
                print("Executing " + args.files[i]);
            }

            //Initiation of the event recording profiler
            if (args.options['eventrec']) 
                initProfiler(config.hostParams);

            if(args.options["compiletime"])
                config.hostParams.compiletime = true;

            var startTimeMs = (new Date()).getTime();

            var ir = compileSrcFile(args.files[i], config.hostParams);

            var midTimeMs = (new Date()).getTime();

            var bridge = makeBridge(
                ir,
                config.hostParams,
                [],
                new CIntAsBox()
            );

            bridge(config.hostParams.ctxPtr);

            var endTimeMs = (new Date()).getTime();
            var compTimeMs = midTimeMs - startTimeMs;
            var execTimeMs = endTimeMs - midTimeMs;

            if (args.options["time"])
            {
                print("  compilation time: " + compTimeMs + " ms");
                print("  execution time:   " + execTimeMs + " ms");
            }
        }
    }

    // If there are no filenames on the command line, start shell mode
    else
    {
        // Perform a minimal Tachyon compilation
        bootstrap(config.hostParams, false, false);

        // Call the Tachyon read-eval-print loop
        tachyonRepl();
    }

    if (args.options['eventrec'])
            profilerReport(config.hostParams);
}

/**
Tachyon read-eval-print loop
*/
function tachyonRepl()
{
    // Print a help listing
    function printHelp()
    {
        print('Available special commands:');
        print('  /load <filename>               load and execute a script');
        print('  /time <command>                time the compilation/execution of a command');
        print('  /time_comp <command>           time the compilation of a command');
        print('  /time_exec <command>           time the execution of a command');
        print('  /ast  <command>                view AST produced for a command/file');
        print('  /hir  <command>                view HIR produced for a command/file');
        print('  /lir  <command>                view LIR produced for a command/file');
        print('  /asm  <command>                view ASM produced for a command/file');
        print('  /reg  <command>                view register allocation for a command/file');
        print('  /prim_list                     view a list of the primitive functions');
        print('  /prim_ir <func_name>           view LIR produced for a primitive function');
        print('  /cfg <command> [func_name]     visualize the CFG for a command/file/function');
        print('  /help                          print a help listing');
        print('  /exit                          exit the read-eval-print loop');
    }

    // Load and execute a script
    function loadScript(fileName)
    {
        print('Loading script: "' + fileName + '"');

        var ir = compileSrcFile(fileName, config.hostParams);

        var bridge = makeBridge(
            ir,
            config.hostParams,
            [],
            new CIntAsBox()
        );

        bridge(config.hostParams.ctxPtr);
    }

    // Test if a string could be a source file name
    function isSrcFile(str)
    {
        return str.indexOf('.js') === (str.length - 3);
    }

    // Execute a special command
    function execSpecial(cmd)
    {
        var spaceIdx = cmd.indexOf(' ');
        if (spaceIdx !== -1)
        {
            var args = cmd.slice(spaceIdx + 1);
            var cmd = cmd.slice(0, spaceIdx);
        }
        else
        {
            var args = '';
        }

        switch (cmd)
        {
            case 'exit':
            case 'quit':
            return true;

            case 'help':
            printHelp();
            break;

            case 'load':
            loadScript(args);
            break;

            case 'time':
            var startTimeMs = (new Date()).getTime();
            execCode(args);
            var endTimeMs = (new Date()).getTime();
            var time = (endTimeMs - startTimeMs) / 1000;
            print('time: ' + time + 's');
            break;

            case 'time_comp':
            var startTimeMs = (new Date()).getTime();
            compFileOrString(args);
            var endTimeMs = (new Date()).getTime();
            var time = (endTimeMs - startTimeMs) / 1000;
            print('time: ' + time + 's');
            break;

            case 'time_exec':
            var ir = compFileOrString(args);
            var startTimeMs = (new Date()).getTime();
            execIR(ir);
            var endTimeMs = (new Date()).getTime();
            var time = (endTimeMs - startTimeMs) / 1000;
            print('time: ' + time + 's');
            break;

            case 'ast':
            config.hostParams.printAST = true;
            compFileOrString(args);
            config.hostParams.printAST = false;
            break;

            case 'hir':
            config.hostParams.printHIR = true;
            compFileOrString(args);
            config.hostParams.printHIR = false;
            break;

            case 'lir':
            config.hostParams.printLIR = true;
            compFileOrString(args);
            config.hostParams.printLIR = false;
            break;

            case 'asm':
            config.hostParams.printASM = true;
            compFileOrString(args);
            config.hostParams.printASM = false;
            break;

            case 'reg':
            config.hostParams.printRegAlloc = true;
            compFileOrString(args);
            config.hostParams.printRegAlloc = false;
            break;

            case 'prim_list':
            var bindings = config.hostParams.staticEnv.getBindings();
            bindings.forEach(
                function (name)
                {
                    var binding = config.hostParams.staticEnv.getBinding(name);
                    if (binding instanceof IRFunction)
                        print(name);
                }
            );
            break;

            case 'prim_ir':
            if (config.hostParams.staticEnv.hasBinding(args) === false)
            {
                print('primitive not found: "' + args + '"');
                break;
            }
            var func = config.hostParams.staticEnv.getBinding(args);
            print(func);
            break;

            case 'cfg':
            var sepIdx = args.lastIndexOf(' ');
            if (sepIdx === -1)
            {
                var cmd = args;
            }
            else
            {
                var cmd = args.slice(0, sepIdx);
                var funcName = args.slice(sepIdx+1);
            }
            var ir = compFileOrString(cmd);
            if (funcName)
                var func = ir.getChild(funcName);
            else
                var func = ir;
            if (!(func instanceof IRFunction))
            {
                print('child function not found: "' + funcName + '"');
                break;
            }
            viewCFG(func.virginCFG);
            break;

            default:
            print('Unknown special command: "' + cmd + '"');
            break;
        }
    }

    // Execute a code string
    function execCode(str)
    {
        try
        {
            var ir = compString(str);

            execIR(ir);
        }

        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }
    }

    // Execute a compiled IR function
    function execIR(ir)
    {
        var bridge = makeBridge(
            ir,
            config.hostParams,
            [],
            new CIntAsBox()
        );

        bridge(config.hostParams.ctxPtr);
    }

    // Compile a code string
    function compString(str)
    {
        // Add an extra semicolon to avoid syntax errors
        str += ';';

        try
        {
            var ir = compileSrcString(str, config.hostParams);
        }

        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }

        return ir;
    }

    // Compile a source file
    function compFile(str)
    {
        try
        {
            var ir = compileSrcFile(str, config.hostParams);
        }

        catch (e)
        {
            if (e.stack)
                print(e.stack);
            else
                print(e);
        }

        return ir;
    }

    // Compile a source file or string
    function compFileOrString(str)
    {
        if (isSrcFile(str))
            return compFile(str);
        else
            return compString(str);
    }

    print('');
    print('Entering read-eval-print loop.');
    print('Type commands below and press enter to execute them.');
    print('For a listing of special commands, type /help');
    print('To exit, type /exit');

    for (;;)
    {
        var cmd = readConsole('\nt> ');
        if (cmd === undefined || cmd === null) return;

        // Remove extra whitespaces from the command
        cmd = stripStr(cmd);

        // If this is a special command    
        if (cmd.charAt(0) === '/')
        {
            var exit = execSpecial(cmd.slice(1));
            if (exit === true)
                return;
        }
        else
        {
            // Execute the code string
            execCode(cmd);
        }
    }
}

try
{
    main();
}

catch (e)
{
    if (e.stack)
        print(e.stack);
    else
        print(e);
}

