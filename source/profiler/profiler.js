/**
@fileOverview
Statistical profiler.

@copyright
Copyright (c) 2010 Tachyon Javascript Engine, All Rights Reserved

@author Bruno Dufour (dufour@iro.umontreal.ca)
*/

/** @namespace */
var profiler = profiler || {};

profiler.BlockInfo = function (codeBlock, mcb)
{
    this.codeBlock = codeBlock;
    this.mcb = mcb;
    this.counters = undefined;
    this.sum = 0;
};

profiler.BlockInfo.prototype.getCodeBlock = function ()
{
    return this.codeBlock;
};

profiler.BlockInfo.prototype.getMCB = function ()
{
    return this.mcb;
};

profiler.BlockInfo.prototype.getSamples = function (from, to)
{
    var counters = this.counters;
    if (from === undefined)
        from = 0;

    if (to === undefined)
    {
        if (from >= 0 && from < this.counters.length)
        {
            return counters[from];
        }
        else
        {
            return 0;
        }
    }
    else
    {
        var sum = 0;
        if (to > counters.length) to = counters.length;
        if (from < 0) from = 0;

        for (var i = from; i < to; i++)
        {
            sum = sum + counters[i];
        }
        return sum;
    }
};

profiler.codeblocks = []

/**
 * Initializes the profiler.
 * 
 * @param        {String} timer    The timer to use (one of "prof", "real" or
 *                                 "virtual"). Defaults to "prof".
 * @param       {Integer} interval The sampling rate in microseconds.   
 */
profiler.init = function (timer, interval)
{
    timer = timer || "prof";   // Use ITIMER_PROF internally
    interval = interval || 100; // 1 ms (regular) interval
    // TODO: support a function for stochastic profiling 

    profilerInit(timer, interval);
};

profiler.registerBlock = function (codeBlock, mcb)
{
    profiler.codeblocks.push(new profiler.BlockInfo(codeBlock, mcb));
    profilerRegisterBlock(mcb);
};

profiler.enable = profilerEnable;
profiler.disable = profilerDisable;
profiler.terminate = function () {
    profilerDisable();
    var p = profiler.getProfile();
    var v = new profiler.ShellListing(true);
    for (var i in p.blocks) {
        var b = p.blocks[i];
        if (b.sum > 0)
        {
            p.genListing(b, v);
            print("--------------------");
            print(v.toString());
        }
    }
    profilerTerminate();
}

profiler.getProfile = function () {
    return new profiler.Profile();
};

profiler.Profile = function () {
    this.blocks = profiler.codeblocks.slice(0);
    for (var i in profiler.codeblocks) {
        var block = this.blocks[i];
        block.counters = profilerGetCounters(block.getMCB());
        block.sum = block.getSamples(0, block.counters.length);
    }
};

profiler.Profile.prototype.genListing = function (block, v, precision) {
    precision = precision || 1;

    // if (!codeblock.hasPCs) {
    //     profiler.computePCs(codeblock);
    // }
    
    var codeblock = block.getCodeBlock();
    var code = codeblock.code;

    function percent(n, d, p) {
         if (p === undefined) p = precision;
         var v;
         if (d !== 0) {
             v = n * 100.0 / d;
         } else {
             v = 0.0;
         }
         return v.toFixed(p) + "%";
    };

    // Prepend a synthetic 'start' function header for convenience

    /*
    var newCode = new Array();
    newCode.push(codeblock.listing("<start>", asm.role.FUNC));
    for (var i = 0; i < code.length; i++) {
        if (code[i].type === asm.type.LST) {
            newCode.push(code[i]);
        }
    }
    code = newCode;
    */

    // Compute samples

    var samples = new Array(code.length);
    var block_samples = 0;
    var func_samples = 0;
    for (var i = code.length - 1; i >= 0; i--) {
        var c = code[i];
        switch (c.kind) {
            case asm.role.FUNC:
                samples[i] = func_samples;
                func_samples = 0;
                break;
            case asm.role.BLOCK:
                samples[i] = block_samples;
                block_samples = 0;
                break;
            case asm.role.INST:
                samples[i] = this.getSamples(c.pos);
                block_samples += samples[i];
                func_samples += samples[i];
                break;
            default:
                // Ignore
                break;
        }
    }

    // Generate listing
    v.begin(block);
    for (var i = 0; i < code.length; i++) {
        var c = code[i];
        if (c.type !== asm.type.LST) continue;
        switch (c.role) {
            case asm.role.FUNC:
                func_samples = samples[i];
                v.visitFunction(percent(func_samples, block.sum), c.text);
                break;
            case asm.role.LBL:
                v.visitBlock(percent(samples[i], func_samples), c.text);
                break;
            case asm.role.INST:
                v.visitInstruction(percent(samples[i], func_samples), c.text);
                break;
            default:
                v.visitOther(undefined, c.text);
                break;
        }
    }
    v.end(block);
};

profiler.computePCs = function (codeblock) {
    var code = codeblock.code;
    var pc = 0;
    var last_pc = 0;

    for (var i = 0; i < code.length; i++) {
        if (typeof code[i] === "number") {
            pc++;
        } else {
            code[i].pc = last_pc;
            last_pc = pc;
        }
    }

    codeblock.hasPCs = true;
};

profiler.ShellListing = function (useColor) {
    this.useColor = useColor;
    this.lines = new Array(); // Array of lines

    this.precision = 1;
    this.indent = 2;
    this.padding = 1;
    this.width = this.indent * 2 // indentation
            + 4                  // "100."
            + this.precision     // decimals
            + 1                  // "%"
            + 2 * this.padding;  // padding
};

profiler.ShellListing.prototype.begin = function (block) {
    this.lines.push("Profiling report");
    this.lines.push("----------------------------------------");
    this.lines.push(block.sum + " sample(s) taken");
    this.lines.push("");
};

profiler.ShellListing.prototype.visitFunction = function (p, t) {
    this.visitDefault(p, t, 0, shell.bg.WHITE + shell.fg.BOLD + shell.fg.BLACK);
};

profiler.ShellListing.prototype.visitBlock = function (p, t) {
    this.visitDefault(p, t, 1, shell.bg.BOLD);
};

profiler.ShellListing.prototype.visitInstruction = function (p, t) {
    this.visitDefault(p, t, 2, shell.bg.NONE);
};

profiler.ShellListing.prototype.visitOther = function (p, t) {
    this.visitDefault(p, t, 2, shell.bg.NONE);
};

profiler.ShellListing.prototype.visitDefault = function (p, t, level, color) {
    function spaces(n) 
    {
        return new Array(n+1).join(" ");
    };

    if (p === undefined) p = "";
    if (t === undefined) t = "";
    var s = spaces(level * this.indent) + p;
    s = rightPadStr(s, " ", this.width) + t;

    if (this.useColor) {
        s = shell.colorize(s, color);
    }
    
    this.lines.push(s);
};


profiler.ShellListing.prototype.end = function (block) {
    this.lines.push("----------------------------------------");
};

profiler.ShellListing.prototype.toString = function () {
    return this.lines.join("\n");
};

profiler.listing = function (codeblock, profile, useColor) {
    profile = profile || profiler.getProfile();
    useColor = useColor || true;

    v = new profiler.ShellListing(useColor);
    profile.genListing(codeblock, v, v.precision);
    return v.toString();
};


profiler.HTMLListing = function() {
    this.htmldoc = new HTMLPage("Profiling report");

    // this.css = new XMLElement("style", { "content" : "text/css" });
    // var css = "/* Insert CSS here */";
    // this.css.addChild(new XMLText(css));

    this.css = new XMLElement("link", { "href" : "profile.css", "rel" : "stylesheet", "type" : "text/css" });
    this.htmldoc.head.addChild(this.css);

    this.currentListing = undefined;
};

profiler.HTMLListing.prototype.begin = function (block) {
    this.currentListing = new XMLElement("table", { "class" : "listing" });
    this.htmldoc.addContents(this.currentListing);
};

profiler.HTMLListing.prototype.visitFunction = function (p, t) {
    this.visitDefault(p, t, "function");
};

profiler.HTMLListing.prototype.visitBlock = function (p, t) {
    this.visitDefault(p, t, "block");
};

profiler.HTMLListing.prototype.visitInstruction = function (p, t) {
    this.visitDefault(p, t, "inst");
};

profiler.HTMLListing.prototype.visitOther = function (p, t) {
    this.visitDefault(p, t, "other");
};

profiler.HTMLListing.prototype.visitDefault = function (p, t, cssClass) {
    var tr = new XMLElement("tr", { "class" : cssClass });
    this.currentListing.addChild(tr);
    
    var span;
    
    // Add percentage of execution time
    var val = new XMLElement("td", { "class" : "value" });
    tr.addChild(val);
    if (p) {
        span = new XMLElement("span", { "class" : "value" });
        val.addChild(span);
        span.addChild(new XMLText(p));
    }

    // Add label
    var label = new XMLElement("td", { "class" : "label" });
    tr.addChild(label);
    if (t) {
        span = new XMLElement("span", { "class" : "label" });
        label.addChild(span);
        span.addChild(new XMLText(t));
    }
};

profiler.HTMLListing.prototype.end = function (block) {

};

profiler.toHTML = function (codeblock, profile, filename) {
    profile = profile || profiler.getProfile();

    v = new profiler.HTMLListing();
    profile.genListing(codeblock, v, 1);

    if (filename) {
        v.htmldoc.toFile(filename);
        return undefined;
    } else {
        return v.htmldoc.toString();
    }
}
