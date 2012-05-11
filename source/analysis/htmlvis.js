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
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
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
HTML-based analysis output visualization.

@author
Maxime Chevalier-Boisvert
*/

/**
Output an HTML visualization of the analysis results
*/
TypeAnalysis.prototype.writeHTML = function (fileName)
{
    print('Writing HTML output to "' + fileName + '"');

    var jsData = readFile('analysis/html-js.js');
    var cssData = readFile('analysis/html-css.css');

    var page = new HTMLPage('Analysis Results');

    var css = new XMLElement('style', { type:"text/css" });
    css.addChild(cssData);
    page.head.addChild(css);

    var js = new XMLElement('script', { type:"text/javascript" });
    js.addChild(jsData);
    page.head.addChild(js);


    // TODO: list of bubble divs



    function indentElem(elem)
    {
        var sawNL = true;
        var indent = '    ';

        var workList = [];
        workList.push(elem);

        while (workList.length > 0)
        {
            var elem = workList.pop();

            if (elem instanceof XMLText)
            {
                var text = elem.text;

                var chars = [];

                for (var i = 0; i < text.length; ++i)
                {
                    if (sawNL === true)
                    {
                        for (var j = 0; j < indent.length; ++j)
                            chars.push(indent.charCodeAt(j));
                        sawNL = false;
                    }
                
                    var ch = text.charCodeAt(i);

                    // Newline
                    if (ch === 10)
                        sawNL = true;

                    chars.push(ch);
                }

                elem.text = strFromChars(chars);;
            }
            else
            {
                for (var i = elem.children.length - 1; i >= 0; --i)
                    workList.push(elem.children[i]);
            }
        }
    }

    var ta = this;

    function visitInstr(instr)
    {
        // TODO

    }

    function visitBlock(block)
    {
        var elem = new XMLElement('span');

        // If the block wasn't visited, grey it out
        if (ta.blockVisited(block) === false)
            elem.attribs.style = "color: #222222;";

        elem.addChild(block.getBlockName() + ':\n');

        // For each instruction of the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            // TODO
            elem.addChild(instr.toString() + '\n');
        }

        return elem;
    }

    function visitFunc(func)
    {
        // If the function wasn't visited, do nothing
        var entry = func.hirCFG.entry;
        if (ta.blockVisited(entry) === false)
            return;

        var fnElem = new XMLElement('span');

        fnElem.addChild(func.retType + ' function ' + func.funcName + '(');

        for (var i = 0; i < func.argVars.length; ++i)
        {
            fnElem.addChild(func.argTypes[i] + ' ' + func.argVars[i]);

            if (i !== func.argVars.length - 1)
                fnElem.addChild(', ');
        }

        fnElem.addChild(')\n');
        fnElem.addChild('{\n');

        // Visit the sub-functions
        var subFnsAdded = false;
        for (var i = 0; i < func.childFuncs.length; ++i)
        {
            var subElem = visitFunc(func.childFuncs[i]);
            if (!subElem)
                continue;

            indentElem(subElem);
            fnElem.addChild(subElem);
            subFnsAdded = true;
        }

        // For each block of the function
        for (var i = 0; i < func.hirCFG.blocks.length; ++i)
        {
            var block = func.hirCFG.blocks[i];

            var subElem = visitBlock(block);

            if (subFnsAdded === true || i > 0)
                fnElem.addChild('\n');

            indentElem(subElem);
            fnElem.addChild(subElem);
        }

        fnElem.addChild('}\n');

        return fnElem;
    }

    function visitUnit(ir)
    {
        var pre = new HTMLPre()
        var func = visitFunc(ir, pre)
        pre.addChild(func);
        page.addContents(pre);
    }

    this.allUnits.forEach(visitUnit);

    var str = page.toString();
    writeFile(fileName, str);
}

