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
    css.addChild(new XMLText(cssData, true));
    page.head.addChild(css);

    var js = new XMLElement('script', { type:"text/javascript" });
    js.addChild(new XMLText(jsData, true));
    page.head.addChild(js);

    var ta = this;

    // Next available id number
    var nextId = 0;

    // Assign an id to an element
    function assignId(elem)
    {
        var id = nextId++;
        elem.attribs.id = id;
        return id;
    }

    // Map of color names to color codes
    var colorMap = {
        'grey': '#898973',
        'green': '#009933',
        'blue': '#3333FF',
        'red': '#FF0000',
        'orange': '#FF6600'
    };

    // Set the color of an element
    function setColor(elem, color)
    {
        if (elem.attribs.style)
            elem.attribs.style += ' ';
        else
            elem.attribs.style = '';

        elem.attribs.style += 'color:' + colorMap[color] + ';';
    }

    function makeBubble(span, typeSet)
    {
        var div = new XMLElement('div');

        var divId  = assignId(div);
        var spanId = assignId(span);

        div.attribs['class'] = 'bubble';

        span.attribs.onmouseover = 'showBubble("' + divId + '","' + spanId + '")';
        span.attribs.onmouseout = 'hideBubble("' + divId + '")';

        div.addChild(typeSet.toString());

        page.addContents(div);
    }

    function colorType(elem, instr, useIdx)
    {
        var outType = ta.getTypeSet(instr);

        // If the instruction wasn't visited, stop
        if (outType === null)
            return;

        var type = 
            (useIdx === undefined)? 
            outType:ta.getTypeSet(instr, useIdx);

        // Test if the set contains a single object
        var numObjs = type.getNumObjs();
        var singleObj = numObjs === 1;

        // Color string
        var color = undefined;

        // Empty type set
        if (type === TypeSet.empty)
            color = 'grey';

        // Any/unknown type
        else if (type.flags === TypeFlags.ANY)
            color = 'red';

        // The type is known exactly
        else if (
            type.flags === TypeFlags.UNDEF ||
            type.flags === TypeFlags.NULL  ||
            type.flags === TypeFlags.TRUE  ||
            type.flags === TypeFlags.FALSE ||
            type.flags === TypeFlags.INT   ||
            type.flags === TypeFlags.STRING ||
            ((type.flags & ~TypeFlags.EXTOBJ) === 0 && singleObj) ||
            (type.flags === TypeFlags.CELL && singleObj))
            color = 'green';

        // Undef is in the type set
        else if (type.flags & TypeFlags.UNDEF)
            color = 'orange';

        // More than one type per set, but not undef
        else
            color = 'blue';

        // Make an info-bubble for the element
        makeBubble(elem, type);

        if (color !== undefined)
            setColor(elem, color);
    }

    function visitInstr(instr, indent)
    {
        var elem = new XMLElement('span');

        elem.addChild(indent);

        if (instr.type !== IRType.none)
        {
            var outElem = new XMLElement('span');
            colorType(outElem, instr);
            outElem.addChild(instr.type.name + ' ' + instr.getValName());
            elem.addChild(outElem);        
            elem.addChild(' = ');
        }

        elem.addChild(instr.mnemonic);

        // For each use
        for (useIdx = 0; useIdx < instr.uses.length; ++useIdx)
        {
            elem.addChild(' ')

            var use = instr.uses[useIdx];

            if (instr instanceof PhiInstr)
            {
                var pred = instr.preds[useIdx];

                var predElem = new XMLElement('span');
  
                predElem.addChild('[');
    
                var useElem = new XMLElement('span');
                useElem.addChild(use.getValName());
                predElem.addChild(useElem);

                predElem.addChild(' ');
                predElem.addChild(pred.getBlockName());
                predElem.addChild(']');

                if (ta.blockVisited(pred) === false)
                    setColor(predElem, 'grey');
                else
                    colorType(useElem, instr, useIdx);

                elem.addChild(predElem);
            }
            else
            {
                var useElem = new XMLElement('span');
                useElem.addChild(use.getValName());
                colorType(useElem, instr, useIdx);
                elem.addChild(useElem);
            }

            if (useIdx !== instr.uses.length - 1)
                elem.addChild(',');
        }

        if (instr instanceof ArgValInstr)
        {
            elem.addChild(' ' + instr.argIndex);
        }

        // For each branch target
        for (var i = 0; i < instr.targets.length; ++i)
        {
            var targetElem = new XMLElement('span');

            var target = instr.targets[i];

            targetElem.addChild(
                (instr.targetNames[i]? (' ' + instr.targetNames[i]):'') + 
                ' ' + target.getBlockName()
            );

            // If the target is unvisited, grey it out
            if (ta.blockVisited(target) === false)
                setColor(target, 'grey');

            elem.addChild(targetElem);
        }

        return elem;
    }

    function visitBlock(block, indent)
    {
        var elem = new XMLElement('span');

        // If the block wasn't visited, grey it out
        if (ta.blockVisited(block) === false)
            setColor(elem, 'grey');

        elem.addChild(indent);
        elem.addChild(block.getBlockName() + ':\n');

        // For each instruction of the block
        for (var j = 0; j < block.instrs.length; ++j)
        {
            var instr = block.instrs[j];

            var instrElem = visitInstr(instr, indent);

            elem.addChild(instrElem);
            elem.addChild('\n');
        }

        return elem;
    }

    function visitFunc(func, indent)
    {
        // If the function wasn't visited, do nothing
        var entry = func.hirCFG.entry;
        if (ta.blockVisited(entry) === false)
            return;

        var fnElem = new XMLElement('span');

        fnElem.addChild(indent);
        fnElem.addChild(func.retType + ' function ' + func.funcName + '(');

        for (var i = 0; i < func.argVars.length; ++i)
        {
            fnElem.addChild(func.argTypes[i] + ' ' + func.argVars[i]);

            if (i !== func.argVars.length - 1)
                fnElem.addChild(', ');
        }

        fnElem.addChild(') ');

        // Create an element for the expand/collapse button
        var buttonElem = new XMLElement('span');
        buttonElem.attribs['class'] = 'button';
        buttonElem.addChild('[+]');

        // Create an element for the function body, make it initially hidden
        var bodyElem = new XMLElement('span');
        bodyElem.attribs.style = "display:none;";

        // Allocate ids for the expand button and body
        var buttonId = assignId(buttonElem);
        var bodyId = assignId(bodyElem);

        // Set the click event code for the button
        buttonElem.attribs.onClick = 'toggleBody("' + bodyId + '","' + buttonId + '");';

        fnElem.addChild(buttonElem);
        fnElem.addChild('\n');
        bodyElem.addChild(indent);
        bodyElem.addChild('{\n');

        // Visit the sub-functions
        var subFnsAdded = false;
        for (var i = 0; i < func.childFuncs.length; ++i)
        {
            var subElem = visitFunc(func.childFuncs[i], indent + '    ');
            if (!subElem)
                continue;

            bodyElem.addChild(subElem);
            subFnsAdded = true;
        }

        // For each block of the function
        for (var i = 0; i < func.hirCFG.blocks.length; ++i)
        {
            var block = func.hirCFG.blocks[i];

            var subElem = visitBlock(block, indent + '    ');

            if (subFnsAdded === true || i > 0)
                bodyElem.addChild('\n');

            bodyElem.addChild(subElem);
        }

        bodyElem.addChild(indent);
        bodyElem.addChild('}\n');

        fnElem.addChild(bodyElem);

        return fnElem;
    }

    function visitUnit(ir)
    {
        var pre = new HTMLPre()
        var func = visitFunc(ir, '')
        pre.addChild(func);
        page.addContents(pre);
    }

    this.allUnits.forEach(visitUnit);

    var str = page.toString();
    writeFile(fileName, str);
}

