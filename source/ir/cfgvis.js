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
Control-flow graph visualization.

@author
Maxime Chevalier-Boisvert
*/

/**
Generate a visualization of a control-flow graph
*/
function viewCFG(cfg)
{
    assert (
        cfg instanceof ControlFlowGraph,
        'expected CFG'
    );

    var dotSrc = '';

    function addln(line)
    {
        dotSrc += line + '\n';
    }

    addln('digraph G {');

    // If the function name is defined, label the graph with it
    if (cfg.ownerFunc.funcName !== '')
        addln('label="' + cfg.ownerFunc.funcName + '"');

    // For each CFG block
    for (var itr = cfg.getBlockItr(); itr.valid(); itr.next())
    {
        // Get a reference to the block
        var block = itr.get();

        // Get the block name
        var blockName = block.getBlockName();

        // Get a reference to the branch instruction
        var branch = block.getLastInstr();

        // For each target
        for (var i = 0; i < branch.targets.length; ++i)
        {
            var target = branch.targets[i];

            if (!target)
                continue;

            // Get the target name
            var targetName = target.getBlockName();

            // Get the label for this edge
            var edgeLabel = 
                (branch.targets.length > 1)?
                branch.targetNames[i]:'""';

            // Get the color for this edge
            var edgeColor = 
                (!(branch instanceof IfInstr) && i === 1)? 'red':'black';

            // Add an edge between this block and the target
            addln(
                blockName + ' -> ' + targetName + ' [' +
                'label=' + edgeLabel + ',color=' + edgeColor + ']'
            );
        }
    }

    addln('}');

    //
    // TODO: use temp file names
    //

    // Write the dot source to a file
    writeFile('cfg.dot', dotSrc);

    // Produce the graph using the dot tool
    shellCommand('dot -Tpng cfg.dot -o cfg.png')

    // View the generated graph
    //shellCommand('dotty cfg.dot');
    shellCommand('display cfg.png');

    // Remove the graph and image files
    remove('cfg.dot');
    remove('cfg.png');
}

