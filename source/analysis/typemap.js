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
Type map data structure implementation for the type propagation analysis.

@author
Maxime Chevalier-Boisvert
*/

/**
Type map data structure. Maps SSA temporaries to types.
*/
function TypeMap(map, globalType)
{
    if (map === undefined)
        map = new HashMap();

    if (globalType === undefined)
        globalType = TypeDesc.noinf;

    /**
    Hash map of SSA temporaries to type descriptors
    */
    this.map = map;

    /**
    Global object type
    */
    this.globalType = globalType;
}

/**
Get the type for an SSA temp
*/
TypeMap.prototype.getValType = function (val)
{
    // If this is an IR instruction
    if (val instanceof IRInstr)
    {
        // Get the use type from the type map
        return this.map.get(val);
    }
    else
    {
        // Get a type descriptor for the constant
        return TypeDesc.constant(val);
    }
}

/**
Set the type for an SSA temp
*/
TypeMap.prototype.setValType = function (val, type)
{
    this.map.set(val, type);
}

/**
Get the type for the global object
*/
TypeMap.prototype.getGlobalType = function ()
{
    return this.globalType;
}

/**
Set the type for the global object
*/
TypeMap.prototype.setGlobalType = function (type)
{
    this.globalType = type;
}

/**
Copy a type map
*/
TypeMap.prototype.copy = function ()
{
    return new TypeMap(this.map.copy(), this.globalType);
}

/**
Merge another type map into this one. Returns true if this map has changed.
*/
TypeMap.prototype.merge = function (predMap)
{
    // Flag to indicate the successor map was changed
    var changed = false;

    // For each value in the predecessor map
    for (var itr = predMap.map.getItr(); itr.valid() === true; itr.next())
    {
        var pair = itr.get();

        var succType = this.map.get(pair.key);

        if (succType === HashMap.NOT_FOUND)
        {
            this.map.set(pair.key, pair.value);
            changed = true;
        }
        else
        {
            // Merge the types for this value
            var newType = succType.union(pair.value);

            // If the value type changed
            if (succType.equal(newType) === false)
            {
                this.map.set(pair.key, newType);
                changed = true;
            }
        }
    }

    // Merge the global type
    var newGlobal = this.globalType.union(predMap.globalType);

    // If the global type changed
    if (this.globalType.equal(newGlobal) === false)
    {
        this.globalType = newGlobal;
        changed = true;
    }

    // Return the changed flag
    return changed;
}

