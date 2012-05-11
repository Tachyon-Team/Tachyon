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

var log = {};

// log levels

log.DEBUG = 4;
log.TRACE = 3;
log.INFO  = 2;
log.WARN  = 1;
log.ERROR = 0;

// meta-levels
log.ALL = log.DEBUG;
log.NONE = log.ERROR - 1;

log.level = function (s)
{
    if (s === undefined)
    {
        return log.ERROR;
    }

    var level = s.toLowerCase();

    if (level === "trace")
    {
        return log.TRACE;
    }
    else if (level === "debug")
    {
        return log.DEBUG;
    }
    else if (level === "info")
    {
        return log.INFO;
    }
    else if (level === "warn")
    {
        return log.WARN;
    }
    else if (level === "error")
    {
        return log.ERROR;
    }
    else if (level === "none")
    {
        return log.NONE;
    }
    else if (level === "all")
    {
        return log.ALL;
    }
    else 
    {
        error("Unknown log level");
    }
};

log.trace = function (s)
{
    if (config.verbosity >= log.TRACE)
    {
        print(s);
    }
};

log.debug = function (s)
{
    if (config.verbosity >= log.DEBUG)
    {
        print(s);
    }
};

log.info = function (s)
{
    if (config.verbosity >= log.INFO)
    {
        print(s);
    }
};

log.warn = function (s)
{
    if (config.verbosity >= log.WARN)
    {
        print(s);
    }
};

log.error = function (s)
{
    if (config.verbosity >= log.ERROR)
    {
        print(s);
    }
};

