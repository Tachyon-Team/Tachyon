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
        return log.ALL;
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
