/**
@fileOverview
Code to represent and manipulate functions in the intermediate representation.

@author
Maxime Chevalier-Boisvert

@copyright
Copyright (c) 2010 Maxime Chevalier-Boisvert, All Rights Reserved
*/

/**
@class Intermediate representation function
*/
function IRFunction(funcName, argNames, closVars)
{
    /**
    Function name
    @field
    */
    this.funcName = funcName;

    /**
    Argument name list
    @field
    */
    this.argNames = argNames;

    /**
    Closure variable name list
    @field
    */
    this.closVars = closVars;

    /**
    Virgin, unoptimized IR CFG
    @field
    */
    this.virginIR = null;

    /**
    List of child (nested) functions
    @field
    */
    this.childFuncs = [];

    /**
    Parent function, for nested functions
    @field
    */
    this.parentFunc = null;
}
IRFunction.prototype = new IRValue();

/**
Produce a string representation of an IR function
*/
IRFunction.prototype.toString = function ()
{
    var output = 'function ' + this.funcName + '(';

    for (var i = 0; i < this.argNames.length; ++i)
    {
        output += this.argNames[i];

        if (i != this.argNames.length - 1)
            output += ', ';
    }

    output += ') ['

    for (var i = 0; i < this.closVars.length; ++i)
    {
        output += this.closVars[i];

        if (i != this.closVars.length - 1)
            output += ', ';
    }

    output += ']\n{\n';

    for (var i = 0; i < this.childFuncs.length; ++i)
    {
        output += indentText(this.childFuncs[i].toString(), '    ') + '\n';
    }

    if (this.childFuncs.length > 0)
        output += '\n';

    output += indentText(this.virginIR.toString(), '    ');

    output += '\n}';

    return output;
};

/**
Return the IR value name for this function
*/
IRFunction.prototype.getValName = function ()
{
    return '<func' + (this.funcName? (' "' + this.funcName + '"'):'') + '>';
}

/**
Create a deep copy of the function
*/
IRFunction.prototype.copy = function ()
{
    // TODO
}

/**
Get the argument names
*/
IRFunction.prototype.getArgNames = function()
{
    return this.argNames;
};

/**
Get the default number of function arguments
*/
IRFunction.prototype.getNumArgs = function ()
{
    return this.argNames.length;
};

/**
Add a child function
*/
IRFunction.prototype.addChildFunc = function (func)
{
    this.childFuncs.push(func);

    func.parentFunc = this;
};

/*
func = new IRFunction('foobar', ['foo', 'bar', 'bif'], 'foo\nbar\nbif');

cfg = new ControlFlowGraph(func);

entry = cfg.getEntryBlock();
l1 = cfg.getNewBlock('left');
l2 = cfg.getNewBlock('left');
r1 = cfg.getNewBlock('right');
merge = cfg.getNewBlock('merge');

entry.addInstr(new ArithInstr(ArithOp.DIV, new IntConst(1), new IntConst(2)));
entry.addInstr(new IfInstr(new BoolConst(true), l1, r1));

l1.addInstr(new ArithInstr(ArithOp.ADD, new IntConst(1), new IntConst(2)), 'eee');
l1.addInstr(new GetPropValInstr(cfg.getThisArg(), new IntConst(2)));
l1.addInstr(new JumpInstr(l2));

l2.addInstr(new PhiInstr([l1.instrs[1]], [l1]));
l2.addInstr(new ArithInstr(ArithOp.MOD, l1.instrs[1], new IntConst(7)));
l2.addInstr(new ArithInstr(ArithOp.SUB, new IntConst(3), new IntConst(4)));
l2.addInstr(new ArithInstr(ArithOp.SUB, new IntConst(3), new IntConst(4)));
l2.addInstr(new JumpInstr(merge));

r1.addInstr(new ArithInstr(ArithOp.MUL, new IntConst(7), new IntConst(8)), 'eee');
r1.addInstr(new JumpInstr(merge));

merge.addInstr(new PhiInstr([l1.instrs[0], r1.instrs[0]], [l1, r1]));
merge.addInstr(new SetPropValInstr(entry.instrs[0], new IntConst(2)));
merge.addInstr(new RetInstr(new UndefConst()));

merge.addInstr(new BitInstr(BitOp.LSFT, new IntConst(1), new IntConst(2)), 'foo', 1);

print('ORIGINAL CFG: \n-------------\n');

print(cfg + '\n');

cfg.simplify();

print('SIMPLIFIED CFG: \n---------------\n');

print(cfg + '\n');

cfg2 = cfg.copy();

print('COPY OF CFG: \n---------------\n');

print(cfg2 + '\n');

print('CFG1 VALID: ' + cfg.validate());
print('CFG2 VALID: ' + cfg2.validate());

print("done");
*/

