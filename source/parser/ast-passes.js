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

//=============================================================================

// File: "ast-passes.js", Time-stamp: <2011-03-01 13:20:29 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Utility functions

function get_free_id(postFix, loc)
{
    return new Token(
        IDENT_CAT,
        ("$tachyon$" + (get_free_id.nextIdNum++) + "$" + postFix),
        loc
    );
}

get_free_id.nextIdNum = 0;

//=============================================================================

// Generic AST walker.

function ast_walk_statement(ast, ctx)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        ast.block = ctx.walk_statement(ast.block);
        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        ast.funct = ctx.walk_expr(ast.funct);
        return ast;
    }
    else if (ast instanceof BlockStatement)
    {
        ast.statements = ast_walk_statements(ast.statements, ctx);
        return ast;
    }
    else if (ast instanceof VariableStatement)
    {
        ast.decls.forEach(function (decl, i, self)
                          {
                              decl.initializer = ctx.walk_expr(decl.initializer);
                          });
        return ast;
    }
    else if (ast instanceof ConstStatement)
    {
        // TODO
        error("ConstStatement not implemented");
        return ast;
    }
    else if (ast instanceof ExprStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof IfStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statements = ast_walk_statements(ast.statements, ctx);
        return ast;
    }
    else if (ast instanceof DoWhileStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof WhileStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ForStatement)
    {
        ast.expr1 = ctx.walk_expr(ast.expr1);
        ast.expr2 = ctx.walk_expr(ast.expr2);
        ast.expr3 = ctx.walk_expr(ast.expr3);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ForVarStatement)
    {
        for (var i=ast.decls.length-1; i>=0; i--)
        {
            var decl = ast.decls[i];
            decl.initializer = ctx.walk_expr(decl.initializer);
        }
        ast.expr2 = ctx.walk_expr(ast.expr2);
        ast.expr3 = ctx.walk_expr(ast.expr3);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ForInStatement)
    {
        ast.lhs_expr = ctx.walk_expr(ast.lhs_expr);
        ast.set_expr = ctx.walk_expr(ast.set_expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ForVarInStatement)
    {
        ast.initializer = ctx.walk_expr(ast.initializer);
        ast.set_expr = ctx.walk_expr(ast.set_expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ContinueStatement)
    {
        return ast;
    }
    else if (ast instanceof BreakStatement)
    {
        return ast;
    }
    else if (ast instanceof ReturnStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof WithStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof SwitchStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.clauses.forEach(function (c, i, asts)
                            {
                                c.expr = ctx.walk_expr(c.expr);
                                c.statements = ast_walk_statements(c.statements, ctx);
                            });
        return ast;
    }
    else if (ast instanceof LabelledStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof ThrowStatement)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
    else if (ast instanceof TryStatement)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        ast.catch_part = ctx.walk_statement(ast.catch_part);
        ast.finally_part = ctx.walk_statement(ast.finally_part);
        return ast;
    }
    else if (ast instanceof CatchPart)
    {
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof DebuggerStatement)
    {
        return ast;
    }
    else if (ast instanceof AtomicStatement) /********* extensions *********/
    {
        ast.statement = ctx.walk_statement(ast.statement);
        return ast;
    }
    else if (ast instanceof FutureStatement) /********* extensions *********/
    {
        ast.expr = ctx.walk_expr(ast.expr);
        return ast;
    }
   else
    {
        //pp(ast);
        error("unknown ast in walk_statement");
    }
}

function ast_walk_statements(asts, ctx)
{
    asts.forEach(function (ast, i, asts)
                 {
                     asts[i] = ctx.walk_statement(ast);
                 });
    return asts;
}

function ast_walk_expr(ast, ctx)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof OpExpr)
    {
        ast.exprs = ast_walk_exprs(ast.exprs, ctx);
        return ast;
    }
    else if (ast instanceof NewExpr)
    {
        ast.expr = ctx.walk_expr(ast.expr);
        ast.args = ast_walk_exprs(ast.args, ctx);
        return ast;
    }
    else if (ast instanceof CallExpr)
    {
        ast.fn = ctx.walk_expr(ast.fn);
        ast.args = ast_walk_exprs(ast.args, ctx);
        return ast;
    }
    else if (ast instanceof FunctionExpr)
    {
        ast.body = ast_walk_statements(ast.body, ctx);
        return ast;
    }
    else if (ast instanceof Literal)
    {
        return ast;
    }
    else if (ast instanceof ArrayLiteral)
    {
        ast.exprs = ast_walk_exprs(ast.exprs, ctx);
        return ast;
    }
    else if (ast instanceof RegExpLiteral)
    {
        return ast;
    }
    else if (ast instanceof ObjectLiteral)
    {
        ast.properties.forEach(function (prop, i, self)
                               {
                                   // name shouldn't be treated as an expression
                                   //prop.name = ctx.walk_expr(prop.name);
                                   prop.value = ctx.walk_expr(prop.value);
                               });
        return ast;
    }
    else if (ast instanceof Ref)
    {
        return ast;
    }
    else if (ast instanceof This)
    {
        return ast;
    }
    else
    {
        //pp(ast);
        error("unknown ast in walk_expr");
    }
}

function ast_walk_exprs(asts, ctx)
{
    asts.forEach(function (ast, i, asts)
                 {
                     asts[i] = ctx.walk_expr(ast);
                 });
    return asts;
}


//-----------------------------------------------------------------------------

// Pass 1.
//
// Adds profiling code.

function ast_pass1_ctx()
{
    this.fn_decl = null;
}


ast_pass1_ctx.prototype.walk_statement = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        var s = new Scanner(new String_input_port(profile_lib));
        var p = new Parser(s, false);
        var prog = p.parse();

        ast_walk_statement(ast, this);

        ast.block.statements = prog.block.statements.concat(ast.block.statements);

        ast.block.statements.push(
            new ExprStatement(ast.loc,
                              new CallExpr(ast.loc,
                                           new Ref(ast.loc,
                                                   new Token(IDENT_CAT, "profile$end_of_program", ast.loc)),
                                           [])));

        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        var save_fn_decl = this.fn_decl;
        this.fn_decl = ast;
        ast.funct = this.walk_expr(ast.funct);
        this.fn_decl = save_fn_decl;
        return ast;
    }
    else if (ast instanceof ReturnStatement)
    {
        ast.expr = this.walk_expr(ast.expr);
        if (!this.filter_debug(ast))
        {
            return ast;
        }
        else if (ast.expr !== null)
        {
            ast.expr = this.call_hook("profile$return1", ast.loc, ast.expr);
            return ast;
        }
        else
        {
            return new BlockStatement(ast.loc,
                                      [new ExprStatement(ast.loc,
                                                        this.call_hook("profile$return0", ast.loc)),
                                       ast]);
        }
    }
    else
    {
        return ast_walk_statement(ast, this);
    }
};

function is_ref(a)
{
    return a instanceof Ref;
}

function is_prop_access(a)
{
    return a instanceof OpExpr && is_prop_access_op(a.op);
}

function is_prop_access_op(op)
{
    return op === "x [ y ]";
}

function is_lvalue(a)
{
    return is_ref(a) || is_prop_access(a);
}

function is_assign_op1(op)
{
    return op === "++ x" ||
           op === "-- x" ||
           op === "x ++" ||
           op === "x --";
}

function is_assign_op2(op)
{
    return op === "x = y" ||
           op === "x += y" ||
           op === "x -= y" ||
           op === "x *= y" ||
           op === "x /= y" ||
           op === "x <<= y" ||
           op === "x >>= y" ||
           op === "x >>>= y" ||
           op === "x &= y" ||
           op === "x ^= y" ||
           op === "x |= y" ||
           op === "x %= y";
}

ast_pass1_ctx.prototype.walk_expr = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof OpExpr)
    {
        var op = ast.op;

        if (is_prop_access_op(op)) // handle "a[b]"
        {
            return this.call_hook("profile$get_prop",
                                  ast.loc,
                                  this.walk_expr(ast.exprs[0]),
                                  this.walk_expr(ast.exprs[1]));
        }
        else if (is_assign_op1(op))
        {
            var fn;

                 if (op === "++ x") fn = "_preinc";
            else if (op === "-- x") fn = "_predec";
            else if (op === "x ++") fn = "_postinc";
            else if (op === "x --") fn = "_postdec";
            else error("unknown assignment operator " + op);

            if (is_prop_access(ast.exprs[0])) // handle "a[b] ++"
            {
                return this.call_hook("profile$put_prop" + fn,
                                      ast.loc,
                                      this.walk_expr(ast.exprs[0].exprs[0]),
                                      this.walk_expr(ast.exprs[0].exprs[1]));
            }
            else
            {
                return this.call_hook("profile$set_var" + fn,
                                      ast.loc,
                                      ast);
            }
        }
        else if (is_assign_op2(op))
        {
            var fn;

                 if (op === "x = y")    fn = "";
            else if (op === "x += y")   fn = "_add";
            else if (op === "x -= y")   fn = "_sub";
            else if (op === "x *= y")   fn = "_mul";
            else if (op === "x /= y")   fn = "_div";
/* TODO
            else if (op === "x <<= y")  fn = "_lsh";
            else if (op === "x >>= y")  fn = "_rsh";
            else if (op === "x >>>= y") fn = "_ursh";
            else if (op === "x &= y")   fn = "_and";
            else if (op === "x ^= y")   fn = "_xor";
            else if (op === "x |= y")   fn = "_ior";
            else if (op === "x %= y")   fn = "_mod";
*/
            else error("unknown assignment operator " + op);

            if (is_prop_access(ast.exprs[0])) // handle "a[b] += c"
            {
                return this.call_hook("profile$put_prop" + fn,
                                      ast.loc,
                                      this.walk_expr(ast.exprs[0].exprs[0]),
                                      this.walk_expr(ast.exprs[0].exprs[1]),
                                      this.walk_expr(ast.exprs[1]));
            }
            else
            {
                ast.exprs[1] = this.walk_expr(ast.exprs[1]);
                return this.call_hook("profile$set_var" + fn,
                                      ast.loc,
                                      ast);
            }
        }

        return ast_walk_expr(ast, this);
    }
    else if (ast instanceof NewExpr)
    {
        return this.call_hook("profile$NewExpr_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
    else if (ast instanceof CallExpr)
    {
        if (is_prop_access(ast.fn))
        {
            return this.call_hook.apply(this,
                                        ["profile$call_prop",
                                         ast.loc,
                                         this.walk_expr(ast.fn.exprs[0]),
                                         this.walk_expr(ast.fn.exprs[1])]
                                        .concat(ast_walk_exprs(ast.args, this)));
        }

        return this.call_hook("profile$CallExpr_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
    else if (ast instanceof FunctionExpr)
    {
        ast_walk_statements(ast.body, this);

        if (this.filter_debug(ast))
        {
            ast.body.unshift(
                new ExprStatement(ast.loc,
                                  this.call_hook(
                                      "profile$enter",
                                      ast.loc,
                                      new Literal(ast.loc,
                                                  ((this.fn_decl !== null)
                                                   ? this.fn_decl.id.toString()
                                                   : "")
                                                  + "(" + ast.params.join() + ")"),
                                      new Ref(ast.loc,
                                              new Token(IDENT_CAT, "arguments", ast.loc)))));
            ast.body.push(new ExprStatement(ast.loc,
                                            this.call_hook("profile$return0",
                                                           ast.loc)));
        }

        return ast;
        return this.call_hook("profile$FunctionExpr_hook",
                              ast.loc,
                              ast);
    }
    else if (ast instanceof Literal)
    {
        return this.call_hook("profile$Literal_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
    else if (ast instanceof ArrayLiteral)
    {
        return this.call_hook("profile$ArrayLiteral_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
    else if (ast instanceof RegExpLiteral)
    {
        return this.call_hook("profile$RegExpLiteral_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
    else if (ast instanceof ObjectLiteral)
    {
        return this.call_hook("profile$ObjectLiteral_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
/*
  doesn't work due to "new foo(...)"

    else if (ast instanceof Ref)
    {
        return this.call_hook("profile$Ref_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
*/
    else if (ast instanceof This)
    {
        return this.call_hook("profile$This_hook",
                              ast.loc,
                              ast_walk_expr(ast, this));
    }
    else
    {
        return ast_walk_expr(ast, this);
    }
};

ast_pass1_ctx.prototype.call_hook = function (fn, loc)
{
    var args = [new Literal(loc,
                            loc.to_string())];

    for (i=2; i<arguments.length; i++)
        args.push(arguments[i]);
                    
    return new CallExpr(loc,
                        new Ref(loc,
                                new Token(IDENT_CAT, fn, loc)),
                        args);
};

ast_pass1_ctx.prototype.filter_debug = function (ast)
{
    // TODO: This filtering should be user configurable from a command line option.
    if (ast.loc.filename === "parser/parser.js" ||
        ast.loc.filename === "parser/scanner.js" ||
//        ast.loc.filename === "utility/debug.js" ||
        (this.fn_decl !== null &&
         this.fn_decl.id.toString() === "assert"))
        return false;
    return true;
};

function ast_pass1(ast)
{
    var ctx = new ast_pass1_ctx();
    ctx.walk_statement(ast);
}

var profile_lib = "                                               \
                                                                  \
function profile$String_output_port()                             \
{                                                                 \
    this.char_buffer = [];                                        \
    this.string_buffer = [];                                      \
}                                                                 \
                                                                  \
profile$String_output_port.prototype.empty_char_buffer = function () \
{                                                                 \
    if (this.char_buffer.length > 0)                              \
    {                                                             \
        this.string_buffer.push(String.fromCharCode.apply(null, this.char_buffer)); \
        this.char_buffer = [];                                    \
    }                                                             \
};                                                                \
                                                                  \
profile$String_output_port.prototype.write_char = function (c)    \
{                                                                 \
    this.char_buffer.push(c);                                     \
    if (this.char_buffer.length > 500)                            \
        this.empty_char_buffer();                                 \
};                                                                \
                                                                  \
profile$String_output_port.prototype.write_string = function (str) \
{                                                                 \
    for (var i=0; i<str.length; i++)                              \
        this.write_char(str.charCodeAt(i));                       \
};                                                                \
                                                                  \
profile$String_output_port.prototype.print = function (str) \
{                                                                 \
    this.write_string(str + \"\\n\");                             \
};                                                                \
                                                                  \
profile$String_output_port.prototype.get_output_string = function () \
{                                                                 \
    this.empty_char_buffer();                                     \
    return String.prototype.concat.apply(\"\", this.string_buffer); \
};                                                                \
                                                                  \
function profile$print(str)                                       \
{ profile$output.print(str);                                      \
}                                                                 \
                                                                  \
var profile$output = new profile$String_output_port();            \
                                                                  \
var profile$trace = false;                                        \
var profile$nesting = -1;                                         \
var profile$stack = [];                                           \
var profile$fn_abstypes = {};                                     \
var profile$prop_access_abstypes = {};                            \
var profile$fetch_counter = 0;                                    \
var profile$store_counter = 0;                                    \
var profile$obj_counter = 0;                                      \
var profile$new_counter = 0;                                      \
var profile$map_counter = 0;                                      \
var profile$hit_counter = 0;                                      \
var profile$miss_counter = 0;                                     \
var profile$map_cache = {};                                       \
var profile$maps = [];                                            \
                                                                  \
function profile$Map(len, props, maxindex)                        \
{ this.len = len;                                                 \
  this.props = props;                                             \
  this.maxindex = maxindex;                                       \
  this.id = profile$map_counter++;                                \
  profile$maps.push(this);                                        \
}                                                                 \
                                                                  \
function profile$numify(obj)                                      \
{ if (typeof obj === \"string\")                                  \
  { var n = Number(obj);                                          \
    if (n.toString() === obj)                                     \
      return n;                                                   \
  }                                                               \
  return obj;                                                     \
}                                                                 \
                                                                  \
function profile$map_to_string(map)                               \
{ var str = \"\";                                                 \
  if (map.maxindex >= 0)                                          \
  { if (str !== \"\") str += \",\";                               \
    str += \"0..\" + map.maxindex;                                \
  }                                                               \
  for (var p in map.props)                                        \
  { p = profile$numify(p);                                        \
    if (!(map.props[p] instanceof profile$Map))                   \
    { if (str !== \"\") str += \",\";                             \
      str += p;                                                   \
    }                                                             \
  }                                                               \
  return \"{\" + str + \"}\";                                     \
}                                                                 \
                                                                  \
var profile$map_root = new profile$Map(0, {}, -1);                \
                                                                  \
function profile$clone(obj)                                       \
{ if (obj === null || typeof obj !== \"object\")                  \
    return obj;                                                   \
                                                                  \
  if (obj instanceof Array)                                       \
  { var copy = [];                                                \
    for (var i = 0; i < obj.length; ++i)                          \
      copy[i] = profile$clone(obj[i]);                            \
    return copy;                                                  \
  }                                                               \
                                                                  \
  if (obj instanceof Object)                                      \
  { var copy = {};                                                \
    for (var attr in obj)                                         \
      if (obj.hasOwnProperty(attr))                               \
        copy[attr] = profile$clone(obj[attr]);                    \
    return copy;                                                  \
  }                                                               \
                                                                  \
  error(\"Unsupported object type\");                             \
}                                                                 \
                                                                  \
function profile$is_nonneg_int(obj)                               \
{ return typeof obj === \"number\" &&                             \
         Math.floor(obj) === obj &&                               \
         obj >= 0;                                                \
}                                                                 \
                                                                  \
function profile$copy_props(props)                                \
{ var newprops = {};                                              \
  for (var p in props)                                            \
  { p = profile$numify(p);                                        \
    if (!(props[p] instanceof profile$Map))                       \
      newprops[p] = props[p];                                     \
  }                                                               \
  return newprops;                                                \
}                                                                 \
                                                                  \
function profile$map_extend(map, prop)                            \
{ prop = profile$numify(prop);                                    \
  if (profile$is_nonneg_int(prop) && prop <= map.maxindex)        \
    return map;                                                   \
  else if (map.props.hasOwnProperty(prop))                        \
  { if (!(map.props[prop] instanceof profile$Map))                \
      return map;                                                 \
    else                                                          \
      return map.props[prop];                                     \
  }                                                               \
  else                                                            \
  { var subprops = profile$copy_props(map.props);                 \
    var submap;                                                   \
    if (profile$is_nonneg_int(prop))                              \
      submap = new profile$Map(map.len, subprops, prop);          \
    else                                                          \
    { subprops[prop] = map.len;                                   \
      submap = new profile$Map(map.len+1, subprops, map.maxindex); \
    }                                                             \
    map.props[prop] = submap;                                     \
    return submap;                                                \
  }                                                               \
}                                                                 \
                                                                  \
function profile$object_map_init(obj)                             \
{ var map = profile$map_root;                                     \
  for (var p in obj)                                              \
    map = profile$map_extend(map, p);                             \
  return map;                                                     \
}                                                                 \
                                                                  \
function profile$object_info_init(obj)                            \
{ var info = { sn: profile$obj_counter++,                         \
               map: null                                          \
             };                                                   \
  Object.defineProperty(obj,                                      \
                        \"profile$info\",                         \
                        { value: info,                            \
                          enumerable: false                       \
                        });                                       \
  info.map = profile$object_map_init(obj);                        \
  return info;                                                    \
}                                                                 \
                                                                  \
function profile$object_info(obj)                                 \
{ var info;                                                       \
  if (!obj.hasOwnProperty(\"profile$info\"))                      \
    info = profile$object_info_init(obj);                         \
  else                                                            \
    info = obj[\"profile$info\"];                                 \
  return info;                                                    \
}                                                                 \
                                                                  \
function profile$get_tp_descr()                                   \
{ return profile$fn_abstypes[profile$stack[profile$nesting]];     \
}                                                                 \
                                                                  \
function profile$enter_tp(fn, args)                               \
{ var descr = profile$get_tp_descr();                             \
  if (descr === undefined)                                        \
    { var loc = profile$stack[profile$nesting];                   \
      descr = { loc: loc,                                         \
                fn: fn,                                           \
                calls: 0,                                         \
                args: [],                                         \
                result: undefined                                 \
              };                                                  \
      profile$fn_abstypes[loc] = descr;                           \
    }                                                             \
  descr.calls++;                                                  \
  for (var i=0; i<args.length; i++)                               \
     descr.args[i] = profile$abstype_add(descr.args[i], args[i]); \
}                                                                 \
                                                                  \
function profile$return_tp(result)                                \
{ var descr = profile$get_tp_descr();                             \
  descr.result = profile$abstype_add(descr.result, result);       \
}                                                                 \
                                                                  \
function profile$absnum_add(absnum, val)                          \
{ if (absnum === undefined)                                       \
    absnum = { minnum: val,                                       \
               maxnum: val                                        \
             };                                                   \
  else if (val < absnum.minnum)                                   \
    absnum.minnum = val;                                          \
  else if (val > absnum.maxnum)                                   \
    absnum.maxnum = val;                                          \
  return absnum;                                                  \
}                                                                 \
                                                                  \
function profile$absnum_to_string(absnum)                         \
{ if (absnum.minnum === absnum.maxnum)                            \
    return \"\" + absnum.minnum;                                  \
  else                                                            \
    return absnum.minnum + \"..\" + absnum.maxnum;                \
}                                                                 \
                                                                  \
function profile$absbool_add(absbool, val)                        \
{ if (absbool === undefined)                                      \
    absbool = {};                                                 \
  absbool[val] = true;                                            \
  return absbool;                                                 \
}                                                                 \
                                                                  \
function profile$absbool_to_string(absbool)                       \
{ var str = \"\";                                                 \
  if (absbool[true] !== undefined)                                \
    { if (str !== \"\") str += \" U \"; str += \"true\"; }        \
  if (absbool[false] !== undefined)                               \
    { if (str !== \"\") str += \" U \"; str += \"false\"; }       \
  return str;                                                     \
}                                                                 \
                                                                  \
function profile$absobj_add(absobj, val)                          \
{ if (absobj === undefined)                                       \
    absobj = {};                                                  \
  var info = profile$object_info(val);                            \
  absobj[info.map.id] = true;                                     \
  return absobj;                                                  \
}                                                                 \
                                                                  \
function profile$absobj_to_string(absobj)                         \
{ var str = \"\";                                                 \
  for (var x in absobj)                                           \
    { if (str !== \"\") str += \" U \"; str += \"map[\" + x + \"]\"; } \
  return str;                                                     \
}                                                                 \
                                                                  \
function profile$abstype_add(abstype, val)                        \
{ if (abstype === undefined)                                      \
    abstype = { num: undefined,                                   \
                str: undefined,                                   \
                bool: undefined,                                  \
                undef: undefined,                                 \
                nul: undefined,                                   \
                fn: undefined,                                    \
                obj: undefined,                                   \
                other: []                                         \
              };                                                  \
  if (val !== null && typeof val === 'object')                    \
    profile$object_info(val);                                     \
  if (typeof val === 'number')                                    \
    abstype.num = profile$absnum_add(abstype.num, val);           \
  else if (typeof val === 'string')                               \
    abstype.str = profile$absnum_add(abstype.str, val.length);    \
  else if (typeof val === 'boolean')                              \
    abstype.bool = profile$absbool_add(abstype.bool, val);        \
  else if (val === undefined)                                     \
    abstype.undef = true;                                         \
  else if (val === null)                                          \
    abstype.nul = true;                                           \
  else if (typeof val === 'function')                             \
    abstype.fn = true;                                            \
  else if (typeof val === 'object')                               \
    abstype.obj = profile$absobj_add(abstype.obj, val);           \
  else                                                            \
    abstype.other.push(val);                                      \
  return abstype;                                                 \
}                                                                 \
                                                                  \
function profile$abstype_to_string(abstype)                       \
{ var str = \"\";                                                 \
  if (abstype.num !== undefined)                                  \
    { if (str !== \"\") str += \" U \";                           \
      str += profile$absnum_to_string(abstype.num);               \
    }                                                             \
  if (abstype.str !== undefined)                                  \
    { if (str !== \"\") str += \" U \";                           \
      str += \"string[\" + profile$absnum_to_string(abstype.str) + \"]\"; \
    }                                                             \
  if (abstype.bool !== undefined)                                 \
    { if (str !== \"\") str += \" U \";                           \
      str += profile$absbool_to_string(abstype.bool);             \
    }                                                             \
  if (abstype.undef !== undefined)                                \
    { if (str !== \"\") str += \" U \";                           \
      str += \"undefined\";                                       \
    }                                                             \
  if (abstype.nul !== undefined)                                  \
    { if (str !== \"\") str += \" U \";                           \
      str += \"null\";                                            \
    }                                                             \
  if (abstype.fn !== undefined)                                   \
    { if (str !== \"\") str += \" U \";                           \
      str += \"function\";                                        \
    }                                                             \
  if (abstype.obj !== undefined)                                  \
    { if (str !== \"\") str += \" U \";                           \
      str += profile$absobj_to_string(abstype.obj);               \
    }                                                             \
  if (abstype.other.length > 0)                                   \
    { if (str !== \"\") str += \" U \";                           \
      str += abstype.other.join(\" U \");                         \
    }                                                             \
  return str;                                                     \
}                                                                 \
                                                                  \
function profile$report()                                         \
{ var fn_descrs = [];                                             \
  for (var loc in profile$fn_abstypes)                            \
    fn_descrs.push(profile$fn_abstypes[loc]);                     \
  fn_descrs.sort(function (x,y) { return (x.calls > y.calls) ? 1 : -1; }); \
                                                                  \
  var prop_access_descrs = [];                                    \
  for (var loc in profile$prop_access_abstypes)                   \
    prop_access_descrs.push(profile$prop_access_abstypes[loc]);   \
  prop_access_descrs.sort(function (x,y) { return (x.accesses > y.accesses) ? 1 : -1; }); \
                                                                  \
  profile$output = new profile$String_output_port();              \
                                                                  \
  profile$print(\"--------------------------- TYPE PROFILE\");    \
  profile$print(\"fetch_counter = \" + profile$fetch_counter);    \
  profile$print(\"store_counter = \" + profile$store_counter);    \
  profile$print(\"obj_counter = \" + profile$obj_counter);        \
  profile$print(\"new_counter = \" + profile$new_counter);        \
  profile$print(\"map_counter = \" + profile$map_counter);        \
  profile$print(\"hit_counter = \" + profile$hit_counter);        \
  profile$print(\"miss_counter = \" + profile$miss_counter);      \
  profile$print(\"\");                                            \
                                                                  \
  for (var i=0; i<profile$maps.length; i++)                       \
  { var map = profile$maps[i];                                    \
    profile$print(\"map[\" + map.id + \"] = \" + profile$map_to_string(map)); \
  }                                                               \
  profile$print(\"\");                                            \
                                                                  \
  profile$print(\"FUNCTION PROFILE:\");                           \
  profile$print(\"\");                                            \
  for (var j=0; j<fn_descrs.length; j++)                          \
    { var descr = fn_descrs[j];                                   \
      profile$print(descr.calls + \" \" + descr.fn + \" \" + \"(\" + descr.loc + \":)\"); \
      for (var i=0; i<descr.args.length; i++)                     \
        {                                                         \
          profile$print(\"     arg[\" + i + \"] = \" +            \
                profile$abstype_to_string(descr.args[i]));        \
        }                                                         \
      profile$print(\"     result = \" +                          \
            profile$abstype_to_string(descr.result));             \
      profile$print(\"\");                                        \
    }                                                             \
                                                                  \
  profile$print(\"PROPERTY ACCESS RECEIVER PROFILE:\");           \
  profile$print(\"\");                                            \
  for (var j=0; j<prop_access_descrs.length; j++)                 \
    { var descr = prop_access_descrs[j];                          \
      profile$print(descr.accesses + \" \" + \"(\" + descr.loc + \":) \" + profile$abstype_to_string(descr.abstype)); \
      profile$print(\"\");                                        \
    }                                                             \
                                                                  \
  return profile$output.get_output_string();                      \
}                                                                 \
                                                                  \
function profile$nest(loc, fn, enter)                             \
{ var level;                                                      \
  if (enter)                                                      \
    { level = ++profile$nesting;                                  \
      profile$stack[level] = loc;                                 \
    }                                                             \
  else                                                            \
    level = profile$nesting--;                                    \
  if (!profile$trace) return;                                     \
  var prefix = \"\";                                              \
  if (level > 9) { prefix = \"|[\"+level+\"] \"; level = 8; }     \
  while (level-- > 0) prefix = \"|  \" + prefix;                  \
  profile$print(prefix+(enter?\"((\":\"))\")+\" \"+loc+\": \"+fn);\
}                                                                 \
                                                                  \
function profile$enter(loc, fn, args)                             \
{ profile$nest(loc, fn, true);                                    \
  profile$enter_tp(fn, args);                                     \
}                                                                 \
                                                                  \
function profile$return0(loc)                                     \
{ profile$return_tp(undefined);                                   \
  profile$nest(loc, false);                                       \
}                                                                 \
                                                                  \
function profile$return1(loc, result)                             \
{ profile$return_tp(result);                                      \
  profile$nest(loc, false);                                       \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$NewExpr_hook(loc, val)                           \
{ profile$new_counter++;                                          \
  return val;                                                     \
}                                                                 \
                                                                  \
function profile$CallExpr_hook(loc, val)                          \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$FunctionExpr_hook(loc, val)                      \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$Literal_hook(loc, val)                           \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$ArrayLiteral_hook(loc, val)                      \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$RegExpLiteral_hook(loc, val)                     \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$ObjectLiteral_hook(loc, val)                     \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$Ref_hook(loc, val)                               \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$This_hook(loc, val)                              \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$access_prop_tp(loc, obj)                         \
{ var descr = profile$prop_access_abstypes[loc];                  \
  if (descr === undefined)                                        \
    { descr = { loc: loc,                                         \
                accesses: 0,                                      \
                abstype: undefined                                \
              };                                                  \
      profile$prop_access_abstypes[loc] = descr;                  \
    }                                                             \
  descr.accesses++;                                               \
  descr.abstype = profile$abstype_add(descr.abstype, obj);        \
}                                                                 \
                                                                  \
function profile$fetch_prop(loc, obj, prop)                       \
{ if (typeof obj !== \"object\") return;                          \
  profile$fetch_counter++;                                        \
  profile$access_prop_tp(loc, obj);                               \
  var info = profile$object_info(obj);                            \
  if (profile$map_cache.hasOwnProperty(loc) &&                    \
      profile$map_cache[loc].id === info.map.id)                  \
    profile$hit_counter++;                                        \
  else                                                            \
  { profile$miss_counter++;                                       \
    profile$map_cache[loc] = info.map;                            \
  }                                                               \
}                                                                 \
                                                                  \
function profile$store_prop(loc, obj, prop)                       \
{ if (typeof obj !== \"object\") return;                          \
  profile$store_counter++;                                        \
  profile$access_prop_tp(loc, obj);                               \
  var info = profile$object_info(obj);                            \
  info.map = profile$map_extend(info.map, prop);                  \
  if (profile$map_cache.hasOwnProperty(loc) &&                    \
      profile$map_cache[loc].id === info.map.id)                  \
    profile$hit_counter++;                                        \
  else                                                            \
  { profile$miss_counter++;                                       \
    profile$map_cache[loc] = info.map;                            \
  }                                                               \
}                                                                 \
                                                                  \
function profile$get_prop(loc, obj, prop)                         \
{ profile$fetch_prop(loc, obj, prop);                             \
  return obj[prop];                                               \
}                                                                 \
                                                                  \
function profile$put_prop_preinc(loc, obj, prop)                  \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = ++obj[prop];                                       \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop_predec(loc, obj, prop)                  \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = --obj[prop];                                       \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop_postinc(loc, obj, prop)                 \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = obj[prop]++;                                       \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop_postdec(loc, obj, prop)                 \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = obj[prop]--;                                       \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop(loc, obj, prop, val)                    \
{ profile$store_prop(loc, obj, prop);                             \
  var result = obj[prop] = val;                                   \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop_add(loc, obj, prop, val)                \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = obj[prop] += val;                                  \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop_sub(loc, obj, prop, val)                \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = obj[prop] -= val;                                  \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop_mul(loc, obj, prop, val)                \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = obj[prop] *= val;                                  \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$put_prop_div(loc, obj, prop, val)                \
{ profile$fetch_prop(loc, obj, prop);                             \
  profile$store_prop(loc, obj, prop);                             \
  var result = obj[prop] /= val;                                  \
  return result;                                                  \
}                                                                 \
                                                                  \
function profile$set_var_preinc(loc, val)                         \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var_predec(loc, val)                         \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var_postinc(loc, val)                        \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var_postdec(loc, val)                        \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var(loc, val)                                \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var_add(loc, val)                            \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var_sub(loc, val)                            \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var_mul(loc, val)                            \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$set_var_div(loc, val)                            \
{ return val;                                                     \
}                                                                 \
                                                                  \
function profile$call_prop(loc, obj, prop)                        \
{ profile$fetch_prop(loc, obj, prop);                             \
  var f = obj[prop];                                              \
  var args = [];                                                  \
  for (var i=3; i<arguments.length; i++)                          \
    args.push(arguments[i]);                                      \
  return f.apply(obj, args);                                      \
}                                                                 \
                                                                  \
function profile$end_of_program()                                 \
{ if (false)                                                      \
    print(profile$report());                                      \
  else                                                            \
    setTimeout(\"document.write('<pre>'+profile$report()+'</pre>');\", 5000); \
}                                                                 \
";

//-----------------------------------------------------------------------------

// Pass 2.
//
// Identifies functions that use the "eval" and "arguments" symbols.
//
// NOTE: this is not done through free variables for two reasons:
// 1. Free variables can come from nested sub-functions.
// 2. Free variable resolution and must be done after pass 2

function ast_pass2_ctx(ast)
{
    this.ast = ast;
}

ast_pass2_ctx.prototype.walk_statement = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else
    {
        return ast_walk_statement(ast, this);
    }
};

ast_pass2_ctx.prototype.walk_expr = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }

    // Function expression
    else if (ast instanceof FunctionExpr)
    {
        // Create a new context to traverse the function body
        var new_ctx = new ast_pass2_ctx(ast);

        // Traverse the function body
        ast_walk_statements(ast.body, new_ctx);

        // Return the updated function
        return ast;
    }

    // Variable reference
    else if (ast instanceof Ref)
    {
        // TODO: eliminate when ids are fixed
        var symName = ast.id.toString();

        if (symName === "arguments")
            this.ast.usesArguments = true;

        else if (symName === "eval")
            this.ast.usesEval = true;

        return ast;
    }

    else
    {
        return ast_walk_expr(ast, this);
    }
};

function ast_pass2(ast)
{
    var ctx = new ast_pass2_ctx(ast);
    ctx.walk_statement(ast);
}

//-----------------------------------------------------------------------------

// Pass 3.
//
// Transforms an AST into which, when the arguments name occurs free, references
// to formal parameters become references to an alias of the arguments object

function ast_pass3_ctx(varMap)
{
    // Map function parameters names/ids to pairs argObj/index or nothing
    this.varMap = varMap;
}

ast_pass3_ctx.prototype.walk_statement = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else
    {
        return ast_walk_statement(ast, this);
    }
};

ast_pass3_ctx.prototype.walk_expr = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }

    // Function expression
    else if (ast instanceof FunctionExpr)
    {
        // Create a copy of the
        var newVarMap = this.varMap.copy();

        // If the arguments object may be used
        if (ast.usesArguments)
        {
            // Get a free id for the arguments object alias
            var argsObjId = get_free_id("argsObj", ast.loc);

            // Create the arguments object alias in the function body
            ast.body.unshift(
                new VariableStatement(
                    ast.loc,
                    [
                        new Decl(
                            ast.loc,
                            argsObjId,
                            new Ref(ast.loc,
                                    new Token(IDENT_CAT, "arguments", ast.loc))
                        )
                    ]
                )
            );

            // For each function parameter
            for (var i = 0; i < ast.params.length; ++i)
            {
                var symName = ast.params[i].toString();
                newVarMap.set(symName, { id:argsObjId, index:i });
            }
        }
        else
        {
            // The parameters of the function are not associated
            for (var i = 0; i < ast.params.length; ++i)
            {
                var symName = ast.params[i].toString();
                newVarMap.rem(symName);
            }
        }

        // Create a new context to traverse the function body
        var new_ctx = new ast_pass3_ctx(newVarMap);

        // Traverse and update the function body
        ast.body = ast_walk_statements(ast.body, new_ctx);

        // Return the updated function
        return ast;
    }

    // Variable reference
    else if (ast instanceof Ref)
    {
        // TODO: eliminate when ids are fixed
        var symName = ast.id.toString();

        // Try to get the association for this symbol
        var assoc = this.varMap.get(symName);

        // If there is an association for this symbol
        if (assoc !== HashMap.NOT_FOUND)
        {
            // Replace the parameter reference by an argument object indexing
            return new OpExpr(
                ast.loc,
                "x [ y ]",
                [
                    new Ref(ast.loc, assoc.id),
                    new Literal(ast.loc, assoc.index)
                ]
            );
        }
        else
        {
            return ast;
        }
    }

    else
    {
        return ast_walk_expr(ast, this);
    }
};

function ast_pass3(ast)
{
    var ctx = new ast_pass3_ctx(new HashMap());
    ctx.walk_statement(ast);
}

//-----------------------------------------------------------------------------

// Pass 4.
//
// Transforms an AST into a simpler AST.
//
//   - transform "VariableStatement" into assignment
//   - transform "ForVarStatement" into "ForStatement"
//   - transform "ForVarInStatement" into "ForInStatement"
//   - flattening of nested block statements

function ast_pass4_ctx(vars, scope)
{
    this.vars = vars;
    this.scope = scope;
}

function ast_pass4_empty_ctx(scope)
{
    return new ast_pass4_ctx({}, scope);
}

function ast_Variable(id, is_param, scope)
{
    this.id       = id;
    this.is_param = is_param;
    this.scope    = scope;
}

ast_Variable.prototype.toString = function ()
{
    return this.id.toString();
};

ast_pass4_ctx.prototype.function_ctx = function (ast)
{
    var new_ctx = ast_pass4_empty_ctx(ast);
    ast.params.forEach(function (param, i, self)
                       {
                           new_ctx.add_variable(param, true);
                       });
    return new_ctx;
};

ast_pass4_ctx.prototype.catch_ctx = function (ast)
{
    var new_ctx = ast_pass4_empty_ctx(ast);
    new_ctx.add_variable(ast.id, true);
/*
    [new Decl(IDENT.loc.join(Initializer.loc),
                     IDENT,
                     Initializer)]
*/
    return new_ctx;
};

ast_pass4_ctx.prototype.add_variable = function (id, is_param)
{
    var id_str = id.value;
    var v = this.vars[id_str];
    if (typeof v === "undefined")
    {
        v = new ast_Variable(id, is_param, this.scope);
        this.vars[id_str] = v;
    }
    return v;
};

ast_pass4_ctx.prototype.walk_statement = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        var new_ctx = ast_pass4_empty_ctx(ast);
        ast.block = new_ctx.walk_statement(ast.block);
        ast.vars = new_ctx.vars;
        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        this.add_variable(ast.id, false);
        ast.funct = this.walk_expr(ast.funct);
        return ast;
    }
    else if (ast instanceof VariableStatement)
    {
        var ctx = this;
        var accum = [];
        ast.decls.forEach(function (decl, i, self)
                          {
                              ctx.add_variable(decl.id, false);
                              if (decl.initializer !== null)
                              {
                                  decl.initializer = ctx.walk_expr(decl.initializer);
                                  accum.push(new ExprStatement(
                                               decl.loc,
                                               new OpExpr(decl.loc,
                                                          op2_table[EQUAL_CAT],
                                                          [new Ref(decl.id.loc,
                                                                   decl.id),
                                                           decl.initializer])));
                              }
                          });
        if (accum.length === 1)
            return accum[0];
        else
            return new BlockStatement(ast.loc,
                                      accum);
    }
    else if (ast instanceof ForVarStatement)
    {
        var accum = null;
        for (var i=ast.decls.length-1; i>=0; i--)
        {
            var decl = ast.decls[i];
            this.add_variable(decl.id, false);
            if (decl.initializer !== null)
            {
                decl.initializer = this.walk_expr(decl.initializer);
                var init = new OpExpr(decl.loc,
                                      op2_table[EQUAL_CAT],
                                      [new Ref(decl.id.loc,
                                               decl.id),
                                       decl.initializer]);
                if (accum === null)
                    accum = init;
                else
                    accum = new OpExpr(decl.loc,
                                       op2_table[COMMA_CAT],
                                       [init, accum]);
            }
        }
        ast.expr2 = this.walk_expr(ast.expr2);
        ast.expr3 = this.walk_expr(ast.expr3);
        ast.statement = this.walk_statement(ast.statement);
        return new ForStatement(ast.loc,
                                accum,
                                ast.expr2,
                                ast.expr3,
                                ast.statement);
    }
    else if (ast instanceof ForVarInStatement)
    {
        this.add_variable(ast.id, false);
        var initializer = this.walk_expr(ast.initializer);
        var set_expr = this.walk_expr(ast.set_expr);
        var statement = this.walk_statement(ast.statement);
        var for_stat = new ForInStatement(ast.loc,
                                          new Ref(ast.id.loc,
                                                  ast.id),
                                          set_expr,
                                          statement);
        if (initializer === null)
            return for_stat;
        else
            return new BlockStatement(ast.loc,
                                      [new ExprStatement(
                                         initializer.loc,
                                         new OpExpr(ast.loc,
                                                    op2_table[EQUAL_CAT],
                                                    [new Ref(ast.id.loc,
                                                             ast.id),
                                                     initializer])),
                                       for_stat]);
    }
    else if (ast instanceof CatchPart)
    {
        var new_ctx = this.catch_ctx(ast);
        ast.statement = new_ctx.walk_statement(ast.statement);
        ast.vars = new_ctx.vars;
        ast.parent = this.scope;
        return ast;
    }
    else
        return ast_walk_statement(ast, this);
};

ast_pass4_ctx.prototype.walk_expr = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof FunctionExpr)
    {
        var new_ctx = this.function_ctx(ast);
        if (ast.id !== null)
            new_ctx.add_variable(ast.id, false);
        ast.body = new_ctx.walk_statements(ast.body);
        ast.vars = new_ctx.vars;
        ast.parent = this.scope;
        return ast;
    }
    else
    {
        return ast_walk_expr(ast, this);
    }
};

ast_pass4_ctx.prototype.walk_statements = function (asts)
{
    var ctx = this;
    var accum = [];
    asts.forEach(function (ast, i, asts)
                 {
                     var a = ctx.walk_statement(ast);
                     if (a instanceof BlockStatement)
                         accum.push(a.statements); // merge embedded blocks
                     else
                         accum.push([a]);
                 });
    return Array.prototype.concat.apply([], accum);
};

function ast_pass4(ast)
{
    var ctx = ast_pass4_empty_ctx(null);
    ctx.walk_statement(ast);
}

//-----------------------------------------------------------------------------

// Pass 5.
//
// Transforms an AST into an AST in which
//
//   - variables are resolved according to their scope
//   - a map of escaping variables is added to functions
//   - a map of closure-provided variables is added to functions
//   - a list of all nested functions is added to functions

function ast_pass5_ctx(scope)
{
    this.scope = scope;
}

ast_pass5_ctx.prototype.function_ctx = function (ast)
{
    var new_ctx = new ast_pass5_ctx(ast);

    ast.params.forEach(
        function (param, i, self)
        {
            param[i] = new_ctx.resolve_variable(param);
        }
    );

    return new_ctx;
};

ast_pass5_ctx.prototype.catch_ctx = function (ast)
{
    var new_ctx = new ast_pass5_ctx(ast);

    ast.id = new_ctx.resolve_variable(ast.id);

    return new_ctx;
};

ast_pass5_ctx.prototype.resolve_variable = function (id)
{
    // Where is this id declared???

    function resolve(scope)
    {
        var id_str = id.value;

        // If the id is declared in the current scope
        var v = scope.vars[id_str];
        if (typeof v !== "undefined")
            return v;

        // If the id is a free variable of the current scope
        v = scope.free_vars[id_str];
        if (typeof v !== "undefined")
            return v;

        // If the current scope is global
        if (scope instanceof Program)
            v = new ast_Variable(id, false, scope);
        else
            v = resolve(scope.parent);

        if (!(scope instanceof CatchPart))
        {
            // This variable is declared in an enclosing scope, add it to the free variable list of the current scope
            scope.free_vars[id_str] = v;

            // If this is not a global variable, add it to the closure variable list of the current scope
            // TODO: the computation of clos_vars should be done elsewhere as it is not related to the semantics
            if (!(v.scope instanceof Program))
                scope.clos_vars[id_str] = v;
        }

        // If the variable's scope is a function and does not match the current scope, mark
        // the variable as escaping in its scope of origin
        if (v.scope instanceof FunctionExpr && v.scope !== scope)
            v.scope.esc_vars[id_str] = v;

        return v;
    }

    return resolve(this.scope);
};

ast_pass5_ctx.prototype.walk_statement = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof Program)
    {
        ast.free_vars = {};
        ast.funcs = [];

        var new_ctx = new ast_pass5_ctx(ast);
        ast.block = new_ctx.walk_statement(ast.block);
        return ast;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        // Set the current function declaration
        this.func_decl = ast;

        ast.id = this.resolve_variable(ast.id);
        ast.funct = this.walk_expr(ast.funct);

        return ast;
    }
    else if (ast instanceof CatchPart)
    {
        ast.free_vars = {};

        // TODO
        //var new_ctx = this.catch_ctx(ast);
        var new_ctx = this;

        ast.statement = new_ctx.walk_statement(ast.statement);
        return ast;
    }
    else
    {
        return ast_walk_statement(ast, this);
    }
};

ast_pass5_ctx.prototype.walk_expr = function (ast)
{
    if (ast === null)
    {
        // no transformation
        return ast;
    }
    else if (ast instanceof FunctionExpr)
    {
        ast.free_vars = {};
        ast.clos_vars = {};
        ast.esc_vars = {};
        ast.funcs = [];

        // Add this function to the scope's nested function list
        // If this function is part of a function declaration, add the declaration instead
        // TODO: fix this code which does not work when the scope is for a CatchPart
        if (this.func_decl !== undefined && this.func_decl.funct === ast)
            this.scope.funcs.push(this.func_decl);
        else
            this.scope.funcs.push(ast);

        var new_ctx = this.function_ctx(ast);

        if (ast.id !== null)
            ast.id = new_ctx.resolve_variable(ast.id);

        ast.body = ast_walk_statements(ast.body, new_ctx);

        return ast;
    }
    else if (ast instanceof Ref)
    {
        ast.id = this.resolve_variable(ast.id);
        return ast;
    }
    else
    {
        return ast_walk_expr(ast, this);
    }
};

function ast_pass5(ast)
{
    var ctx = new ast_pass5_ctx(ast);
    ctx.walk_statement(ast);
}

//-----------------------------------------------------------------------------

function ast_normalize(ast, debug)
{
    if (debug)
        ast_pass1(ast);
    else
        ast_pass2(ast);
    ast_pass3(ast);
    ast_pass4(ast);
    ast_pass5(ast);

    return ast;
}

//=============================================================================
