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

// File: "pp.js", Time-stamp: <2011-03-15 13:47:52 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// AST pretty printer.

function pp(ast)
{
    pp_indent(ast, 0);
}

function pp_indent(ast, indent)
{
    if (ast === null)
        print(pp_prefix(indent) + "null");
    else if (ast instanceof Program)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Program");

        if (ast.vars !== null)
        {
            for (var v in ast.vars)
                pp_id(ast.vars[v], indent, "var");
        }

        if (ast.free_vars !== null)
        {
            for (var v in ast.free_vars)
                pp_id(ast.free_vars[v], indent, "free_var");
        }

        if (ast.funcs !== null)
        {
            for (var i in ast.funcs)
            {
                if (ast.funcs[i].id !== null)
                    pp_id(ast.funcs[i].id, indent, "func");
                else
                    print(pp_prefix(indent) + "|-func anonymous");
            }
        }

        pp_asts(indent, "block", [ast.block]);
    }
    else if (ast instanceof BlockStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "BlockStatement");
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof VariableStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "VariableStatement");
        pp_asts(indent, "decls", ast.decls);
    }
    else if (ast instanceof Decl)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Decl");
        pp_id(ast.id, indent, "id");
        pp_asts(indent, "initializer", [ast.initializer]);
    }
    else if (ast instanceof ConstStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ConstStatement");
        pp_asts(indent, "decls", ast.decls);
    }
    else if (ast instanceof FunctionDeclaration)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "FunctionDeclaration");
        if (ast.id !== null)
            pp_id(ast.id, indent, "id");
        pp_asts(indent, "funct", [ast.funct]);
    }
    else if (ast instanceof ExprStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ExprStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof IfStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "IfStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof DoWhileStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "DoWhileStatement");
        pp_asts(indent, "statement", [ast.statement]);
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof WhileStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "WhileStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForStatement");
        pp_asts(indent, "expr1", [ast.expr1]);
        pp_asts(indent, "expr2", [ast.expr2]);
        pp_asts(indent, "expr3", [ast.expr3]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForVarStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForVarStatement");
        pp_asts(indent, "decls", ast.decls);
        pp_asts(indent, "expr2", [ast.expr2]);
        pp_asts(indent, "expr3", [ast.expr3]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForInStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForInStatement");
        pp_asts(indent, "lhs_expr", [ast.lhs_expr]);
        pp_asts(indent, "set_expr", [ast.set_expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForVarInStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ForVarInStatement");
        pp_id(ast.id, indent, "id");
        pp_asts(indent, "initializer", [ast.initializer]);
        pp_asts(indent, "set_expr", [ast.set_expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ContinueStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ContinueStatement");
        if (ast.label !== null)
            print(pp_prefix(indent) + "|-label= " + ast.label.toString());
    }
    else if (ast instanceof BreakStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "BreakStatement");
        if (ast.label !== null)
            print(pp_prefix(indent) + "|-label= " + ast.label.toString());
    }
    else if (ast instanceof ReturnStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ReturnStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof WithStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "WithStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof SwitchStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "SwitchStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "clauses", ast.clauses);
    }
    else if (ast instanceof CaseClause)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "CaseClause");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof LabelledStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "LabelledStatement");
        print(pp_prefix(indent) + "|-label= " + ast.label.toString());
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ThrowStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ThrowStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof TryStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "TryStatement");
        pp_asts(indent, "statement", [ast.statement]);
        pp_asts(indent, "catch_part", [ast.catch_part]);
        pp_asts(indent, "finally_part", [ast.finally_part]);
    }
    else if (ast instanceof CatchPart)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "CatchPart");

        // TODO: temporary fix until catch scope issues are fixed
        //pp_id(ast.id, indent, "id");

        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof DebuggerStatement)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "DebuggerStatement");
    }
    else if (ast instanceof OpExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "OpExpr");
        print(pp_prefix(indent) + "|-op= \"" + ast.op + "\"");
        pp_asts(indent, "exprs", ast.exprs);
    }
    else if (ast instanceof NewExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "NewExpr");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "args", ast.args);
    }
    else if (ast instanceof CallExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "CallExpr");
        pp_asts(indent, "fn", [ast.fn]);
        pp_asts(indent, "args", ast.args);
    }
    else if (ast instanceof FunctionExpr)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "FunctionExpr");

        if (ast.id !== null)
            pp_id(ast.id, indent, "id");

        for (var p in ast.params)
            pp_loc(ast.params[p].loc, pp_prefix(indent) + "|-param= " + ast.params[p].toString());

        for (var a in ast.annotations)
            pp_loc(ast.annotations[a].loc, pp_prefix(indent) + "|-annotation= \"" + ast.annotations[a].value + "\"");

        if (ast.vars !== null)
        {
            for (var v in ast.vars)
                pp_id(ast.vars[v], indent, "var");
        }

        if (ast.free_vars !== null)
        {
            for (var v in ast.free_vars)
                pp_id(ast.free_vars[v], indent, "free_var");
        }

        if (ast.clos_vars !== null)
        {
            for (var v in ast.clos_vars)
                pp_id(ast.clos_vars[v], indent, "clos_var");
        }

        if (ast.esc_vars !== null)
        {
            for (var v in ast.esc_vars)
                pp_id(ast.esc_vars[v], indent, "esc_var");
        }

        if (ast.funcs !== null)
        {
            for (var i in ast.funcs)
            {
                if (ast.funcs[i].id !== null)
                    pp_id(ast.funcs[i].id, indent, "func");
                else
                    print(pp_prefix(indent) + "|-func anonymous");
            }
        }

        pp_asts(indent, "body", ast.body);
    }
    else if (ast instanceof Literal)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Literal");
        print(pp_prefix(indent) + "|-value= " + ast.value);
    }
    else if (ast instanceof ArrayLiteral)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ArrayLiteral");
        pp_asts(indent, "exprs", ast.exprs);
    }
    else if (ast instanceof ObjectLiteral)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "ObjectLiteral");
        pp_asts(indent, "properties", ast.properties);
    }
    else if (ast instanceof Property)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Property");
        pp_asts(indent, "name", [ast.name]);
        pp_asts(indent, "value", [ast.value]);
    }
    else if (ast instanceof Ref)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "Ref");
        pp_id(ast.id, indent, "id");
    }
    else if (ast instanceof This)
    {
        pp_loc(ast.loc, pp_prefix(indent) + "This");
    }
    else
        print(pp_prefix(indent) + "UNKNOWN AST");
}

function pp_id(id, indent, label)
{
    var kind = "[unknown]";

    if (id.scope instanceof Program)
        kind = "[global]";
    else if (id.scope instanceof FunctionExpr)
        kind = "[local]";
    else if (id.scope instanceof CatchPart)
        kind = "[catch]";

    pp_loc(id.scope.loc, pp_prefix(indent) + "|-" + label + "= " + id.toString() + " " + kind);
}

function pp_loc(loc, line)
{
    print(line + pp_spaces(48-line.length) + "  (" + loc.to_string() + ":)");
}

function pp_asts(indent, label, asts)
{
    if (asts !== null)
    {
        print(pp_prefix(indent) + "|-" + label + "=");
        for (var i=0; i<asts.length; i++)
            pp_indent(asts[i], indent+1);
    }
}

function pp_prefix(indent)
{
    if (indent > 0)
        return "|   " + pp_prefix(indent-1);
    else
        return "";
}

function pp_spaces(n)
{
    if (n > 0)
        return " " + pp_spaces(n-1);
    else
        return "";
}

//-----------------------------------------------------------------------------

// JavaScript pretty-printing.

function js_pp(ast)
{
    print(js_to_string(ast));
}

function js_to_string(ast)
{
    var ctx = new js_pp_ctx(new String_output_port(""), 0);
    ast_to_js(ast, ctx);
    return ctx.port.get_output_string();
}

function js_pp_ctx(port, indent)
{
    this.port = port;
    this.indent = indent;
}

function js_unparse_string(str)
{
    var port = new String_output_port("");

    port.write_string("\"");

    for (var i=0; i<str.length; i++)
    {
        var c = str.charAt(i);
        switch (c)
        {
            case "\\": c = "\\\\"; break;
            case "\"": c = "\\\""; break;
            case "\0": c = "\\0"; break;
            case "\b": c = "\\b"; break;
            case "\t": c = "\\t"; break;
            case "\n": c = "\\n"; break;
            case "\v": c = "\\v"; break;
            case "\f": c = "\\f"; break;
            case "\r": c = "\\r"; break;
        }
        port.write_string(c);
    }

    port.write_string("\"");

    return port.get_output_string();
}

function ast_to_js(ast, ctx)
{
    if (ast === null)
        error("null ast");
    else if (ast instanceof Program)
    {
        for (var v in ast.vars)
            js_var(js_id_to_js(v), ctx);
        ast_to_js(ast.block, ctx);
    }
    else if (ast instanceof BlockStatement)
    {
        for (var i=0; i<ast.statements.length; i++)
            ast_to_js(ast.statements[i], ctx);
    }
    // else if (ast instanceof VariableStatement)
    // { impossible due to pass1 transformations }
    else if (ast instanceof ConstStatement)
    {
        // TODO
        pp(ast);
        error("ConstStatement not implemented");
    }
    else if (ast instanceof FunctionDeclaration)
    {
        function_to_js(ast.funct, ast.id, ctx);
        js_out("\n", ctx);
    }
    else if (ast instanceof ExprStatement)
    {
        js_indent(ctx);
        ast_to_js(ast.expr, ctx);
        js_out(";\n", ctx);
    }
    else if (ast instanceof IfStatement)
    {
        js_indent(ctx);
        js_out("if (", ctx);
        ast_to_js(ast.expr, ctx);
        js_out(")\n", ctx);

        js_indent(ctx);
        js_out("{\n", ctx);
        js_indent_begin(ctx);

        ast_to_js(ast.statements[0], ctx);

        js_indent_end(ctx);
        js_indent(ctx);
        js_out("}\n", ctx);

        if (ast.statements.length === 2)
        {
            js_indent(ctx);
            js_out("else\n", ctx);

            js_indent(ctx);
            js_out("{\n", ctx);
            js_indent_begin(ctx);

            ast_to_js(ast.statements[1], ctx);

            js_indent_end(ctx);
            js_indent(ctx);
            js_out("}\n", ctx);
        }
    }
    else if (ast instanceof DoWhileStatement)
    {
        js_indent(ctx);
        js_out("do\n", ctx);

        js_indent(ctx);
        js_out("{\n", ctx);
        js_indent_begin(ctx);

        ast_to_js(ast.statement, ctx);

        js_indent_end(ctx);
        js_indent(ctx);
        js_out("} while (", ctx);
        ast_to_js(ast.expr, ctx);
        js_out(");\n", ctx);
    }
    else if (ast instanceof WhileStatement)
    {
        js_indent(ctx);
        js_out("while (", ctx);
        ast_to_js(ast.expr, ctx);
        js_out(")\n", ctx);

        js_indent(ctx);
        js_out("{\n", ctx);
        js_indent_begin(ctx);

        ast_to_js(ast.statement, ctx);

        js_indent_end(ctx);
        js_indent(ctx);
        js_out("}\n", ctx);
    }
    else if (ast instanceof ForStatement)
    {
        js_indent(ctx);
        js_out("for (", ctx);
        if (ast.expr1 !== null)
            ast_to_js(ast.expr1, ctx);
        js_out("; ", ctx);
        if (ast.expr2 !== null)
            ast_to_js(ast.expr2, ctx);
        js_out("; ", ctx);
        if (ast.expr3 !== null)
            ast_to_js(ast.expr3, ctx);
        js_out(")\n", ctx);

        js_indent(ctx);
        js_out("{\n", ctx);
        js_indent_begin(ctx);

        ast_to_js(ast.statement, ctx);

        js_indent_end(ctx);
        js_indent(ctx);
        js_out("}\n", ctx);
    }
    // else if (ast instanceof ForVarStatement)
    // { impossible due to pass1 transformations }
    else if (ast instanceof ForInStatement)
    {
        js_indent(ctx);
        js_out("for (", ctx);
        ast_to_js(ast.lhs_expr, ctx);
        js_out(" in ", ctx);
        ast_to_js(ast.set_expr, ctx);
        js_out(")\n", ctx);

        js_indent(ctx);
        js_out("{\n", ctx);
        js_indent_begin(ctx);

        ast_to_js(ast.statement, ctx);

        js_indent_end(ctx);
        js_indent(ctx);
        js_out("}\n", ctx);
    }
    // else if (ast instanceof ForVarInStatement)
    // { impossible due to pass1 transformations }
    else if (ast instanceof ContinueStatement)
    {
        js_indent(ctx);
        js_out("continue", ctx);
        if (ast.label !== null)
            js_out(" " + ast.label.toString(), ctx);
        js_out(";\n", ctx);
    }
    else if (ast instanceof BreakStatement)
    {
        js_indent(ctx);
        js_out("break", ctx);
        if (ast.label !== null)
            js_out(" " + ast.label.toString(), ctx);
        js_out(";\n", ctx);
    }
    else if (ast instanceof ReturnStatement)
    {
        js_indent(ctx);
        js_out("return", ctx);
        if (ast.expr !== null)
        {
            js_out(" ", ctx);
            ast_to_js(ast.expr, ctx);
        }
        js_out(";\n", ctx);
    }
    else if (ast instanceof WithStatement)
    {
        // TODO
        pp(ast);
        error("WithStatement not implemented");
        /*
        pp_loc(ast.loc, pp_prefix(indent) + "WithStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statement", [ast.statement]);
        */
    }
    else if (ast instanceof SwitchStatement)
    {
        js_indent(ctx);
        js_out("switch (", ctx);
        ast_to_js(ast.expr, ctx);
        js_out(")\n", ctx);

        js_indent(ctx);
        js_out("{\n", ctx);
        js_indent_begin(ctx);

        for (var i=0; i<ast.clauses.length; i++)
        {
            var clause_i = ast.clauses[i];
            js_indent(ctx);
            if (clause_i.expr === null)
                js_out("default:\n", ctx);
            else
            {
                js_out("case ", ctx);
                ast_to_js(clause_i.expr, ctx);
                js_out(":\n", ctx);
            }
            js_indent(ctx);
            js_out("{\n", ctx);
            js_indent_begin(ctx);

            for (var j=0; j<clause_i.statements.length; j++)
                ast_to_js(clause_i.statements[j], ctx);

            js_indent_end(ctx);
            js_indent(ctx);
            js_out("}\n", ctx);
        }

        js_indent_end(ctx);

        js_indent(ctx);
        js_out("}\n", ctx);
    }
    // else if (ast instanceof CaseClause)
    // { impossible due to handling of SwitchStatement }
    else if (ast instanceof LabelledStatement)
    {
        js_indent(ctx);
        js_out(ast.label.toString() + ":\n", ctx);

        js_indent_begin(ctx);
        ast_to_js(ast.statement, ctx);
        js_indent_end(ctx);
    }
    else if (ast instanceof ThrowStatement)
    {
        js_indent(ctx);
        js_out("throw ", ctx);
        ast_to_js(ast.expr, ctx);
        js_out(";\n", ctx);
    }
    else if (ast instanceof TryStatement)
    {
        js_indent(ctx);
        js_out("try\n", ctx);

        js_indent(ctx);
        js_out("{\n", ctx);
        js_indent_begin(ctx);

        ast_to_js(ast.statement, ctx);

        js_indent_end(ctx);
        js_indent(ctx);
        js_out("}\n", ctx);

        if (ast.catch_part !== null)
        {
            js_indent(ctx);
            js_out("catch (", ctx);
            js_out(js_id_to_js(ast.catch_part.id.toString()), ctx);
            js_out(")\n", ctx);

            js_indent(ctx);
            js_out("{\n", ctx);
            js_indent_begin(ctx);

            ast_to_js(ast.catch_part.statement, ctx);

            js_indent_end(ctx);
            js_indent(ctx);
            js_out("}\n", ctx);
        }

        if (ast.finally_part !== null)
        {
            js_indent(ctx);
            js_out("finally\n", ctx);

            js_indent(ctx);
            js_out("{\n", ctx);
            js_indent_begin(ctx);

            ast_to_js(ast.finally_part, ctx);

            js_indent_end(ctx);
            js_indent(ctx);
            js_out("}\n", ctx);
        }
    }
    // else if (ast instanceof CatchPart)
    // { impossible due to handling of TryStatement }
    else if (ast instanceof DebuggerStatement)
    {
        js_indent(ctx);
        js_out("debugger\n", ctx);
    }
    else if (ast instanceof OpExpr)
    {
        js_out("(", ctx);
        if (ast.op === "x ? y : z")
        {
            ast_to_js(ast.exprs[0], ctx);
            js_out("?", ctx);
            ast_to_js(ast.exprs[1], ctx);
            js_out(":", ctx);
            ast_to_js(ast.exprs[2], ctx);
        }
        else if (ast.op === "x [ y ]")
        {
            ast_to_js(ast.exprs[0], ctx);
            js_out("[", ctx);
            ast_to_js(ast.exprs[1], ctx);
            js_out("]", ctx);
        }
        else
        {
            var len = ast.op.length;
            var last = ast.op.charAt(len-1);

            if (last === "y")
            {
                ast_to_js(ast.exprs[0], ctx);
                js_out(ast.op.substring(1, len-1), ctx);
                ast_to_js(ast.exprs[1], ctx);
            }
            else if (last === "x")
            {
                js_out(ast.op.substring(0, len-1), ctx);
                ast_to_js(ast.exprs[0], ctx);
            }
            else if (ast.op.charAt(0) === "x")
            {
                ast_to_js(ast.exprs[0], ctx);
                js_out(ast.op.substring(1, len), ctx);
            }
            else
                error("unknown op " + ast.op);
        }
        js_out(")", ctx);
    }
    else if (ast instanceof NewExpr)
    {
        js_out("new ", ctx);
        ast_to_js(ast.expr, ctx);
        js_out("(", ctx);
        var sep = "";
        for (var i=0; i<ast.args.length; i++)
        {
            js_out(sep, ctx);
            ast_to_js(ast.args[i], ctx);
            sep = ", ";
        }
        js_out(")", ctx);
    }
    else if (ast instanceof CallExpr)
    {
        ast_to_js(ast.fn, ctx);
        js_out("(", ctx);
        var sep = "";
        for (var i=0; i<ast.args.length; i++)
        {
            js_out(sep, ctx);
            ast_to_js(ast.args[i], ctx);
            sep = ", ";
        }
        js_out(")", ctx);
    }
    else if (ast instanceof FunctionExpr)
    {
        js_out("(", ctx); // FIXME: V8 seems to require extra parentheses at toplevel
        function_to_js(ast, null, ctx);
        js_out(")", ctx); // FIXME: V8 seems to require extra parentheses at toplevel
    }
    else if (ast instanceof Literal)
    {
        var val = ast.value;
        var str;
        if (val === null)
            str = "null";
        else if (typeof val === "string")
            str = js_unparse_string(val);
        else
            str = ast.value.toString();
        js_out(str, ctx);
    }
    else if (ast instanceof ArrayLiteral)
    {
        js_out("[", ctx);
        var sep = "";
        for (var i=0; i<ast.exprs.length; i++)
        {
            js_out(sep, ctx);
            ast_to_js(ast.exprs[i], ctx);
            sep = ", ";
        }
        js_out("]", ctx);
    }
    else if (ast instanceof ObjectLiteral)
    {
        js_out("{", ctx);
        var sep = "";
        for (var i=0; i<ast.properties.length; i++)
        {
            js_out(sep, ctx);
            ast_to_js(ast.properties[i].name, ctx);
            js_out(": ", ctx);
            ast_to_js(ast.properties[i].value, ctx);
            sep = ", ";
        }
        js_out("}", ctx);
    }
    else if (ast instanceof RegExpLiteral)
    {
        js_out("/", ctx);
        js_out(ast.pattern, ctx);
        js_out("/", ctx);
    }
    else if (ast instanceof Ref)
    {
        js_out(js_id_to_js(ast.id.toString()), ctx);
    }
    else if (ast instanceof This)
    {
        js_out("this", ctx);
    }
    else
        error("UNKNOWN AST");
}

function function_to_js(ast, id, ctx)
{
    if (id === null)
        id = ast.id;

    js_out("function ", ctx);

    if (id !== null)
        js_out(js_id_to_js(id.toString()), ctx);

    js_out("(", ctx);

    var sep = "";
    for (var i=0; i<ast.params.length; i++)
    {
        js_out(sep, ctx);
        js_out(js_id_to_js(ast.params[i].toString()), ctx);
        sep = ", ";
    }
    js_out(")\n", ctx);

    js_indent(ctx);
    js_out("{\n", ctx);
    js_indent_begin(ctx);

    for (var a in ast.annotations)
        js_annotation(ast.annotations[a].value, ctx);

    for (var v in ast.vars)
        if (!ast.vars[v].is_param)
            js_var(js_id_to_js(v), ctx);

    for (var i=0; i<ast.body.length; i++)
        ast_to_js(ast.body[i], ctx);

    js_indent_end(ctx);
    js_indent(ctx);
    js_out("}", ctx);
}

function js_id_to_js(id)
{
    return id;
}

function js_out(str, ctx)
{
    ctx.port.write_string(str);
}

function js_indent(ctx)
{
    for (var i=0; i<ctx.indent; i++)
        js_out("    ", ctx);
}

function js_indent_begin(ctx)
{
    ctx.indent++;
}

function js_indent_end(ctx)
{
    ctx.indent--;
}

function js_annotation(annotation, ctx)
{
    js_indent(ctx);
    js_out("\"" + annotation + "\";\n", ctx);
}

function js_var(id, ctx)
{
    js_indent(ctx);
    js_out("var " + id + ";\n", ctx);
}

function js_var_assign(id, ctx)
{
    js_indent(ctx);
    js_out(id + " = ", ctx);
}

//=============================================================================
