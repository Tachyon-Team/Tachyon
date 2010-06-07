//=============================================================================

// File: "scheme.js", Time-stamp: <2010-06-06 20:02:27 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Scheme object constructors.

function isBoolean(obj)
{
    return (typeof obj == "boolean") || (obj instanceof Boolean);
}

function isNumber(obj)
{
    return (typeof obj == "number") || (obj instanceof Number);
}

function isString(obj)
{
    return (typeof obj == "string") || (obj instanceof String);
}

function isProcedure(obj)
{
    return (typeof obj == "function") || (obj instanceof Function);
}

function Symbol(name)
{
    this.name = name;
}

function isSymbol(obj)
{
    return obj instanceof Symbol;
}

var gensym_count = 0;

function gensym(prefix)
{
    return new Symbol(prefix + (gensym_count++).toString());
}

var include = new Symbol("include");
var define  = new Symbol("define");
var begin   = new Symbol("begin");
var lambda  = new Symbol("lambda");
var set_    = new Symbol("set!");
var llet    = new Symbol("let");
var llet_   = new Symbol("let*");
var letrec  = new Symbol("letrec");
var cond    = new Symbol("cond");
var iff     = new Symbol("if");
var quote            = new Symbol("quote");
var quasiquote       = new Symbol("quasiquote");
var unquote          = new Symbol("unquote");
var unquote_splicing = new Symbol("unquote-splicing");

function Null()
{
}

function isNull(obj)
{
    return obj instanceof Null;
}

var empty_list = new Null();

function Pair(car, cdr)
{
    this.car = car;
    this.cdr = cdr;
}

function cons(car, cdr)
{
    return new Pair(car, cdr);
}

function car(obj)
{
    return obj.car;
}

function cdr(obj)
{
    return obj.cdr;
}

function isPair(obj)
{
    return obj instanceof Pair;
}

function make_vector(len)
{
    return new Array(len);
}

function isVector(obj)
{
    return obj instanceof Array;
}

function vector_length(obj)
{
    return obj.length;
}

function vector_ref(obj, i)
{
    return obj[i];
}

function vector_set_(obj, i, x)
{
    obj[i] = x;
}

function vector2list(obj)
{
    var lst = empty_list;
    for (var i=vector_length(obj)-1; i>=0; i--)
        lst = cons(vector_ref(obj, i), lst);
    return lst;
}

//-----------------------------------------------------------------------------

// Scheme object pretty-printing.

function scm_pp(obj)
{
    var port = new String_output_port("");
    scm_pretty_print(obj, port);
    print(port.get_output_string());
}

function scm_pretty_print(obj, port)
{
    scm_generic_write(obj,
                      80,
                      function (str)
                      {
                          port.write_string(str);
                          return true;
                      });
}

function scm_write(obj, port)
{
    scm_generic_write(obj,
                      -1,
                      function (str)
                      {
                          port.write_string(str);
                          return true;
                      });
}

function scm_generic_write(obj, width, output)
{
    function out(str, col)
    {
        if (col >= 0 && output(str))
            return col + str.length;
        else
            return -1;
    }

    function read_macro(expr)
    {
        return isPair(expr) && isPair(cdr(expr)) &&
            isNull(cdr(cdr(expr))) && isSymbol(car(expr)) &&
            (car(expr).name == "quote" ||
             car(expr).name == "quasiquote" ||
             car(expr).name == "unquote" ||
             car(expr).name == "unquote-splicing");
    }

    function read_macro_body(expr)
    {
        return car(cdr(expr));
    }

    function read_macro_prefix(expr)
    {
        if (car(expr).name == "quote")
            return "'";
        else if (car(expr).name == "quasiquote")
            return "`";
        else if (car(expr).name == "unquote")
            return ",";
        else if (car(expr).name == "unquote-splicing")
            return ",@";
        else
            return ""; // should never happen
    }

    function wr(obj, col)
    {
        function wr_undefined(x, col)
        {
            return out("#<undefined>", col);
        }

        function wr_boolean(x, col)
        {
            return out((obj == false) ? "#f" : "#t", col);
        }

        function wr_number(x, col)
        {
            return out(obj.toString(), col);
        }

        function wr_string(x, col)
        {
            col = out("\"", col);
            for (var i=0; i<x.length; i++)
            {
                var c = x.charCodeAt(i);
                if (c == 92 || c == 34) // \ or "
                    col = out(String.fromCharCode(c), out("\\", col));
                else if (c == 10) // LF
                    col = out("\\n", col);
                else
                    col = out(String.fromCharCode(c), col);
            }
            return out("\"", col);
        }

        function wr_procedure(x, col)
        {
            return out("#<procedure>", col);
        }

        function wr_null(x, col)
        {
            return out("#<null>", col);
        }

        function wr_unknown(x, col)
        {
            return out("#<unknown>", col);
        }

        function wr_symbol(sym, col)
        {
            return out(sym.name, col);
        }

        function wr_expr(expr, col)
        {
            if (read_macro(expr))
                return wr(read_macro_body(expr),
                          out(read_macro_prefix(expr), col));
            else
                return wr_lst(expr, col);
        }

        function wr_lst(lst, col)
        {
            if (isPair(lst))
            {
                if (col >= 0)
                    col = wr(car(lst), out("(", col));
                for (;;)
                {
                    lst = cdr(lst);
                    if (col < 0)
                        break;
                    if (isPair(lst))
                        col = wr(car(lst), out(" ", col));
                    else if (isNull(lst))
                    {
                        col = out(")", col);
                        break;
                    }
                    else
                    {
                        col = out(")", wr(lst, out(" . ", col)));
                        break;
                    }
                }
            }
            else
                col = out("()", col);
            return col;
        }

        function wr_vector(vect, col)
        {
            return wr_lst(vector2list(vect), out("#", col));
        }

        if (typeof obj == "undefined")
            return wr_undefined(obj, col);
        else if (obj == null)
            return wr_null(obj , col);
        else if (isBoolean(obj))
            return wr_boolean(obj, col);
        else if (isNumber(obj))
            return wr_number(obj, col);
        else if (isProcedure(obj))
            return wr_procedure(obj, col);
        else if (isString(obj))
            return wr_string(obj, col);
        else if (isNull(obj))
            return wr_lst(obj, col);
        else if (isPair(obj))
            return wr_expr(obj, col);
        else if (isVector(obj))
            return wr_vector(obj, col);
        else if (isSymbol(obj))
            return wr_symbol(obj, col);
        else
            return wr_unknown(obj, col);
    }

    function pp(obj, col)
    {
        function spaces(n, col)
        {
            var col0 = col;
            while (col >= 0 && n-- > 0)
                col = out(" ", col);
            return col;
        }

        function indent(to, col)
        {
            if (col >= 0)
            {
                if (to < col)
                {
                    out("\n", col);
                    col = spaces(to, 0);
                }
                else
                    col = spaces(to - col, col);
            }
            return col;
        }

        function pr(obj, col, extra, pp_pair)
        {
            if (isPair(obj) || isVector(obj))
            {
                // try printing on one line
                var strings = [];
                var left = Math.min(width-col-extra+1, max_expr_width);
                scm_generic_write(obj,
                                  -1,
                                  function (str)
                                  {
                                      strings.push(str);
                                      left -= str.length;
                                      return left > 0;
                                  });
                if (left > 0)
                {
                    for (var i=0; i<strings.length; i++)
                        col = out(strings[i], col);
                }
                else if (isPair(obj))
                    col = pp_pair(obj, col, extra);
                else
                    col = pp_list(vector2list(obj), out("#", col), extra, pp_expr);
            }
            else
                col = wr(obj, col);
            return col;
        }

        function pp_expr(expr, col, extra)
        {
            if (read_macro(expr))
                col = pr(read_macro_body(expr),
                         out(read_macro_prefix(expr), col),
                         extra,
                         pp_expr);
            else
            {
                var head = car(expr);
                if (isSymbol(head))
                {
                    var proc = style(head);
                    if (proc != null)
                        col = proc(expr, col, extra);
                    else if (head.name.length > max_call_head_width)

                        col = pp_general(expr, col, extra, false, null, null, pp_expr);
                    else
                        col = pp_call(expr, col, extra, pp_expr);
                }
                else
                   col = pp_list(expr, col, extra, pp_expr);
            }
            return col;
        }

        function pp_call(expr, col, extra, pp_item)
        {
            // (head item1
            //       item2
            //       item3)
            col = wr(car(expr), out("(", col));
            if (col >= 0)
                col = pp_down(cdr(expr), col, col+1, extra, pp_item);
            return col;
        }

        function pp_list(lst, col, extra, pp_item)
        {
            // (item1
            //  item2
            //  item3)
            col = out("(", col);
            return pp_down(lst, col, col, extra, pp_item);
        }

        function pp_down(lst, col, col2, extra, pp_item)
        {
            for (;;)
            {
                if (col < 0)
                    break;
                if (isPair(lst))
                {
                    var extra2 = isNull(cdr(lst)) ? extra+1 : 0;
                    col = pr(car(lst), indent(col2, col), extra2, pp_item);
                    lst = cdr(lst);
                }
                else if (isNull(lst))
                {
                    col = out(")", col);
                    break;
                }
                else
                {
                    col = out(")",
                              pr(lst,
                                 indent(col2, out(".", indent(col2, col))),
                                 extra+1,
                                 pp_item));
                    break;
                }
            }
            return col;
        }

        function pp_general(expr, col, extra, isnamed, pp1, pp2, pp3)
        {
            function tail1(rest, col1, col2, col3)
            {
                if (pp1 != null && isPair(rest))
                {
                    var val1 = car(rest);
                    var extra = isNull(cdr(rest)) ? extra+1 : 0;
                    return tail2(cdr(rest), col1, pr(val1, indent(col3, col2), extra, pp1), col3);
                }
                else
                    return tail2(rest, col1, col2, col3);
            }

            function tail2(rest, col1, col2, col3)
            {
                if (pp2 != null && isPair(rest))
                {
                    var val1 = car(rest);
                    var extra = isNull(cdr(rest)) ? extra+1 : 0;
                    return tail3(cdr(rest), col1, pr(val1, indent(col3, col2), extra, pp2));
                }
                else
                    return tail3(rest, col1, col2);
            }

            function tail3(rest, col1, col2)
            {
                return pp_down(rest, col2, col1, extra, pp3);
            }

            var head = car(expr);
            var rest = cdr(expr);
            var col1 = wr(head, out("(", col));
            if (isnamed && isPair(rest))
            {
                var name = car(rest);
                var col2 = wr(name, out(" ", col1));
                return tail1(cdr(rest), col+indent_general, col2, col2+1);
            }
            else
                return tail1(rest, col+indent_general, col1, col1+1);
        }

        function pp_expr_list(lst, col, extra)
        {
            return pp_list(lst, col, extra, pp_expr);
        }

        function pp_lambda(expr, col, extra)
        {
            return pp_general(expr, col, extra, false, pp_expr_list, null, pp_expr);
        }

        function pp_if(expr, col, extra)
        {
            return pp_general(expr, col, extra, false, pp_expr, null, pp_expr);
        }

        function pp_cond(expr, col, extra)
        {
            return pp_call(expr, col, extra, pp_expr_list);
        }

        function pp_and(expr, col, extra)
        {
            return pp_call(expr, col, extra, pp_expr);
        }

        function pp_let(expr, col, extra)
        {
            var rest = cdr(expr);
            var isnamed = isPair(rest) && isSymbol(car(rest));
            return pp_general(expr, col, extra, isnamed, pp_expr_list, null, pp_expr);
        }

        function pp_begin(expr, col, extra)
        {
            return pp_general(expr, col, extra, false, null, null, pp_expr);
        }

        function pp_do(expr, col, extra)
        {
            return pp_general(expr, col, extra, false, pp_expr_list, pp_expr_list, pp_expr);
        }

        function style(head)
        {
            if (head.name == "lambda" ||
                head.name == "let*" ||
                head.name == "letrec" ||
                head.name == "define")
                return pp_lambda;
            else if (head.name == "if" ||
                     head.name == "set!")
                return pp_if;
            else if (head.name == "cond")
                return pp_cond;
            else if (head.name == "case")
                return pp_case;
            else if (head.name == "and" ||
                     head.name == "or")
                return pp_and;
            else if (head.name == "let")
                return pp_let;
            else if (head.name == "begin")
                return pp_begin;
            else if (head.name == "do")
                return pp_do;
            else
                return null;
        }

        pr(obj, col, 0, pp_expr);
    }

    if (width < 0)
        wr(obj, 0);
    else
        pp(obj, 0);
}

var indent_general = 2;
var max_call_head_width = 5;
var max_expr_width = 50;

//-----------------------------------------------------------------------------

// Scheme code generation.

function gen_include(filename)
{
    return cons(include, cons(filename, empty_list));
}

function gen_var(variable, value)
{
    return cons(new Symbol("js.var"),
                cons(variable, cons(value, empty_list)));
}

function gen_assign(variable, value)
{
    return cons(new Symbol("js.="),
                cons(variable, cons(value, empty_list)));
}

function gen_function(params, body)
{
    return cons(new Symbol("js.function"),
                cons(vector2list(params),
                     cons(body, empty_list)));
}

function gen_if(test, consequent, alternative)
{
    return cons(new Symbol("js.if"),
                cons(test, cons(consequent, cons(alternative, empty_list))));
}

function gen_call(fn_and_args)
{
    return cons(new Symbol("js.call"), vector2list(fn_and_args));
}

function gen_new(ctor_and_args)
{
    return cons(new Symbol("js.new"), vector2list(ctor_and_args));
}

function gen_begin(exprs)
{
    if (exprs.length == 1)
        return exprs[0];
    else
        return cons(begin, vector2list(exprs));
}

function gen_op(op, args)
{
    return cons(js_op_to_scm_table[op], vector2list(args));
}

var js_op_to_scm_table = [];
js_op_to_scm_table["delete x"]       = new Symbol("js.delete");
js_op_to_scm_table["void x"]         = new Symbol("js.void");
js_op_to_scm_table["typeof x"]       = new Symbol("js.typeof");
js_op_to_scm_table["++ x"]           = new Symbol("js.++x");
js_op_to_scm_table["auto ++ x"]      = new Symbol("js.auto++x");
js_op_to_scm_table["-- x"]           = new Symbol("js.--x");
js_op_to_scm_table["auto -- x"]      = new Symbol("js.auto--x");
js_op_to_scm_table["+ x"]            = new Symbol("js.+");
js_op_to_scm_table["- x"]            = new Symbol("js.-");
js_op_to_scm_table["~ x"]            = new Symbol("js.~");
js_op_to_scm_table["! x"]            = new Symbol("js.!");
js_op_to_scm_table["x ++"]           = new Symbol("js.x++");
js_op_to_scm_table["x --"]           = new Symbol("js.x--");
js_op_to_scm_table["x * y"]          = new Symbol("js.*");
js_op_to_scm_table["x / y"]          = new Symbol("js./");
js_op_to_scm_table["x % y"]          = new Symbol("js.%");
js_op_to_scm_table["x + y"]          = new Symbol("js.+");
js_op_to_scm_table["x - y"]          = new Symbol("js.-");
js_op_to_scm_table["x << y"]         = new Symbol("js.<<");
js_op_to_scm_table["x >> y"]         = new Symbol("js.>>");
js_op_to_scm_table["x >>> y"]        = new Symbol("js.>>>");
js_op_to_scm_table["x < y"]          = new Symbol("js.<");
js_op_to_scm_table["x > y"]          = new Symbol("js.>");
js_op_to_scm_table["x <= y"]         = new Symbol("js.<=");
js_op_to_scm_table["x >= y"]         = new Symbol("js.>=");
js_op_to_scm_table["x instanceof y"] = new Symbol("js.instanceof");
js_op_to_scm_table["x in y"]         = new Symbol("js.in");
js_op_to_scm_table["x == y"]         = new Symbol("js.==");
js_op_to_scm_table["x != y"]         = new Symbol("js.!=");
js_op_to_scm_table["x === y"]        = new Symbol("js.===");
js_op_to_scm_table["x !== y"]        = new Symbol("js.!==");
js_op_to_scm_table["x & y"]          = new Symbol("js.&");
js_op_to_scm_table["x ^ y"]          = new Symbol("js.^");
js_op_to_scm_table["x | y"]          = new Symbol("js.|");
js_op_to_scm_table["x && y"]         = new Symbol("js.&&");
js_op_to_scm_table["x || y"]         = new Symbol("js.||");
js_op_to_scm_table["x , y"]          = new Symbol("js.,");
js_op_to_scm_table["x = y"]          = new Symbol("js.=");
js_op_to_scm_table["x += y"]         = new Symbol("js.+=");
js_op_to_scm_table["x -= y"]         = new Symbol("js.-=");
js_op_to_scm_table["x *= y"]         = new Symbol("js.*=");
js_op_to_scm_table["x /= y"]         = new Symbol("js./=");
js_op_to_scm_table["x <<= y"]        = new Symbol("js.<<=");
js_op_to_scm_table["x >>= y"]        = new Symbol("js.>>=");
js_op_to_scm_table["x >>>= y"]       = new Symbol("js.>>>=");
js_op_to_scm_table["x &= y"]         = new Symbol("js.&=");
js_op_to_scm_table["x ^= y"]         = new Symbol("js.^=");
js_op_to_scm_table["x |= y"]         = new Symbol("js.|=");
js_op_to_scm_table["x %= y"]         = new Symbol("js.%=");
js_op_to_scm_table["x [ y ]"]        = new Symbol("js.index");

function gen_ref(sym)
{
    return sym;
}

function gen_this()
{
    return cons(new Symbol("js.this"), empty_list);
}

function gen_array_lit(elems)
{
    return cons(new Symbol("js.array-lit"),
                vector2list(elems));
}

function gen_obj_lit(props)
{
    return cons(new Symbol("js.obj-lit"),
                vector2list(props));
}

function gen_prop(name, value)
{
    return cons(new Symbol("js.prop"),
                cons(name, cons(value, empty_list)));
}


function gen_void()
{
    return gen_lit(false);
}

function gen_lit(data)
{
    if (self_evaluating(data))
        return data;
    else
        return cons(quote, cons(data, empty_list));
}

function self_evaluating(data)
{
    return isBoolean(data) ||
        isString(data) ||
        isNumber(data);
}

//-----------------------------------------------------------------------------

// AST to Scheme translator.

function Context(tail, global)
{
    this.tail = tail;
    this.global = global;
}

function nontail_ctx(ctx)
{
    var c = new Context(false, ctx.global);
    return c;
}

function global_ctx()
{
    var c = new Context(true, true);
    return c;
}

function function_body_ctx()
{
    var c = new Context(true, false);
    return c;
}

function ast_array_to_scm(asts, ctx)
{
    var accum = [];
    for (var i=0; i<asts.length; i++)
        accum.push(ast_to_scm(asts[i],
                              (i == asts.length-1) ? ctx : nontail_ctx(ctx)));
    return accum;
}

function statements_to_scm(asts, ctx)
{
    if (asts.length == 0)
        return gen_void();
    else
        return gen_begin(ast_array_to_scm(asts, ctx));
}

function ast_to_scm(ast, ctx)
{
    if (ast == null)
        error("null ast");
    else if (ast instanceof Program)
    {
        return gen_begin([gen_include("js2scm-rt#.scm"),gen_include("js2scm-rt.scm")].concat(gen_begin(ast_array_to_scm(ast.statements, ctx))));
    }
    else if (ast instanceof BlockStatement)
    {
        return statements_to_scm(ast.statements, ctx);
    }
    else if (ast instanceof VariableStatement)
    {
        var accum = [];
        for (var i=0; i<ast.decls.length; i++)
        {
            var d = ast.decls[i];
            if (ctx.global)
            {
                if (d.initializer != null)
                    accum.push(gen_var(js_id_to_scm(d.id),
                                       ast_to_scm(d.initializer,
                                                  nontail_ctx(ctx))));
                else
                    accum.push(gen_var(js_id_to_scm(d.id),
                                       gen_void()));
            }
            else if (d.initializer != null)
                accum.push(gen_assign(js_id_to_scm(d.id),
                                      ast_to_scm(d.initializer,
                                                 nontail_ctx(ctx))));
        }
        return gen_begin(accum);
    }
    else if (ast instanceof ConstStatement)
    {
        // TODO
        error("ConstStatement not implemented");
    }
    else if (ast instanceof FunctionDeclaration)
    {
        var accum = [];
        for (var i=0; i<ast.params.length; i++)
            accum.push(js_id_to_scm(ast.params[i]));
        return gen_var(js_id_to_scm(ast.id),
                       gen_function(accum,
                                    statements_to_scm(ast.body,
                                                      function_body_ctx())));
    }
    else if (ast instanceof ExprStatement)
    {
        return ast_to_scm(ast.expr, ctx);
    }
    else if (ast instanceof IfStatement)
    {
        if (ast.statements.length == 1)
            return gen_if(ast_to_scm(ast.expr, nontail_ctx(ctx)),
                          ast_to_scm(ast.statements[0], ctx),
                          gen_void());
        else
            return gen_if(ast_to_scm(ast.expr, nontail_ctx(ctx)),
                          ast_to_scm(ast.statements[0], ctx),
                          ast_to_scm(ast.statements[1], ctx));
    }
    else if (ast instanceof DoWhileStatement)
    {
        // TODO
        error("DoWhileStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "DoWhileStatement");
        pp_asts(indent, "statement", [ast.statement]);
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof WhileStatement)
    {
        // TODO
        error("WhileStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "WhileStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForStatement)
    {
        // TODO
        error("ForStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "ForStatement");
        pp_asts(indent, "expr1", [ast.expr1]);
        pp_asts(indent, "expr2", [ast.expr2]);
        pp_asts(indent, "expr3", [ast.expr3]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForVarStatement)
    {
        // TODO
        error("ForVarStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "ForVarStatement");
        pp_asts(indent, "decls", ast.decls);
        pp_asts(indent, "expr2", [ast.expr2]);
        pp_asts(indent, "expr3", [ast.expr3]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForInStatement)
    {
        // TODO
        error("ForInStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "ForInStatement");
        pp_asts(indent, "lhs_expr", [ast.lhs_expr]);
        pp_asts(indent, "set_expr", [ast.set_expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ForVarInStatement)
    {
        // TODO
        error("ForVarInStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "ForVarInStatement");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "initializer", [ast.initializer]);
        pp_asts(indent, "set_expr", [ast.set_expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ContinueStatement)
    {
        // TODO
        error("ContinueStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "ContinueStatement");
        print(pp_prefix(indent) + "|-label= " + ast.label);
    }
    else if (ast instanceof BreakStatement)
    {
        // TODO
        error("BreakStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "BreakStatement");
        print(pp_prefix(indent) + "|-label= " + ast.label);
    }
    else if (ast instanceof ReturnStatement)
    {
        if (ctx.global)
            error("return not allowed at top level");
        else if (!ctx.tail)
            error("return not in tail position");
        else
            return ast_to_scm(ast.expr, ctx);
    }
    else if (ast instanceof WithStatement)
    {
        // TODO
        error("WithStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "WithStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof SwitchStatement)
    {
        // TODO
        error("SwitchStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "SwitchStatement");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "clauses", ast.clauses);
    }
    else if (ast instanceof CaseClause)
    {
        // TODO
        error("CaseClause not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "CaseClause");
        pp_asts(indent, "expr", [ast.expr]);
        pp_asts(indent, "statements", ast.statements);
    }
    else if (ast instanceof LabelledStatement)
    {
        // TODO
        error("LabelledStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "LabelledStatement");
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "statement", [ast.statement]);
    }
    else if (ast instanceof ThrowStatement)
    {
        // TODO
        error("ThrowStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "ThrowStatement");
        pp_asts(indent, "expr", [ast.expr]);
    }
    else if (ast instanceof TryStatement)
    {
        // TODO
        error("TryStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "TryStatement");
        pp_asts(indent, "statement", [ast.statement]);
        print(pp_prefix(indent) + "|-id= " + ast.id);
        pp_asts(indent, "catch_part", [ast.catch_part]);
        pp_asts(indent, "finally_part", [ast.finally_part]);
    }
    else if (ast instanceof DebuggerStatement)
    {
        // TODO
        error("DebuggerStatement not implemented");
        pp_loc(ast.loc, pp_prefix(indent) + "DebuggerStatement");
    }
    else if (ast instanceof OpExpr)
    {
        return gen_op(ast.op, ast_array_to_scm(ast.exprs, false));
    }
    else if (ast instanceof NewExpr)
    {
        var accum = [ast_to_scm(ast.expr, nontail_ctx(ctx))];
        for (var i=0; i<ast.args.length; i++)
            accum.push(ast_to_scm(ast.args[i], nontail_ctx(ctx)));
        return gen_new(accum);
    }
    else if (ast instanceof CallExpr)
    {
        var accum = [ast_to_scm(ast.fn, nontail_ctx(ctx))];
        for (var i=0; i<ast.args.length; i++)
            accum.push(ast_to_scm(ast.args[i], nontail_ctx(ctx)));
        return gen_call(accum);
    }
    else if (ast instanceof FunctionExpr)
    {
        var accum = [];
        for (var i=0; i<ast.params.length; i++)
            accum.push(js_id_to_scm(ast.params[i]));
        return gen_function(accum,
                            statements_to_scm(ast.body,
                                              function_body_ctx()));
    }
    else if (ast instanceof Literal)
    {
        return gen_lit(js_value_to_scm(ast.value));
    }
    else if (ast instanceof ArrayLiteral)
    {
        var accum = [];
        for (var i=0; i<ast.exprs.length; i++)
            accum.push(ast_to_scm(ast.exprs[i], nontail_ctx(ctx)));
        return gen_array_lit(accum);
    }
    else if (ast instanceof ObjectLiteral)
    {
        var accum = [];
        for (var i=0; i<ast.properties.length; i++)
        {
            var p = ast.properties[i];
            accum.push(gen_prop(ast_to_scm(p.name, nontail_ctx(ctx)),
                                ast_to_scm(p.value, nontail_ctx(ctx))));
        }
        return gen_obj_lit(accum);
    }
    else if (ast instanceof Ref)
    {
        return gen_ref(js_id_to_scm(ast.id));
    }
    else if (ast instanceof This)
    {
        return gen_this();
    }
    else
        error("UNKNOWN AST");
}

function js_id_to_scm(id)
{
    return new Symbol("_" + id);
}

function js_value_to_scm(value)
{
    if (value == null)
        return empty_list;
    else
        return value;
}

function compile_to_scm(ast)
{
    print("#! /usr/bin/env gsi");
    scm_pp(ast_to_scm(ast, global_ctx()));
}

//=============================================================================
