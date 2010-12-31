//=============================================================================

// File: "scheme.js", Time-stamp: <2010-12-31 11:51:50 feeley>

// Copyright (c) 2010 by Marc Feeley, All Rights Reserved.

//=============================================================================

// Scheme object constructors.

function isBoolean(obj)
{
    return (typeof obj === "boolean") || (obj instanceof Boolean);
}

function isNumber(obj)
{
    return (typeof obj === "number") || (obj instanceof Number);
}

function isString(obj)
{
    return (typeof obj === "string") || (obj instanceof String);
}

function isProcedure(obj)
{
    return (typeof obj === "function") || (obj instanceof Function);
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
            (car(expr).name === "quote" ||
             car(expr).name === "quasiquote" ||
             car(expr).name === "unquote" ||
             car(expr).name === "unquote-splicing");
    }

    function read_macro_body(expr)
    {
        return car(cdr(expr));
    }

    function read_macro_prefix(expr)
    {
        if (car(expr).name === "quote")
            return "'";
        else if (car(expr).name === "quasiquote")
            return "`";
        else if (car(expr).name === "unquote")
            return ",";
        else if (car(expr).name === "unquote-splicing")
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
            return out((x === false) ? "#f" : "#t", col);
        }

        function wr_number(x, col)
        {
            return out(x.toString(), col);
        }

        function wr_string(x, col)
        {
            col = out("\"", col);
            for (var i=0; i<x.length; i++)
            {
                var c = x.charCodeAt(i);
                if (c === 92 || c === 34) // \ or "
                    col = out(String.fromCharCode(c), out("\\", col));
                else if (c === 10) // LF
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

        if (typeof obj === "undefined")
            return wr_undefined(obj, col);
        else if (obj === null)
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
                    if (proc !== null)
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
                if (pp1 !== null && isPair(rest))
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
                if (pp2 !== null && isPair(rest))
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
            if (head.name === "lambda" ||
                head.name === "let*" ||
                head.name === "letrec" ||
                head.name === "define")
                return pp_lambda;
            else if (head.name === "if" ||
                     head.name === "set!")
                return pp_if;
            else if (head.name === "cond")
                return pp_cond;
            else if (head.name === "and" ||
                     head.name === "or")
                return pp_and;
            else if (head.name === "let")
                return pp_let;
            else if (head.name === "begin")
                return pp_begin;
            else if (head.name === "do")
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

function gen_var(variable)
{
    return cons(new Symbol("js.var"),
                cons(variable, empty_list));
}

function gen_assign(variable, value)
{
    return cons(new Symbol("js.="),
                cons(variable, cons(value, empty_list)));
}

function gen_function(features, params, body)
{
    return cons(features.use_nontail_return
                ? (features.use_arguments
                   ? new Symbol("js.function-with-nontail-return-with-arguments")
                   : new Symbol("js.function-with-nontail-return"))
                : (features.use_arguments
                   ? new Symbol("js.function-with-arguments")
                   : new Symbol("js.function")),
                cons(vector2list(params),
                     cons(body, empty_list)));
}

function gen_return(value)
{
    return cons(new Symbol("js.return"),
                cons(value, empty_list));
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
    if (exprs.length === 0)
        return gen_undefined();
    else if (exprs.length === 1)
        return exprs[0];
    else
        return cons(begin, vector2list(exprs));
}

function gen_continue(ctrl_point)
{
    return cons(new Symbol("js.continue"),
                cons(ctrl_point, empty_list));
}

function gen_break(ctrl_point)
{
    return cons(new Symbol("js.break"),
                cons(ctrl_point, empty_list));
}

function gen_dowhile(break_ctrl_point, continue_ctrl_point, loop_id, body, test)
{
    return cons(new Symbol("js.dowhile"),
                cons(break_ctrl_point,
                     cons(continue_ctrl_point,
                          cons(loop_id, cons(body, cons(test, empty_list))))));
}

function gen_while(break_ctrl_point, continue_ctrl_point, loop_id, test, body)
{
    return cons(new Symbol("js.while"),
                cons(break_ctrl_point,
                     cons(continue_ctrl_point,
                          cons(loop_id, cons(test, cons(body, empty_list))))));
}

function gen_for(break_ctrl_point, continue_ctrl_point, loop_id, test, step, body)
{
    return cons(new Symbol("js.for"),
                cons(break_ctrl_point,
                     cons(continue_ctrl_point,
                          cons(loop_id, cons(test, cons(step, cons(body, empty_list)))))));
}

function gen_forin(break_ctrl_point, continue_ctrl_point, loop_id, id, set, body)
{
    return cons(new Symbol("js.forin"),
                cons(break_ctrl_point,
                     cons(continue_ctrl_point,
                          cons(loop_id, cons(id, cons(set, cons(body, empty_list)))))));
}

function gen_switch(break_ctrl_point, value, clauses)
{
    return cons(new Symbol("js.switch"),
                cons(break_ctrl_point,
                     cons(value, vector2list(clauses))));
}

function gen_case(fall_through, expr, statement)
{
    return cons(fall_through
                ? new Symbol("js.case-fall-through")
                : new Symbol("js.case"),
                cons((expr === null)
                     ? cons(new Symbol("js.default"), empty_list)
                     : expr,
                     cons(statement, empty_list)));
}

function gen_throw(val)
{
    return cons(new Symbol("js.throw"),
                cons(val, empty_list));
}

function gen_try(body, final_body)
{
    return cons(new Symbol("js.try"),
                cons(body,
                     (final_body === null)
                     ? empty_list
                     : cons(final_body, empty_list)));
}

function gen_try_catch(body, id, catch_body, final_body)
{
    return cons(new Symbol("js.try-catch"),
                cons(body,
                     cons(id,
                          cons(catch_body,
                               (final_body === null)
                               ? empty_list
                               : cons(final_body, empty_list)))));
}

function gen_debugger()
{
    return cons(new Symbol("js.debugger"), empty_list);
}

function gen_op(op, args)
{
    return cons(js_op_to_scm_table[op], vector2list(args));
}

var js_op_to_scm_table = {};
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
js_op_to_scm_table["x | y"]          = new Symbol("|js.\\||");
js_op_to_scm_table["x && y"]         = new Symbol("js.&&");
js_op_to_scm_table["x || y"]         = new Symbol("|js.\\|\\||");
js_op_to_scm_table["x , y"]          = new Symbol("|js.,|");
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
js_op_to_scm_table["x ? y : z"]      = new Symbol("js.x?y:z");

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

function gen_undefined()
{
    return cons(new Symbol("js.undefined"), empty_list);
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

function Context(tail, global, spine, features)
{
    this.tail = tail;
    this.global = global;
    this.spine = spine;
    this.features = features;
}

function Features(use_nontail_return,
                  use_arguments,
                  ctrl_point_count)
{
    this.use_nontail_return = use_nontail_return;
    this.use_arguments      = use_arguments;
    this.ctrl_point_count   = ctrl_point_count;
}

function begin_function(ctx)
{
    var state = new Features(ctx.features.use_nontail_return,
                             ctx.features.use_arguments,
                             ctx.features.ctrl_point_count);
    ctx.features.use_nontail_return = false;
    ctx.features.use_arguments      = false;
    ctx.features.ctrl_point_count   = 0;
    return state;
}

function end_function(state, ctx, result)
{
    ctx.features.use_nontail_return = state.use_nontail_return;
    ctx.features.use_arguments      = state.use_arguments;
    ctx.features.ctrl_point_count   = state.ctrl_point_count;
    return result;
}

function nest_ctx(ctx, ast)
{
    return new Context(ctx.tail,
                       ctx.global,
                       { ast: ast,
                         break_ctrl_point: 0,
                         continue_ctrl_point: 0,
                         parent: ctx.spine },
                       ctx.features);
}

function nontail_ctx(ctx)
{
    return new Context(false,
                       ctx.global,
                       ctx.spine,
                       ctx.features);
}

function global_ctx()
{
    return new Context(true,
                       true,
                       null,
                       new Features(false, false, 0, 0));
}

function function_body_ctx()
{
    return new Context(true,
                       false,
                       null,
                       new Features(false, false, 0, 0));
}

function ast_array_to_scm(asts, ctx)
{
    var accum = [];
    for (var i=0; i<asts.length; i++)
    {
        var ast_i = asts[i];
        var ctx_i = (i === asts.length-1) ? ctx : nontail_ctx(ctx);
        accum.push(ast_to_scm(ast_i, ctx_i));
    }
    return accum;
}

function statements_to_scm(asts, ctx)
{
    return gen_begin(ast_array_to_scm(asts, ctx));
}

function force_undefined_at_tail(code, ctx)
{
    if (ctx.tail)
        return gen_begin([code, gen_undefined()]);
    else
        return code;
}

function ast_to_scm(ast, ctx)
{
    ctx = nest_ctx(ctx, ast);

    if (ast === null)
        error("null ast");
    else if (ast instanceof Program)
    {
        var accum = [];
        for (var v in ast.vars)
            accum.push(gen_var(js_id_to_scm(v)));
        return gen_begin([gen_include("js2scm-rt#.scm"),
                          gen_include("js2scm-rt.scm")].
                         concat([gen_begin(accum.concat([ast_to_scm(ast.block, ctx)]))]));
    }
    else if (ast instanceof BlockStatement)
    {
        return statements_to_scm(ast.statements, ctx);
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
        return force_undefined_at_tail(
            gen_assign(js_id_to_scm(ast.id.toString()),
                       ast_to_scm(ast.funct, ctx)),
            ctx);
    }
    else if (ast instanceof ExprStatement)
    {
        return force_undefined_at_tail(ast_to_scm(ast.expr, ctx),
                                       ctx);
    }
    else if (ast instanceof IfStatement)
    {
        if (ast.statements.length === 1)
            return gen_if(ast_to_scm(ast.expr, nontail_ctx(ctx)),
                          ast_to_scm(ast.statements[0], ctx),
                          gen_undefined());
        else
            return gen_if(ast_to_scm(ast.expr, nontail_ctx(ctx)),
                          ast_to_scm(ast.statements[0], ctx),
                          ast_to_scm(ast.statements[1], ctx));
    }
    else if (ast instanceof DoWhileStatement)
    {
        var loop_id = gensym("loop");
        var stat_scm = ast_to_scm(ast.statement, nontail_ctx(ctx));
        var expr_scm = ast_to_scm(ast.expr, nontail_ctx(ctx));

        return force_undefined_at_tail(gen_dowhile(ctx.spine.break_ctrl_point,
                                                   ctx.spine.continue_ctrl_point,
                                                   loop_id,
                                                   stat_scm,
                                                   expr_scm),
                                       ctx);
    }
    else if (ast instanceof WhileStatement)
    {
        var loop_id = gensym("loop");
        var expr_scm = ast_to_scm(ast.expr, nontail_ctx(ctx));
        var stat_scm = ast_to_scm(ast.statement, nontail_ctx(ctx));

        return force_undefined_at_tail(gen_while(ctx.spine.break_ctrl_point,
                                                 ctx.spine.continue_ctrl_point,
                                                 loop_id,
                                                 expr_scm,
                                                 stat_scm),
                                       ctx);
    }
    else if (ast instanceof ForStatement)
    {
        var loop_id = gensym("loop");

        var expr1_scm = (ast.expr1 === null)
                        ? gen_undefined()
                        : ast_to_scm(ast.expr1, nontail_ctx(ctx));

        var expr2_scm = (ast.expr2 === null)
                        ? gen_lit(js_value_to_scm(true))
                        : ast_to_scm(ast.expr2, nontail_ctx(ctx));

        var stat_scm = ast_to_scm(ast.statement, nontail_ctx(ctx));

        var expr3_scm = (ast.expr3 === null)
                        ? gen_undefined()
                        : ast_to_scm(ast.expr3, nontail_ctx(ctx));

        return force_undefined_at_tail(gen_begin([expr1_scm,
                                                  gen_for(ctx.spine.break_ctrl_point,
                                                          ctx.spine.continue_ctrl_point,
                                                          loop_id,
                                                          expr2_scm,
                                                          stat_scm,
                                                          expr3_scm)]),
                                       ctx);
    }
    // else if (ast instanceof ForVarStatement)
    // { impossible due to pass1 transformations }
    else if (ast instanceof ForInStatement)
    {
        var loop_id = gensym("loop");
        var lhs_expr_scm = ast_to_scm(ast.lhs_expr, nontail_ctx(ctx));
        var set_expr_scm = ast_to_scm(ast.set_expr, nontail_ctx(ctx));
        var stat_scm = ast_to_scm(ast.statement, nontail_ctx(ctx));

        return force_undefined_at_tail(gen_forin(ctx.spine.break_ctrl_point,
                                                 ctx.spine.continue_ctrl_point,
                                                 loop_id,
                                                 lhs_expr_scm,
                                                 set_expr_scm,
                                                 stat_scm),
                                       ctx);
    }
    // else if (ast instanceof ForVarInStatement)
    // { impossible due to pass1 transformations }
    else if (ast instanceof ContinueStatement)
    {
        var probe = ctx.spine;
        var target = null;

        while (probe !== null)
        {
            if (probe.ast instanceof LabelledStatement)
            {
                if (ast.label !== null &&
                    ast.label.toString() === probe.ast.label.toString())
                    break;
            }
            else if (probe.ast instanceof DoWhileStatement ||
                     probe.ast instanceof WhileStatement ||
                     probe.ast instanceof ForStatement ||
                     probe.ast instanceof ForInStatement)
            {
                target = probe;
                if (ast.label === null)
                    break;
            }
            else
                target = null;

            probe = probe.parent;
        }

        if (target === null)
            error("undefined continue target");

        if (target.continue_ctrl_point === 0)
            target.continue_ctrl_point = ++ctx.features.ctrl_point_count;

        return gen_continue(target.continue_ctrl_point);
    }
    else if (ast instanceof BreakStatement)
    {
        var probe = ctx.spine;
        var target = null;

        while (probe !== null)
        {
            if (probe.ast instanceof LabelledStatement)
            {
                if (ast.label !== null &&
                    ast.label.toString() === probe.ast.label.toString())
                    break;
            }
            else if (probe.ast instanceof DoWhileStatement ||
                     probe.ast instanceof WhileStatement ||
                     probe.ast instanceof ForStatement ||
                     probe.ast instanceof ForInStatement ||
                     probe.ast instanceof SwitchStatement)
            {
                target = probe;
                if (ast.label === null)
                    break;
            }
            else
                target = null;

            probe = probe.parent;
        }

        if (target === null)
            error("undefined break target");

        if (target.break_ctrl_point === 0)
            target.break_ctrl_point = ++ctx.features.ctrl_point_count;

        return gen_break(target.break_ctrl_point);
    }
    else if (ast instanceof ReturnStatement)
    {
        if (ctx.global)
            error("return not allowed at top level");
        else
        {
            var value_scm = (ast.expr === null)
                            ? gen_undefined()
                            : ast_to_scm(ast.expr, ctx);

            if (!ctx.tail)
            {
                ctx.features.use_nontail_return = true;
                return gen_return(value_scm);
            }
            else
                return value_scm;
        }
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
        function gen_clause(expr, statements, ctx, last_clause)
        {
            var fall_through = !last_clause;
            var accum = [];

            var i = 0;
            while (i < statements.length)
            {
                var statement = statements[i++];
                if (i === statements.length)
                {
                    if (statement instanceof BlockStatement)
                    {
                        statements = statement.statements;
                        i = 0;
                    }
                    else if (statement instanceof ContinueStatement ||
                             statement instanceof ReturnStatement)
                    {
                        fall_through = false;
                        accum.push(ast_to_scm(statement, ctx));
                    }
                    else if (statement instanceof BreakStatement)
                    {
                        fall_through = false;
                    }
                    else
                        accum.push(ast_to_scm(statement, ctx));
                }
                else
                    accum.push(ast_to_scm(statement, ctx));
            }

            return gen_case(fall_through,
                            (expr === null)
                            ? null
                            : ast_to_scm(expr, nontail_ctx(ctx)),
                            gen_begin(accum));
        }

        var expr_scm = ast_to_scm(ast.expr, nontail_ctx(ctx));

        var accum = [];
        for (var i=0; i<ast.clauses.length; i++)
        {
            var clause_i = ast.clauses[i];
            accum.push(gen_clause(clause_i.expr,
                                  clause_i.statements,
                                  nontail_ctx(ctx), // TODO: could be optimized
                                  i === ast.clauses.length-1));
        }

        return force_undefined_at_tail(gen_switch(ctx.spine.break_ctrl_point,
                                                  expr_scm,
                                                  accum),
                                       ctx);
    }
    // else if (ast instanceof CaseClause)
    // { impossible due to handling of SwitchStatement }
    else if (ast instanceof LabelledStatement)
    {
        return ast_to_scm(ast.statement, ctx);
    }
    else if (ast instanceof ThrowStatement)
    {
        return gen_throw(ast_to_scm(ast.expr, nontail_ctx(ctx)));
    }
    else if (ast instanceof TryStatement)
    {
        var body = ast_to_scm(ast.statement, nontail_ctx(ctx));
        var final_body = (ast.finally_part === null)
                         ? null
                         : ast_to_scm(ast.finally_part, nontail_ctx(ctx));

        if (ast.catch_part === null)
            return gen_try(body,
                           final_body);
        else
            return gen_try_catch(body,
                                 js_id_to_scm(ast.catch_part.id.toString()),
                                 ast_to_scm(ast.catch_part.statement, nontail_ctx(ctx)),
                                 final_body);
    }
    // else if (ast instanceof CatchPart)
    // { impossible due to handling of TryStatement }
    else if (ast instanceof DebuggerStatement)
    {
        return gen_debugger();
    }
    else if (ast instanceof OpExpr)
    {
        return gen_op(ast.op, ast_array_to_scm(ast.exprs, nontail_ctx(ctx)));
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
        var new_ctx = function_body_ctx();

        var accum = [];
        for (var v in ast.vars)
        {
            if (!ast.vars[v].is_param)
                accum.push(gen_var(js_id_to_scm(v)));
        }

        var params = ast.params.map(
                       function (param, i, self)
                       {
                           return js_id_to_scm(param.toString());
                       });

        accum.push(statements_to_scm(ast.body, new_ctx));

        return gen_function(new_ctx.features,
                            params,
                            gen_begin(accum));
    }
    else if (ast instanceof Literal)
    {
        return gen_lit(js_value_to_scm(ast.value));
    }
    else if (ast instanceof ArrayLiteral)
    {
        return gen_array_lit(
                   ast.exprs.map(
                       function (expr, i, self)
                       {
                           return ast_to_scm(expr,
                                             nontail_ctx(ctx));
                       }));
    }
    else if (ast instanceof ObjectLiteral)
    {
        return gen_obj_lit(
                   ast.properties.map(
                       function (prop, i, self)
                       {
                           return gen_prop(ast_to_scm(prop.name,
                                                      nontail_ctx(ctx)),
                                           ast_to_scm(prop.value,
                                                      nontail_ctx(ctx)));
                       }));
    }
    else if (ast instanceof Ref)
    {
        if (ast.id === "arguments")
            ctx.features.use_arguments = true;

        return gen_ref(js_id_to_scm(ast.id.toString()));
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
    if (value === null)
        return empty_list;
    else
        return value;
}

function compile_to_scm(ast)
{
    print("#! /usr/bin/env gsi");
    //pp(ast);
    scm_pp(ast_to_scm(ast, global_ctx()));
}

//=============================================================================
