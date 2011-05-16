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

// File: "js86-compile.js"

// Author: Marc Feeley

//=============================================================================

// Flags that affect code generation.

var use_arg_count;
var use_global_prop_handlers;
var use_stack_limit_checks;
var use_ctx_handlers;
var use_ctx_constants;
var use_distinguish_hot_cold;

//=============================================================================

function clone(obj)
{
    if (obj === null || typeof obj !== "object")
        return obj;

    // Handle Array
    if (obj instanceof Array)
    {
        var copy = [];
        for (var i = 0; i < obj.length; ++i)
        {
            copy[i] = clone(obj[i]);
        }
        return copy;
    }

    // Handle Object
    if (obj instanceof Object)
    {
        var copy = {};
        for (var attr in obj)
        {
            if (obj.hasOwnProperty(attr))
                copy[attr] = clone(obj[attr]);
        }
        return copy;
    }

    error("Unsupported object type");
}

//=============================================================================

var deferred_code_actions = {};
deferred_code_actions[true] = [];
deferred_code_actions[false] = [];
var deferred_obj_actions = [];
var text_section = [];
var data_section = [];
var global_props = {};
var global_props_ordered = [];
var js_strings = {};

var undefined_val = -2;
var false_val = -10;
var true_val = -18;

var label_count = 0;

var all_regs = ["%edi","%esi","%eax","%ebx","%edx"];
var ra_reg_num = 0;
var this_reg_num = 1;
var arg1_reg_num = 2;
var result_reg_num = 2;
var ra_reg = all_regs[ra_reg_num];
var this_reg = all_regs[this_reg_num];
var ctx_reg = "%ecx";
var sp_reg = "%ebp";
var arg_count_reg = "%ecx";

var reg8 = {};
reg8["%eax"] = "%al";
reg8["%ebx"] = "%bl";
reg8["%ecx"] = "%cl";
reg8["%edx"] = "%dl";
reg8["%edi"] = "%edi_lo8"; // not a real register
reg8["%esi"] = "%esi_lo8"; // not a real register

var ra_name = " RA";
var this_name = " THIS";

var word_size = 4;
var global_prop_len = 3;

var tag_mask = 7;
var obj_tag = 7;
var prim_tag = 5;
var obj_type_offs = -(2 * word_size + obj_tag);
var obj_prototype_offs = -(1 * word_size + obj_tag);
var obj_property_table_offs = -(0 * word_size + obj_tag);
var property_offs = 2 * word_size - prim_tag;

var ctx_size = 1000*word_size;
var ctx_stack_limit_offs = 0*word_size;
var ctx_global_obj_offs = 1*word_size;
var ctx_putprop_offs = 2*word_size;
var ctx_stack_limit_handler_offs = 3*word_size;
var ctx_arg_count_handler_offs = 4*word_size;
var ctx_deoptimize_global_prop_handler_offs = 5*word_size;
var ctx_jump_global_prop_handler_offs = 6*word_size;
var ctx_generic_op_handler_offs = 7*word_size;
var ctx_undefined_offs = 8*word_size;
var ctx_false_offs = 9*word_size;
var ctx_true_offs = 10*word_size;

var stack_fudge = 1000*word_size;
var stack_size = 2000000*word_size;
var heap_fudge = 1000*word_size;
var heap_size = 1000000*word_size;

function defer_code(hot, action)
{
    if (!use_distinguish_hot_cold)
        hot = true;
    deferred_code_actions[hot].push(action);
}

function generate_deferred_code(hot)
{
    if (!use_distinguish_hot_cold)
        hot = true;
    while (deferred_code_actions[hot].length > 0)
    {
        deferred_code_actions[hot].shift()();
        if (!hot)
            generate_deferred_code(true);
    }
}

function defer_obj(action)
{
    deferred_obj_actions.push(action);
}

function generate_deferred_obj()
{
    while (deferred_obj_actions.length > 0)
        deferred_obj_actions.shift()();
}

function encode_js_value(val)
{
    if (val === undefined)
        return undefined_val;
    else if (val === false)
        return false_val;
    else if (val === true)
        return true_val;
    else if (typeof val === "number")
        return val*4;
    else
    {
        error("can't encode " + val);
    }
}

function encode_arg_count(n)
{
    return n;
}

function new_label(prefix)
{
    label_count++;
    return prefix + "_" + label_count;
}

function new_c_string(str)
{
    var lbl = new_label("");
    data_section.push(lbl, ": .ascii \"", str, "\\0\"\n");
    return lbl;
}

function new_global_prop(name)
{
    if (global_props.hasOwnProperty(name))
        return global_props[name];

    var index = global_props_ordered.length;
    global_props[name] = index;
    global_props_ordered.push(name);
    return index;
}

function new_js_string(str)
{
    if (js_strings.hasOwnProperty(str))
        return js_strings[str];

    var lbl = new_label("");
    js_strings[str] = lbl;
    return lbl;
}

function gen(str)
{
    text_section.push(str);
}

function gen_puts(str)
{
    gen(" PUTS(" + new_c_string(str) + ")\n");
}

function gen_printf(fmt, val)
{
    gen(" PRINTF(" + new_c_string(fmt) + "," + val + ")\n");
}

function mem(offs, reg)
{
    if (offs === 0)
        return "(" + reg + ")";
    else
        return offs + "(" + reg + ")";
}

function gen_init_runtime(global_obj_lbl, main_lbl, end_lbl)
{
    var rp_lbl = new_label("");

    gen("\
#if !defined(__USER_LABEL_PREFIX__)                                         \n\
#define __USER_LABEL_PREFIX__                                               \n\
#endif                                                                      \n\
                                                                            \n\
#define EXTERN(id) CONCAT1(__USER_LABEL_PREFIX__,id)                        \n\
#define CONCAT1(x,y) CONCAT0(x,y)                                           \n\
#define CONCAT0(x,y) x ## y                                                 \n\
                                                                            \n\
#define PAGE_SIZE_LOG2 12                                                   \n\
                                                                            \n\
#if defined(__MACOSX__) || (defined(__APPLE__) && defined(__MACH__))        \n\
#define ALIGN_EXPT2(log2) .align log2                                       \n\
#else                                                                       \n\
#define ALIGN_EXPT2(log2) .align 1<<log2                                    \n\
#endif                                                                      \n\
                                                                            \n\
#define ALIGN_OBJ ALIGN_EXPT2(3)                                            \n\
#define ALIGN_RET_POINT ALIGN_EXPT2(3)                                      \n\
#define ALIGN_FUNCTION ALIGN_EXPT2(3)                                       \n\
#define ALIGN_GLOBAL_OBJ ALIGN_EXPT2(3)                                     \n\
                                                                            \n\
#define FILLER4 orw $0,%ax                                                  \n\
#define FILLER3 addb $0,%dl                                                 \n\
#define FILLER2 addb $0,%al                                                 \n\
#define FILLER1 nop                                                         \n\
                                                                            \n\
#define PLAIN_OBJ_TYPE 0                                                    \n\
#define PLAIN_OBJ_HEADER_END FILLER3                                        \n\
                                                                            \n\
#define GLOBAL_OBJ_TYPE 1                                                   \n\
#define GLOBAL_OBJ_HEADER_END FILLER3                                       \n\
                                                                            \n\
#define FUNCTION_OBJ_TYPE 2                                                 \n\
#define FUNCTION_OBJ_HEADER_END FILLER3                                     \n\
                                                                            \n\
#define PROPERTY_TABLE_TYPE 3                                               \n\
#define PROPERTY_TABLE_HEADER_END FILLER1                                   \n\
                                                                            \n\
#define STRING_TYPE 4                                                       \n\
#define STRING_HEADER_END FILLER1                                           \n\
                                                                            \n\
#define RET_POINT_TYPE 5                                                    \n\
#define RET_POINT_HEADER_END FILLER1                                        \n\
#define FRAME_DESCR FILLER4 ; FILLER4                                       \n\
                                                                            \n\
#define C_FRAME_MULT 16                                                     \n\
                                                                            \n\
#define PUSHALL pushl %eax ; pushl %ebx ; pushl %ecx ; pushl %edx ; pushl %esi ; pushl %edi ; pushl $0 ; pushl $0 \n\
#define POPALL popl %edi ; popl %edi ; popl %edi ; popl %esi ; popl %edx ; popl %ecx ; popl %ebx ; popl %eax \n\
                                                                            \n\
#define PUSHALL_EXCEPT_EAX pushl %ebx ; pushl %ecx ; pushl %edx ; pushl %esi ; pushl %edi ; pushl $0 ; pushl $0 ; pushl $0 \n\
#define POPALL_EXCEPT_EAX popl %edi ; popl %edi ; popl %edi ; popl %edi ; popl %esi ; popl %edx ; popl %ecx ; popl %ebx \n\
                                                                            \n\
#define EXIT(n) PUSHALL ; subl $C_FRAME_MULT-8,%esp ; pushl n ; call EXTERN(exit) ; addl $C_FRAME_MULT-4,%esp ; POPALL \n\
#define PUTS(str) PUSHALL ; subl $C_FRAME_MULT-8,%esp ; pushl $str ; call EXTERN(puts) ; addl $C_FRAME_MULT-4,%esp ; POPALL \n\
#define PRINTF(fmt,val) PUSHALL ; subl $C_FRAME_MULT-12,%esp ; pushl val ; pushl $fmt ; call EXTERN(printf) ; addl $C_FRAME_MULT-4,%esp ; POPALL \n\
#define MALLOC(size) PUSHALL_EXCEPT_EAX ; subl $C_FRAME_MULT-8,%esp ; pushl size ; call EXTERN(malloc) ; addl $C_FRAME_MULT-4,%esp ; POPALL_EXCEPT_EAX \n\
#define MPROTECT(start,size,flags) PUSHALL ; subl $C_FRAME_MULT-16,%esp ; pushl $flags ; pushl size ; pushl start ; call EXTERN(mprotect) ; addl $C_FRAME_MULT-4,%esp ; POPALL \n\
                                                                            \n\
#define PROT_NONE       0x00                                                \n\
#define PROT_READ       0x01                                                \n\
#define PROT_WRITE      0x02                                                \n\
#define PROT_EXEC       0x04                                                \n\
                                                                            \n\
 .globl EXTERN(main)                                                        \n\
 .globl EXTERN(exit)                                                        \n\
 .globl EXTERN(puts)                                                        \n\
 .globl EXTERN(printf)                                                      \n\
 .globl EXTERN(malloc)                                                      \n\
 .globl EXTERN(mprotect)                                                    \n\
                                                                            \n\
 .text                                                                      \n\
 ALIGN_EXPT2(PAGE_SIZE_LOG2)                                                \n\
                                                                            \n\
EXTERN(main):                                                               \n\
                                                                            \n\
 pushl " + ctx_reg + "                                                      \n\
 pushl " + sp_reg + "                                                       \n\
 pushl " + ra_reg + "                                                       \n\
 pushl $0                                                                   \n\
                                                                            \n\
# MPROTECT($0,$" + end_lbl + ",PROT_READ|PROT_WRITE|PROT_EXEC)               \n\
 MPROTECT($EXTERN(main),$" + 8192 + ",PROT_READ|PROT_WRITE|PROT_EXEC)               \n\
                                                                            \n\
 movl $context," + ctx_reg + "                                              \n\
                                                                            \n\
 MALLOC($" + stack_size + ")                                                \n\
 addl $" + stack_fudge + ",%eax                                             \n\
 movl %eax," + mem(ctx_stack_limit_offs, ctx_reg) + "                       \n\
 addl $" + (stack_size-stack_fudge-word_size) + ",%eax                      \n\
 movl %eax," + sp_reg + "                                                   \n\
                                                                            \n\
 movl $" + global_obj_lbl + ",%eax                                          \n\
 movl %eax," + mem(ctx_global_obj_offs, ctx_reg) + "                        \n\
                                                                            \n\
 movl $stack_limit_handler,%eax                                             \n\
 movl %eax," + mem(ctx_stack_limit_handler_offs, ctx_reg) + "               \n\
                                                                            \n\
 movl $arg_count_handler,%eax                                               \n\
 movl %eax," + mem(ctx_arg_count_handler_offs, ctx_reg) + "                 \n\
                                                                            \n\
 movl $deoptimize_global_prop_handler,%eax                                  \n\
 movl %eax," + mem(ctx_deoptimize_global_prop_handler_offs, ctx_reg) + "    \n\
                                                                            \n\
 movl $jump_global_prop_handler,%eax                                        \n\
 movl %eax," + mem(ctx_jump_global_prop_handler_offs, ctx_reg) + "          \n\
                                                                            \n\
 movl $generic_op_handler,%eax                                              \n\
 movl %eax," + mem(ctx_generic_op_handler_offs, ctx_reg) + "                \n\
                                                                            \n\
 movl $" + undefined_val + "," + mem(ctx_undefined_offs, ctx_reg) + "       \n\
 movl $" + false_val + "," + mem(ctx_false_offs, ctx_reg) + "               \n\
 movl $" + true_val + "," + mem(ctx_true_offs, ctx_reg) + "                 \n\
                                                                            \n\
 movl $" + rp_lbl + "," + ra_reg + "                                        \n\
 movl " + mem(ctx_global_obj_offs, ctx_reg) + "," + this_reg + "            \n\
");
    gen_set_arg_count(0);
    gen("\
 jmp  " + main_lbl + "                                                      \n\
                                                                            \n\
 ALIGN_RET_POINT                                                            \n\
 FRAME_DESCR                                                                \n\
 .long RET_POINT_TYPE                                                       \n\
 RET_POINT_HEADER_END                                                       \n\
" + rp_lbl + ":                                                             \n\
                                                                            \n\
 popl " + ra_reg + "                                                        \n\
 popl " + ra_reg + "                                                        \n\
 popl " + sp_reg + "                                                        \n\
 popl " + ctx_reg + "                                                       \n\
                                                                            \n\
 movl $0,%eax                                                               \n\
 ret                                                                        \n\
                                                                            \n\
stack_limit_handler:                                                        \n\
 subl $C_FRAME_MULT-4,%esp                                                  \n\
 PUTS(stack_limit_msg)                                                      \n\
 EXIT($1)                                                                   \n\
stack_limit_msg:                                                            \n\
 .ascii \"stack_limit_handler was called\\0\"                               \n\
                                                                            \n\
arg_count_handler:                                                          \n\
 subl $C_FRAME_MULT-8,%esp                                                  \n\
 movl $0,%eax                                                               \n\
 movb 12(%esp),%al                                                          \n\
 PRINTF(arg_count_msg,%eax)                                                 \n\
 EXIT($1)                                                                   \n\
arg_count_msg:                                                              \n\
 .ascii \"arg_count_handler was called, arg_count=%d\\n\\0\"                \n\
                                                                            \n\
deoptimize_global_prop_handler:                                             \n\
 pushl %eax                                                                 \n\
 pushl %ebx                                                                 \n\
 pushl %ecx                                                                 \n\
                                                                            \n\
 movl " + mem(ctx_global_obj_offs, ctx_reg) + ",%ebx                        \n\
 movl " + mem(obj_property_table_offs, "%ebx") + ",%ebx                     \n\
 movl 12(%esp),%eax                                                         \n\
 addl (%eax),%ebx                                                           \n\
                                                                            \n\
 movl (%ebx),%ecx   # get property value                                    \n\
                                                                            \n\
# deoptimize patched jumps                                                  \n\
                                                                            \n\
 movl 8(%ebx),%eax                                                          \n\
 testl %eax,%eax                                                            \n\
 jz   deoptimize_global_prop_done                                           \n\
                                                                            \n\
 movb 2(%ecx),%cl # get parameter count                                     \n\
 pushl %ecx                                                                 \n\
                                                                            \n\
deoptimize_global_prop_loop:                                                \n\
 movl (%eax),%ecx                                                           \n\
 movl %ecx,8(%ebx)                                                          \n\
                                                                            \n\
");

    if (use_ctx_handlers)
    {
        gen("\
#                      | %eax                                               \n\
#  7  6  5  4  3  2  1 V                                                    \n\
# FF 51 jj pp pp pp pp nn 00 00 00     <- unoptimized code                  \n\
# B1 nn E9 xx xx xx xx yy yy yy yy     <- 'keep arg count' optimization     \n\
# E9 zz zz zz zz pp pp yy yy yy yy     <- 'avoid arg count' optimization    \n\
                                                                            \n\
");

        if (use_arg_count)
        {
            gen("\
 movl (%esp),%ecx                                                           \n\
 cmpb $0xB1,-7(%eax)                                                        \n\
 jne  deoptimize_global_prop_arg_count_setup                                \n\
 movb -6(%eax),%cl                                                          \n\
deoptimize_global_prop_arg_count_setup:                                     \n\
 movb %cl,(%eax) # write argument count                                     \n\
");
        }

        gen("\
 movl $0x51FF+" + (ctx_jump_global_prop_handler_offs<<16)+ ",-7(%eax) # write call *xxx(%ecx)\n\
 movl 16(%esp),%ecx # write the global property offset                      \n\
 movl (%ecx),%ecx                                                           \n\
 movl %ecx,-4(%eax)                                                         \n\
");
    }
    else
    {
        gen("\
#                      | %eax                                               \n\
#  7  6  5  4  3  2  1 V                                                    \n\
# E8 jj jj jj jj pp pp pp pp nn 00     <- unoptimized code                  \n\
# B1 nn E9 xx xx xx xx yy yy yy yy     <- 'keep arg count' optimization     \n\
# E9 zz zz zz zz pp pp yy yy yy yy     <- 'avoid arg count' optimization    \n\
                                                                            \n\
");

        if (use_arg_count)
        {
            gen("\
 movl (%esp),%ecx                                                           \n\
 cmpb $0xB1,-7(%eax)                                                        \n\
 jne  deoptimize_global_prop_arg_count_setup                                \n\
 movb -6(%eax),%cl                                                          \n\
deoptimize_global_prop_arg_count_setup:                                     \n\
 movb %cl,2(%eax) # write argument count                                    \n\
");
        }

        gen("\
 movb $0xE8,-7(%eax) # write call jump_global_prop_handler                  \n\
 movl $jump_global_prop_handler+2,%ecx                                      \n\
 subl %eax,%ecx                                                             \n\
 movl %ecx,-6(%eax)                                                         \n\
 movl 16(%esp),%ecx # write the global property offset                      \n\
 movl (%ecx),%ecx                                                           \n\
 movl %ecx,-2(%eax)                                                         \n\
");
    }

    gen("\
deoptimize_global_prop:                                                     \n\
 movl 8(%ebx),%eax                                                          \n\
 testl %eax,%eax                                                            \n\
 jnz  deoptimize_global_prop_loop                                           \n\
 popl %ecx                                                                  \n\
deoptimize_global_prop_done:                                                \n\
                                                                            \n\
 popl %ecx                                                                  \n\
 popl %ebx                                                                  \n\
 popl %eax                                                                  \n\
 addl $4,(%esp)                                                             \n\
 ret                                                                        \n\
                                                                            \n\
jump_global_prop_handler:                                                   \n\
 pushl %eax                                                                 \n\
 pushl %ebx                                                                 \n\
 pushl " + ctx_reg + "                                                      \n\
 movb $0," + reg8[arg_count_reg] + "                                        \n\
                                                                            \n\
 movl " + mem(ctx_global_obj_offs, ctx_reg) + ",%ebx                        \n\
 movl " + mem(obj_property_table_offs, "%ebx") + ",%ebx                     \n\
 movl 12(%esp),%eax                                                         \n\
 addl (%eax),%ebx                                                           \n\
                                                                            \n\
 movl (%ebx),%eax     # read global property                                \n\
 andb $" + tag_mask + ",%al          # mask tag                             \n\
 cmpb $" + obj_tag + ",%al          # memory allocated object?              \n\
 jne  jump_global_prop_not_function # property is not a function            \n\
 movl (%ebx),%eax     # read global property again                          \n\
 cmpb $FUNCTION_OBJ_TYPE," + obj_type_offs + "(%eax) # object is a function?\n\
 jne  jump_global_prop_not_function # property is not a function            \n\
                                                                            \n\
 pushl %ebx           # remember global object property location            \n\
 movl 16(%esp),%ebx   # get address of byte just after call to handler      \n\
                                                                            \n\
");

    if (use_arg_count)
    {
        gen("\
 movb 4(%ebx)," + reg8[arg_count_reg] + "     # get arg_count               \n\
 cmpb " + reg8[arg_count_reg] + ",2(%eax)     # arg_count = parameter_count?\n\
 jne  jump_global_prop_check_param_count # must check parameter count       \n\
                                                                            \n\
jump_global_prop_dont_check_param_count:                                    \n\
");
    }

    if (use_ctx_handlers)
    {
        gen("\
 addl $4,%ebx         # point %ebx to patched location list link            \n\
                                                                            \n\
#                      | %ebx                                               \n\
#  7  6  5  4  3  2  1 V                                                    \n\
# FF 51 jj pp pp pp pp nn 00 00 00     <- unoptimized code                  \n\
# E9 zz zz zz zz pp pp yy yy yy yy     <- 'avoid arg count' optimization    \n\
                                                                            \n\
");

        if (use_arg_count)
        {
            gen("\
 cmpb $0x0F,3(%eax)   # 6 byte arg count check jump?                        \n\
 jne  jump_global_prop_dont_check_param_count_jump_skipped                  \n\
 addl $4,%eax         # account for 6 byte jump instead of 2 bytes          \n\
jump_global_prop_dont_check_param_count_jump_skipped:                       \n\
 addl $2+3+2,%eax     # account for relative jump & no arg count setup/check\n\
");
        }
        else
        {
            gen("\
 addl $2,%eax         # account for relative jump & no arg count setup/check\n\
");
        }

        gen("\
 subl %ebx,%eax       # compute relative jump distance                      \n\
 movb $0xE9,-7(%ebx)  # write relative jump                                 \n\
 movl %eax,-6(%ebx)                                                         \n\
");
    }
    else
    {
        gen("\
 addl $2,%ebx         # point %ebx to patched location list link            \n\
                                                                            \n\
#                      | %ebx                                               \n\
#  7  6  5  4  3  2  1 V                                                    \n\
# E8 jj jj jj jj pp pp pp pp nn 00     <- unoptimized code                  \n\
# E9 zz zz zz zz pp pp yy yy yy yy     <- 'avoid arg count' optimization    \n\
                                                                            \n\
");

        if (use_arg_count)
        {
            gen("\
 cmpb $0x0F,3(%eax)   # 6 byte arg count check jump?                        \n\
 jne  jump_global_prop_dont_check_param_count_jump_skipped                  \n\
 addl $4,%eax         # account for 6 byte jump instead of 2 bytes          \n\
jump_global_prop_dont_check_param_count_jump_skipped:                       \n\
 addl $2+3+2,%eax     # account for relative jump & no arg count setup/check\n\
");
        }
        else
        {
            gen("\
 addl $2,%eax         # account for relative jump & no arg count setup/check\n\
");
        }

        gen("\
 subl %ebx,%eax       # compute relative jump distance                      \n\
 movb $0xE9,-7(%ebx)  # write relative jump                                 \n\
 movl %eax,-6(%ebx)                                                         \n\
");
    }

    gen("\
                                                                            \n\
 popl %eax            # get global object property location                 \n\
 pushl " + word_size*2 + "(%eax) # add to list of patched function calls    \n\
 movl %ebx," + word_size*2 + "(%eax)                                        \n\
 popl (%ebx)                                                                \n\
                                                                            \n\
 addl $-7,%ebx        # prepare to jump to generated code                   \n\
 movl %ebx,12(%esp)                                                         \n\
                                                                            \n\
 popl " + ctx_reg + "                                                       \n\
 popl %ebx                                                                  \n\
 popl %eax                                                                  \n\
 ret                                                                        \n\
                                                                            \n\
");

    if (use_arg_count)
    {
        gen("\
jump_global_prop_check_param_count:                                         \n\
");

        if (use_ctx_handlers)
        {
            gen("\
 addl $4,%ebx         # point %ebx to patched location list link            \n\
                                                                            \n\
#                      | %ebx                                               \n\
#  7  6  5  4  3  2  1 V                                                    \n\
# FF 51 jj pp pp pp pp nn 00 00 00     <- unoptimized code                  \n\
# B1 nn E9 xx xx xx xx yy yy yy yy     <- 'keep arg count' optimization     \n\
                                                                            \n\
 movb (%ebx),%cl      # get arg count                                       \n\
 subl %ebx,%eax       # compute relative jump distance                      \n\
 movb $0xE9,-5(%ebx)  # write relative jump                                 \n\
 movb $0xB1,-7(%ebx)  # write movb $n,%cl                                   \n\
 movl %eax,-4(%ebx)                                                         \n\
 movb %cl,-6(%ebx)                                                          \n\
");
        }
        else
        {
            gen("\
 addl $2,%ebx         # point %ebx to patched location list link            \n\
                                                                            \n\
#                      | %ebx                                               \n\
#  7  6  5  4  3  2  1 V                                                    \n\
# E8 jj jj jj jj pp pp pp pp nn 00     <- unoptimized code                  \n\
# B1 nn E9 xx xx xx xx yy yy yy yy     <- 'keep arg count' optimization     \n\
                                                                            \n\
 movb 2(%ebx),%cl     # get arg count                                       \n\
 subl %ebx,%eax       # compute relative jump distance                      \n\
 movb $0xE9,-5(%ebx)  # write relative jump                                 \n\
 movb $0xB1,-7(%ebx)  # write movb $n,%cl                                   \n\
 movl %eax,-4(%ebx)                                                         \n\
 movb %cl,-6(%ebx)                                                          \n\
");
        }

        gen("\
                                                                            \n\
 popl %eax            # get global object property location                 \n\
 pushl " + word_size*2 + "(%eax) # add to list of patched function calls    \n\
 movl %ebx," + word_size*2 + "(%eax)                                        \n\
 popl (%ebx)                                                                \n\
                                                                            \n\
 addl $-7,%ebx        # prepare to jump to generated code                   \n\
 movl %ebx,12(%esp)                                                         \n\
                                                                            \n\
 popl " + ctx_reg + "                                                       \n\
 popl %ebx                                                                  \n\
 popl %eax                                                                  \n\
 ret                                                                        \n\
                                                                            \n\
");
    }

    gen("\
jump_global_prop_not_function:                                              \n\
 movl 4(%ebx),%eax                                                          \n\
 movl (%ebx),%ebx                                                           \n\
 subl $C_FRAME_MULT-16,%esp                                                 \n\
 PRINTF(jump_global_prop_msg1,%eax)                                         \n\
 PRINTF(jump_global_prop_msg2,%ebx)                                         \n\
 EXIT($1)                                                                   \n\
jump_global_prop_msg1:                                                      \n\
 .ascii \"called global property \\\"%s\\\" which is not a function\\n\\0\" \n\
jump_global_prop_msg2:                                                      \n\
 .ascii \"value = %p\\n\\0\"                                                \n\
                                                                            \n\
generic_op_handler:                                                         \n\
 movl 8(%esp),%eax                                                          \n\
 movl 4(%esp),%ebx                                                          \n\
 subl $C_FRAME_MULT-12,%esp                                                 \n\
 PUTS(generic_op_msg1)                                                      \n\
 PRINTF(generic_op_msg2,%eax)                                               \n\
 PRINTF(generic_op_msg3,%ebx)                                               \n\
 EXIT($1)                                                                   \n\
generic_op_msg1:                                                            \n\
 .ascii \"generic_op_handler was called\\0\"                                \n\
generic_op_msg2:                                                            \n\
 .ascii \"value1 = %d\\n\\0\"                                               \n\
generic_op_msg3:                                                            \n\
 .ascii \"value2 = %d\\n\\0\"                                               \n\
                                                                            \n\
 ALIGN_EXPT2(PAGE_SIZE_LOG2)                                                \n\
context:                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
 .long 0                                                                    \n\
                                                                            \n\
");
}

function gen_label(lbl)
{
    gen(lbl + ":\n");
}

function gen_stack_limit_check()
{
    if (use_stack_limit_checks)
    {
        var stack_limit_call_lbl = new_label("");
        var stack_limit_cont_lbl = new_label("");

        gen(" cmpl " + sp_reg + "," + mem(ctx_stack_limit_offs, ctx_reg) + "\n");
        gen(" ja " + stack_limit_call_lbl + "\n");

        defer_code(false,
                   function ()
                   {
                       gen_label(stack_limit_call_lbl);
                       if (use_ctx_handlers)
                       {
                           gen(" call *" + mem(ctx_stack_limit_handler_offs, ctx_reg) + "\n");
                       }
                       else
                       {
                           gen(" call stack_limit_handler\n");
                       }
                       gen(" jmp " + stack_limit_cont_lbl + "\n");
                   });

        gen_label(stack_limit_cont_lbl);
    }
}

function gen_set_arg_count(n)
{
    if (use_arg_count)
    {
        gen(" movb $" + encode_arg_count(n) + "," + reg8[arg_count_reg] + "\n");
    }
}

function gen_global_obj(lbl)
{
    gen("\n");
    gen(" ALIGN_GLOBAL_OBJ\n");
    gen(" .long GLOBAL_OBJ_TYPE\n");
    gen(" .long " + undefined_val + " # prototype\n");
    gen(" .long global_obj_property_table # property table\n");
    gen(" GLOBAL_OBJ_HEADER_END\n");
    gen_label(lbl);

    gen("\n");
    gen(" ALIGN_OBJ\n");
    gen(" .long PROPERTY_TABLE_TYPE\n");
    gen(" PROPERTY_TABLE_HEADER_END\n");
    gen_label("global_obj_property_table");
    gen(" FILLER3\n");

    global_props_ordered.forEach(function (name, i, props)
                                 {
                                     gen("# " + name + "\n");
                                     gen(" .long " + undefined_val + "\n");
                                     gen(" .long " + new_js_string(name) + "\n");
                                     gen(" .long 0 # list of patched jumps\n");
                                 });
}

function gen_js_strings()
{
    for (var str in js_strings)
    {
        gen("\n");
        gen(" ALIGN_OBJ\n");
        gen(" .long STRING_TYPE\n");
        gen(" STRING_HEADER_END\n");
        gen_label(js_strings[str]);
        //gen(" FILLER1\n");
        gen(" .ascii \"" + str + "\\0\"\n");
    }
}

function gen_get_global_prop(name, result_reg)
{
    var index = new_global_prop(name);
    var offs = index * word_size * global_prop_len + property_offs;

    gen(" movl " + mem(ctx_global_obj_offs, ctx_reg) + "," + result_reg + "\n");
    gen(" movl " + mem(obj_property_table_offs, result_reg) + "," + result_reg + "\n");
    gen(" movl " + mem(offs,result_reg) + "," + result_reg + " # " + name + "\n");
}

function gen_put_global_prop(name, val, ctx)
{
    var index = new_global_prop(name);
    var offs = index * word_size * global_prop_len + property_offs;

    ctx.hint = null;

    var x = alloc_reg(ctx);

    ctx = x.ctx;

    var t = get_var(x.result, ctx);

    gen(" movl " + mem(ctx_global_obj_offs, ctx_reg) + "," + t.asm() + "\n");
    gen(" movl " + mem(obj_property_table_offs, t.asm()) + "," + t.asm() + "\n");

    gen_deoptimize_global_prop(name, t.asm());

    gen(" movl " + val + "," + mem(offs,t.asm()) + " # " + name + "\n");

    return { ctx: ctx };
}

function gen_deoptimize_global_prop(name, prop_ptr_reg)
{
    var index = new_global_prop(name);
    var offs = index * word_size * global_prop_len + property_offs;

    if (use_global_prop_handlers)
    {
        var deoptimize_cont_lbl = new_label("");
        var deoptimize_lbl = new_label("");

        gen(" cmpl $0," + mem(offs+2*word_size,prop_ptr_reg) + "\n");
        gen(" jne  " + deoptimize_lbl + "\n");

        defer_code(false,
                   function ()
                   {
                       gen_label(deoptimize_lbl);
                       if (use_ctx_handlers)
                       {
                           gen(" call *" + mem(ctx_deoptimize_global_prop_handler_offs, ctx_reg) + "\n");
                       }
                       else
                       {
                           gen(" call deoptimize_global_prop_handler\n");
                       }
                       gen(" .long " + offs + " # " + name + "\n");
                       gen(" jmp " + deoptimize_cont_lbl + "\n");
                   });

        gen_label(deoptimize_cont_lbl);
    }
}

function gen_jump_global_prop(name, n)
{
    var index = new_global_prop(name);
    var offs = index * word_size * global_prop_len + property_offs;

    if (use_global_prop_handlers)
    {
        if (use_ctx_handlers)
        {
            gen(" call *" + mem(ctx_jump_global_prop_handler_offs, ctx_reg) + "\n");
            gen(" .long " + offs + " # " + name + "\n");
            gen(" .byte " + encode_arg_count(n) + ",0,0,0\n");
        }
        else
        {
            gen(" call jump_global_prop_handler\n");
            gen(" .long " + offs + " # " + name + "\n");
            gen(" .byte " + encode_arg_count(n) + ",0\n");
        }
    }
    else
    {
        gen(" movl " + mem(ctx_global_obj_offs, ctx_reg) + "," + "%esi" + "\n");
        gen(" movl " + mem(obj_property_table_offs, "%esi") + "," + "%esi" + "\n");
        gen_set_arg_count(n);
        gen(" jmp *" + mem(offs,"%esi") + " # " + name + "\n");
    }

    generate_deferred_code(true);
}

function gen_function(lbl, ast, ctx)
{
    gen("\n");
    gen(" ALIGN_FUNCTION\n");
    gen(" .long FUNCTION_OBJ_TYPE\n");
    gen(" .long " + undefined_val + " # prototype\n");
    gen(" .long " + undefined_val + " # property table\n");
    gen(" FUNCTION_OBJ_HEADER_END\n");
    gen_label(lbl);

    if (use_arg_count)
    {
        var nb_params = encode_arg_count(ast.params.length);
        var arg_count_call_lbl = new_label("");
        var arg_count_cont_lbl = new_label("");

        gen(" subb $" + nb_params + "," + reg8[arg_count_reg] + "\n");
        gen(" jne " + arg_count_call_lbl + "\n");

        defer_code(false,
                   function ()
                   {
                       gen_label(arg_count_call_lbl);
                       if (nb_params !== 0)
                           gen(" addb $" + nb_params + "," + reg8[arg_count_reg] + "\n");
                       gen(" pushl " + arg_count_reg + "\n");
                       gen(" xorb " + reg8[arg_count_reg] + "," + reg8[arg_count_reg] + "\n");
                       if (use_ctx_handlers)
                       {
                           gen(" call *" + mem(ctx_arg_count_handler_offs, ctx_reg) + "\n");
                       }
                       else
                       {
                           gen(" call arg_count_handler\n");
                       }
                       gen(" jmp " + arg_count_cont_lbl + "\n");
                   });

        gen_label(arg_count_cont_lbl);
    }

    if (ast.free_vars !== null)
    {
        for (var v in ast.free_vars)
        {
            if (v.scope instanceof Program)
            {
                print("free_var: id=" + ast.free_vars[v].toString());
                error("functions with non global free variables are not implemented yet");
            }
        }
    }

    var x = gen_statements(ast.body, ctx);

    ctx = x.ctx;

    if (x.reachable !== false)
    {
        var res;

        if (use_ctx_constants)
            res = new real_ctx(ctx_undefined_offs);
        else
            res = new real_cst(undefined);

        ctx = gen_return_result(res, ctx);
    }

    generate_deferred_code(false);
}

function real_reg(reg_num)
{
    this.reg_num = reg_num;
}

real_reg.prototype.toString = function () { return all_regs[this.reg_num]; };

real_reg.prototype.asm = function (ctx)
{
    return all_regs[this.reg_num];
};

real_reg.prototype.asm8 = function (ctx)
{
    return reg8[all_regs[this.reg_num]];
};

function real_stk(slot_num)
{
    this.slot_num = slot_num;
}

real_stk.prototype.toString = function () { return "stk[" + this.slot_num + "]"; };

real_stk.prototype.asm = function (ctx)
{
    return mem(-word_size*(this.slot_num + 1 - ctx.frame_size), sp_reg);
};

real_stk.prototype.asm8 = function (ctx)
{
    return this.asm(ctx);
};

function real_ctx(offs)
{
    this.offs = offs;
}

real_ctx.prototype.toString = function () { return "ctx[" + this.offs + "]"; };

real_ctx.prototype.asm = function (ctx)
{
    return mem(this.offs, ctx_reg);
};

real_ctx.prototype.asm8 = function (ctx)
{
    return this.asm(ctx);
};

function real_cst(val)
{
    this.val = val;
}

real_cst.prototype.toString = function () { return String(this.val); };

real_cst.prototype.asm = function (ctx)
{
    return "$" + encode_js_value(this.val);
};

real_cst.prototype.asm8 = function (ctx)
{
    return "$" + (encode_js_value(this.val) & 0xff);
};

var use_count = 0;

function new_use() { return use_count++; }

function new_regs()
{
    var regs = [];

    for (var i=0; i<all_regs.length; i++)
        regs[i] = null;

    return regs;
}

function new_stks()
{
    return [];
}

function invalidate_regs(ctx)
{
    ctx = clone(ctx);
    ctx.loc_map.regs = new_regs();
    return ctx;
}

function loc_map()
{
    this.regs = new_regs();
    this.stks = new_stks();
    this.csts = {};
}

function live_var_after(name, ctx)
{
    return ctx.live_vars_after[name] === true;
}

function lookup_var_in_reg(name, ctx)
{
    var i = ctx.loc_map.regs.length-1;

    while (i >= 0)
    {
        var reg = ctx.loc_map.regs[i];
        if (reg !== null && reg.name === name)
            break;
        i--;
    }

    return i;
}

function lookup_var_in_stk(name, ctx)
{
    var i = ctx.loc_map.stks.length-1;

    while (i >= 0)
    {
        var stk = ctx.loc_map.stks[i];
        if (stk !== null && stk.name === name)
            break;
        i--;
    }

    return i;
}

function lookup_var_in_cst(name, ctx)
{
    if (ctx.loc_map.csts.hasOwnProperty(name))
        return name;

    return false;
}

function free_stk(ctx)
{
    var i = 0;

    while (i < ctx.loc_map.stks.length)
    {
        var stk = ctx.loc_map.stks[i];
        if (stk === null)
            break;
        i++;
    }

    return { ctx: ctx, slot_num: i };
}

function alloc_reg(ctx)
{
    var i;
    var r;

    // try to find an unassigned register or one containing a
    // variable that is not live after or with a copy on the stack

    if (ctx.hint !== null &&
        (ctx.loc_map.regs[ctx.hint.reg_num] === null ||
         !live_var_after(ctx.loc_map.regs[ctx.hint.reg_num].name, ctx) ||
         lookup_var_in_stk(ctx.loc_map.regs[ctx.hint.reg_num].name, ctx) >= 0))
        r = ctx.hint.reg_num;
    else
    {
        i = arg1_reg_num; //FIXME: should start at 0, but must avoid registers without a low 8 bit reg
        while (i < ctx.loc_map.regs.length)
        {
            var reg = ctx.loc_map.regs[i];
            if (reg === null || // unassigned register
                !live_var_after(reg.name, ctx)) // register's content not live after
            {
                r = i;
                break;
            }

            i++;
        }

        if (r < 0)
        {
            // try to find an assigned register whose content is also
            // on the stack

            i = arg1_reg_num; //FIXME: should start at 0, but must avoid registers without a low 8 bit reg
            while (i < ctx.loc_map.regs.length)
            {
                var reg = ctx.loc_map.regs[i];
                if (lookup_var_in_stk(reg.name, ctx) >= 0) // register's content is also on stack
                {
                    r = i;
                    break;
                }

                i++;
            }
        }

        if (r < 0)
        {
            // find the assigned register which has been used the
            // longest ago

            var r_use = 9999999999;

            i = arg1_reg_num; //FIXME: should start at 0, but must avoid registers without a low 8 bit reg
            while (i < ctx.loc_map.regs.length)
            {
                var reg = ctx.loc_map.regs[i];
                if (reg.use < r_use)
                {
                    r = i;
                    r_use = reg.use;
                }

                i++;
            }

            // register must be saved on stack

            ctx = gen_save_reg_on_stack(r, ctx);
        }
    }

    ctx = clone(ctx);

    var result_name = new_temp();

    ctx.loc_map.regs[r] = { name: result_name, use: new_use() };

    return { ctx: ctx, result: result_name };
}

function alloc_reg_move(name, ctx)
{
    var r;
    var t;
    var result_name;

    if (ctx.hint !== null)
    {
        r = ctx.hint.reg_num;

        if (ctx.loc_map.regs[r] === null ||
            !live_var_after(ctx.loc_map.regs[r].name, ctx) ||
            lookup_var_in_stk(ctx.loc_map.regs[r].name, ctx) >= 0 ||
            ctx.loc_map.regs[r].name === name)
        {
            if (ctx.loc_map.regs[r] !== null &&
                live_var_after(ctx.loc_map.regs[r].name, ctx) &&
                lookup_var_in_stk(ctx.loc_map.regs[r].name, ctx) < 0)
            {
                ctx = gen_save_reg_on_stack(r, ctx);
            }

            if (ctx.loc_map.regs[r] === null ||
                ctx.loc_map.regs[r].name !== name)
            {
                t = get_var(name, ctx);
                ctx = gen_move(t, new real_reg(r), ctx);
            }

            result_name = new_temp();

            ctx.loc_map.regs[r] = { name: result_name, use: new_use() };

            return { ctx: ctx, result: result_name };
        }
    }

    r = lookup_var_in_reg(name, ctx);

    if (r >= 0)
    {
        if (live_var_after(name, ctx) &&
            lookup_var_in_stk(name, ctx) < 0)
        {
            ctx = gen_save_reg_on_stack(r, ctx);
        }

        result_name = new_temp();

        ctx.loc_map.regs[r] = { name: result_name, use: new_use() };

        return { ctx: ctx, result: result_name };
    }

    var x = alloc_reg(ctx);

    ctx = x.ctx;

    ctx = gen_move(get_var(name, ctx), get_var(x.result, ctx), ctx);

    return { ctx: ctx, result: x.result };
}

function gen_add_cst(n, reg)
{
    if (n !== 0)
    {
        gen(" addl $" + n + "," + reg + "\n");
        //gen(" lea " + mem(n,reg) + "," + reg + "\n");
    }
}

function gen_move(src, dst, ctx)
{
    if ((src instanceof real_reg &&
         dst instanceof real_reg &&
         src.reg_num === dst.reg_num) ||
        (src instanceof real_stk &&
         dst instanceof real_stk &&
         src.slot_num === dst.slot_num) ||
        (src instanceof real_ctx &&
         dst instanceof real_ctx &&
         src.offs === dst.offs))
        return ctx;

    gen(" movl " + src.asm(ctx) + "," + dst.asm(ctx) + "\n");

    return ctx;
}

function gen_save_reg_on_stack(reg_num, ctx)
{
    var x = free_stk(ctx); // find an unused stack slot

    ctx = clone(x.ctx);

    ctx = gen_move(new real_reg(reg_num), new real_stk(x.slot_num), ctx);

    ctx.loc_map.stks[x.slot_num] = { name: ctx.loc_map.regs[reg_num].name };

    return ctx;
}

function gen_save_live_regs_on_stack(ctx)
{
    var i = 0;

    while (i < ctx.loc_map.regs.length)
    {
        var reg = ctx.loc_map.regs[i];
        if (reg !== null && // assigned register
            live_var_after(reg.name, ctx) && // register's content live after
            lookup_var_in_stk(reg.name, ctx) < 0) // register's content not on stack
        {
            ctx = gen_save_reg_on_stack(i, ctx);
        }

        i++;
    }

    return ctx;
}

function gen_return_result(result, ctx)
{
    ctx = gen_move(result, new real_reg(result_reg_num), ctx);

    var ra = get_var(ra_name, ctx);

    if (sp_reg === "%esp" &&
        ra instanceof real_stk &&
        ra.slot_num === 0)
    {
        gen_add_cst((ctx.frame_size-1)*word_size, sp_reg);

        ctx.frame_size = 0;

        gen(" ret\n");
    }
    else
    {
        gen_add_cst(ctx.frame_size*word_size, sp_reg);

        ctx.frame_size = 0;

        gen(" jmp *" + ra.asm(ctx) + "\n");
    }

    generate_deferred_code(true);
}

var temp_counter = 0;

function new_temp()
{
    return " T" + temp_counter++;
}

function get_var(name, ctx)
{
    var r = lookup_var_in_reg(name, ctx);

    if (r >= 0)
        return new real_reg(r);

    var s = lookup_var_in_stk(name, ctx);

    if (s >= 0)
        return new real_stk(s);

    var c = lookup_var_in_cst(name, ctx);

    if (c !== false)
        return new real_cst(ctx.loc_map.csts[name]);

    error("variable is not in a register or stack slot or a constant! var=" + name);
}

function fn_ctx(top, params, uses_this)
{
    this.top = top;
    this.loc_map = new loc_map();
    this.live_vars_after = {};
    this.frame_size = 0;

    this.loc_map.regs[ra_reg_num] = { name: ra_name, use: new_use() };
    this.live_vars_after[ra_name] = true;

    this.loc_map.regs[this_reg_num] = { name: this_name, use: new_use() };
    this.live_vars_after[this_name] = uses_this;

    for (var i=0; i<params.length; i++)
    {
        var name = params[i].toString();
        this.loc_map.regs[arg1_reg_num+i] = { name: name, use: new_use() };
        this.live_vars_after[name] = true; ///////////////////
    }

    this.hint = new real_reg(result_reg_num); // needed?
}

function gen_statement(ast, ctx)
{
    if (ast === null)
    {
        // no code generation
    }
    else if (ast instanceof Program)
    {
        var end_lbl = new_label("end");

        var global_obj_lbl = new_label("global_obj");

        var top_fn_lbl = new_label("top_fn");

        var top_fn_ast = new FunctionExpr(ast.loc,
                                          null,
                                          [],
                                          ast.block.statements);

        var top_fn_ctx = new fn_ctx(true, [], false);

        gen_init_runtime(global_obj_lbl, top_fn_lbl, end_lbl);

        gen_function(top_fn_lbl, top_fn_ast, top_fn_ctx);

        generate_deferred_obj();

        gen_global_obj(global_obj_lbl);

        gen_js_strings();

        gen("\n");
        gen_label(end_lbl);

        print(text_section.join(""));
        print(data_section.join(""));

        return null;
    }
    else if (ast instanceof FunctionDeclaration)
    {
        var id = ast.id;

        if (id.scope instanceof Program)
        {
            // Toplevel function declaration

            var funct_lbl = new_label(id.toString());

            var funct_ast = ast.funct;

            var funct_ctx = new fn_ctx(true, ast.funct.params, false);

            defer_obj(function ()
                      {
                          gen_function(funct_lbl, funct_ast, funct_ctx);
                      });

            ctx.hint = new real_reg(result_reg_num);

            var x1 = alloc_reg(ctx);

            ctx = x1.ctx;

            var save_lva = ctx.live_vars_after;

            ctx.live_vars_after = clone(ctx.live_vars_after);
            ctx.live_vars_after[x1.result] = true; // keep result live

            var t1 = get_var(x1.result, ctx);

            gen(" movl " + "$" + funct_lbl + "," + t1.asm() + "\n");

            var x2 = gen_put_global_prop(id.toString(), t1.asm(), ctx);

            ctx = x2.ctx;

            ctx.live_vars_after = save_lva;

            return { ctx: ctx, reachable: true };
        }
        else
        {
            error("nested FunctionDeclaration is not implemented");
        }
    }
    else if (ast instanceof BlockStatement)
    {
        return gen_statements(ast.statements, ctx);
    }
    else if (ast instanceof ConstStatement)
    {
        // TODO
        error("ConstStatement not implemented");
    }
    else if (ast instanceof ExprStatement)
    {
        ctx.hint = null;

        var x = gen_expr(ast.expr, ctx);

        ctx = x.ctx;

        return { ctx: ctx, reachable: true };
    }
    else if (ast instanceof IfStatement)
    {
        var then_lbl = new_label("then");
        var else_lbl = new_label("else");

        if (ast.statements.length === 1)
        {
            var x = gen_expr_if(ast.expr, false, then_lbl, else_lbl, ctx)

            ctx = x.ctx;

            x = gen_statement(ast.statements[0], ctx);

            ctx = x.ctx;

            gen_label(else_lbl);

            return { ctx: ctx, reachable: true };
        }
        else
        {
            var join_lbl;

            var x = gen_expr_if(ast.expr, false, then_lbl, else_lbl, ctx)

            ctx = x.ctx;

            var branch1 = gen_statement(ast.statements[0], ctx);

            if (branch1.reachable)
            {
                join_lbl = new_label("join");
                gen(" jmp " + join_lbl + "\n");

                generate_deferred_code(true);
            }

            gen_label(else_lbl);

            var branch2 = gen_statement(ast.statements[1], ctx);

            if (branch1.reachable)
            {
                gen_label(join_lbl);
                if (branch2.reachable)
                    error("context merging not yet implemented");
            }

            ctx = branch1.ctx; //FIXME: merge with branch2.ctx!!!!

            return { ctx: ctx, reachable: (branch1.reachable || branch2.reachable) };
        }
    }
    else if (ast instanceof DoWhileStatement)
    {
        // TODO
        error("DoWhileStatement not implemented");
    }
    else if (ast instanceof WhileStatement)
    {
        // TODO
        error("WhileStatement not implemented");
    }
    else if (ast instanceof ForStatement)
    {
        // TODO
        error("ForStatement not implemented");
    }
    else if (ast instanceof ForInStatement)
    {
        // TODO
        error("ForInStatement not implemented");
    }
    else if (ast instanceof ContinueStatement)
    {
        // TODO
        error("ContinueStatement not implemented");

        return { ctx: ctx, reachable: false }; // code after continue is unreachable
    }
    else if (ast instanceof BreakStatement)
    {
        // TODO
        error("BreakStatement not implemented");

        return { ctx: ctx, reachable: false }; // code after break is unreachable
    }
    else if (ast instanceof ReturnStatement)
    {
        //gen_puts("ReturnStatement");

        if (ast.expr !== null)
        {
            // Tail position

            //////////////////////////FIXME: handle tail calls

            var x = gen_expr(ast.expr, ctx);

            ctx = x.ctx;

            var t = get_var(x.result, ctx);

            ctx = gen_return_result(t, ctx);
        }
        else
        {
            ctx = gen_return_result(new real_cst(undefined), ctx);
        }

        return { ctx: ctx, reachable: false }; // code after return is unreachable
    }
    else if (ast instanceof WithStatement)
    {
        // TODO
        error("WithStatement not implemented");
    }
    else if (ast instanceof SwitchStatement)
    {
        // TODO
        error("SwitchStatement not implemented");
    }
    else if (ast instanceof LabelledStatement)
    {
        // TODO
        error("LabelledStatement not implemented");
    }
    else if (ast instanceof ThrowStatement)
    {
        // TODO
        error("ThrowStatement not implemented");
    }
    else if (ast instanceof TryStatement)
    {
        // TODO
        error("TryStatement not implemented");
    }
    else if (ast instanceof CatchPart)
    {
        // TODO
        error("CatchPart not implemented");
    }
    else if (ast instanceof DebuggerStatement)
    {
        // TODO
        error("DebuggerStatement not implemented");
    }
    else
    {
        //pp(ast);
        error("unknown ast in gen_statement");
    }
}

function gen_statements(asts, ctx)
{
    for (var i=0; i<asts.length; i++)
    {
        var ast = asts[i];
        var x = gen_statement(ast, ctx);

        if (x.reachable === false)
            return x;

        ctx = x.ctx;
    }

    return { ctx: ctx, reachable: true };
}

function gen_expr_op(op, expr1, expr2, ctx)
{
    // use same hint

    var x2 = gen_expr(expr2, ctx);

    ctx = x2.ctx;

    var save_lva = ctx.live_vars_after;

    ctx.live_vars_after = clone(ctx.live_vars_after);
    ctx.live_vars_after[x2.result] = true; // keep result live

    ctx.hint = null;

    var x1 = gen_expr(expr1, ctx);

    ctx = x1.ctx;

    ctx.live_vars_after = save_lva;

    var t1 = get_var(x1.result, ctx);
    var t2 = get_var(x2.result, ctx);

    var do_op;
    var undo_op;

    if (op === "x + y")
    {
        do_op = " addl ";
        undo_op = " subl ";
    }
    else
    {
        do_op = " subl ";
        undo_op = " addl ";
    }

    var op_generic_lbl = new_label("");
    var op_overflow_generic_lbl = new_label("");
    var op_cont_lbl = new_label("");

    gen(" testb $3," + t1.asm8(ctx) + "\n");
    gen(" jne " + op_generic_lbl + "\n");
    gen(" testb $3," + t2.asm8(ctx) + "\n");
    gen(" jne " + op_generic_lbl + "\n");
    gen(do_op + t2.asm(ctx) + "," + t1.asm(ctx) + "\n");
    gen(" jo " + op_overflow_generic_lbl + "\n");

    defer_code(false,
               function ()
               {
                   gen_label(op_overflow_generic_lbl);
                   gen(undo_op + t2.asm(ctx) + "," + t1.asm(ctx) + "\n");
                   gen_label(op_generic_lbl);
                   gen(" pushl " + t1.asm(ctx) + "\n");
                   gen(" pushl " + t2.asm(ctx) + "\n");
                   if (use_ctx_handlers)
                   {
                       gen(" call *" + mem(ctx_generic_op_handler_offs, ctx_reg) + "\n");
                   }
                   else
                   {
                       gen(" call generic_op_handler\n");
                   }
                   gen(" jmp " + op_cont_lbl + "\n");
               });

    gen_label(op_cont_lbl);

    return { ctx: ctx, result: x1.result };
}

function gen_expr_op_cst(op, expr, value, ctx)
{
    // use same hint

    var x1 = gen_expr(expr, ctx);

    ctx = x1.ctx;

    var t1 = get_var(x1.result, ctx);

    ctx.hint = null;

    x1 = alloc_reg_move(x1.result, ctx);

    ctx = x1.ctx;

    t1 = get_var(x1.result, ctx);

    var do_op;
    var undo_op;

    if (op === "x + y")
    {
        do_op = " addl ";
        undo_op = " subl ";
    }
    else
    {
        do_op = " subl ";
        undo_op = " addl ";
    }

    var op_generic_lbl = new_label("");
    var op_overflow_generic_lbl = new_label("");
    var op_cont_lbl = new_label("");

    gen(" testb $3," + t1.asm8(ctx) + "\n");
    gen(" jne " + op_generic_lbl + "\n");
    gen(do_op + "$" + encode_js_value(value) + "," + t1.asm(ctx) + "\n");
    gen(" jo " + op_overflow_generic_lbl + "\n");

    defer_code(false,
               function ()
               {
                   gen_label(op_overflow_generic_lbl);
                   gen(undo_op + "$" + encode_js_value(value) + "," + t1.asm(ctx) + "\n");
                   gen_label(op_generic_lbl);
                   gen(" pushl " + t1.asm(ctx) + "\n");
                   gen(" pushl $" + encode_js_value(value) + "\n");
                   if (use_ctx_handlers)
                   {
                       gen(" call *" + mem(ctx_generic_op_handler_offs, ctx_reg) + "\n");
                   }
                   else
                   {
                       gen(" call generic_op_handler\n");
                   }
                   gen(" jmp " + op_cont_lbl + "\n");
               });

    gen_label(op_cont_lbl);

    return { ctx: ctx, result: x1.result };
}

function gen_expr(ast, ctx)
{
    if (ast === null)
    {
        // no transformation
    }
    else if (ast instanceof OpExpr)
    {
        if (ast.op === "x = y")
        {
            var dst = ast.exprs[0];

            if (dst instanceof Ref &&
                dst.id.scope instanceof Program)
            {
                // use same hint

                var x1 = gen_expr(ast.exprs[1], ctx);

                ctx = x1.ctx;

                var save_lva = ctx.live_vars_after;

                ctx.live_vars_after = clone(ctx.live_vars_after);
                ctx.live_vars_after[x1.result] = true; // keep result live

                var t1 = get_var(x1.result, ctx);

                var x2 = gen_put_global_prop(dst.id.toString(), t1.asm(), ctx);

                ctx = x2.ctx;

                ctx.live_vars_after = save_lva;

                return { ctx: ctx, result: x1.result };
            }
            else
            {
                error("assignment to a non-global property is not implemented");
            }
        }
        else if (ast.op === "x + y" ||
                 ast.op === "x - y")
        {
            if (ast.exprs[0] instanceof Literal && ast.op === "x + y")
                return gen_expr_op_cst(ast.op, ast.exprs[1], ast.exprs[0].value, ctx);
            else if (ast.exprs[1] instanceof Literal)
                return gen_expr_op_cst(ast.op, ast.exprs[0], ast.exprs[1].value, ctx);
            else
                return gen_expr_op(ast.op, ast.exprs[0], ast.exprs[1], ctx);
        }
        else
            error("unimplemented OpExpr " + ast.op);
    }
    else if (ast instanceof NewExpr)
    {
        // TODO
        error("NewExpr not implemented");
        //ast.expr = ctx.walk_expr(ast.expr);
        //ast.args = gen_exprs(ast.args, ctx);
    }
    else if (ast instanceof CallExpr)
    {
        if (ast.fn instanceof Ref &&
            ast.fn.id.scope instanceof Program)
        {
            if (false)
            {
                var tub_lbl = new_label("");

                gen(" jmp " + tub_lbl + "\n");
                gen(" .long 123\n");
                gen_label(tub_lbl);
            }

            var rp_lbl = new_label("");

            var save_lva = ctx.live_vars_after;

            var args = [];

            ast.args.forEach(function (expr_ast, i, asts)
                             {
                                 var x = gen_expr(expr_ast, ctx);

                                 ctx = x.ctx;

                                 ctx.live_vars_after = clone(ctx.live_vars_after);
                                 ctx.live_vars_after[x.result] = true; // keep result live

                                 args[i] = x.result;
                             });

            ctx.live_vars_after = save_lva;

            ctx = gen_save_live_regs_on_stack(ctx);

            ast.args.forEach(function (expr_ast, i, asts)
                             {
                                 var t = get_var(args[i], ctx);

                                 ctx = gen_move(t, new real_reg(arg1_reg_num+i), ctx);
                             });

            gen_add_cst((ctx.frame_size-ctx.loc_map.stks.length)*word_size, sp_reg);

            ctx.frame_size = ctx.loc_map.stks.length;

            gen_stack_limit_check();

            gen(" movl $" + rp_lbl + "," + ra_reg + "\n");
            gen(" movl " + mem(ctx_global_obj_offs, ctx_reg) + "," + this_reg + "\n");
            gen_jump_global_prop(ast.fn.id.toString(), ast.args.length);

            gen(" ALIGN_RET_POINT\n");
            gen(" FRAME_DESCR\n");
            gen(" .long RET_POINT_TYPE\n");
            gen(" RET_POINT_HEADER_END\n");
            gen_label(rp_lbl);

            ctx = invalidate_regs(ctx);

            var result_name = new_temp();

            ctx.loc_map.regs[result_reg_num] = { name: result_name, use: new_use() };

            return { ctx: ctx, result: result_name };
        }
        else
        {
            error("CallExpr to a non-global property is not implemented");
            //ast.fn = ctx.walk_expr(ast.fn);
            //ast.args = gen_exprs(ast.args, ctx);
        }
    }
    else if (ast instanceof FunctionExpr)
    {
        //////////////FIXME
        var fn_lbl = new_label("");

        gen_function(fn_lbl, ast, ctx);

        //ast.body = gen_statements(ast.body, ctx);
    }
    else if (ast instanceof Literal)
    {
        var result_name = new_temp();

        ctx = clone(ctx);

        ctx.loc_map.csts[result_name] = ast.value;

        return { ctx: ctx, result: result_name };
    }
    else if (ast instanceof ArrayLiteral)
    {
        // TODO
        error("ArrayLiteral not implemented");
        //ast.exprs = gen_exprs(ast.exprs, ctx);
    }
    else if (ast instanceof ObjectLiteral)
    {
        // TODO
        error("ObjectLiteral not implemented");
        //ast.properties.forEach(function (prop, i, self)
        //                       {
        //                           prop.name = ctx.walk_expr(prop.name);
        //                           prop.value = ctx.walk_expr(prop.value);
        //                       });
    }
    else if (ast instanceof Ref)
    {
        if (ast instanceof Ref &&
            ast.id.scope instanceof Program)
        {
            // use same hint

            var x = alloc_reg(ctx);

            ctx = x.ctx;

            var t = get_var(x.result, ctx);

            gen_get_global_prop(ast.id.toString(), t.asm(ctx));

            return { ctx: ctx, result: x.result };
        }
        else
        {
            return { ctx: ctx, result: ast.id.toString() };
        }
    }
    else if (ast instanceof This)
    {
        return { ctx: ctx, result: this_name };
    }
    else
    {
        error("unimplemented expression in gen_expr");
    }
}

function gen_expr_if(ast, invert, true_lbl, false_lbl, ctx)
{
    if (ast === null)
    {
        // no transformation
    }
    else if (ast instanceof OpExpr)
    {
        if (ast.op === "x < y")
        {
            if (ast.exprs[0] instanceof Literal)
                return gen_expr_if_op_cst(ast.op, ast.exprs[1], ast.exprs[0].value, !invert, true_lbl, false_lbl, ctx);
            else if (ast.exprs[1] instanceof Literal)
                return gen_expr_if_op_cst(ast.op, ast.exprs[0], ast.exprs[1].value, invert, true_lbl, false_lbl, ctx);
            else
                return gen_expr_if_op(ast.op, ast.exprs[0], ast.exprs[1].value, invert, true_lbl, false_lbl, ctx);
        }
        else
            error("unimplemented OpExpr " + ast.op);
    }
    else
    {
        error("unimplemented expression in gen_expr_if");
    }
}

function gen_expr_if_op_cst(op, expr, value, invert, true_lbl, false_lbl, ctx)
{
    var x = gen_expr(expr, ctx);

    ctx = x.ctx;

    var t = get_var(x.result, ctx);

    var op_generic_lbl = new_label("");

    var opnd = t.asm(ctx);

    gen(" testb $3," + reg8[opnd] + "\n");
    gen(" jne " + op_generic_lbl + "\n");
    gen(" cmpl " + "$" + encode_js_value(value) + "," + opnd + "\n");
    gen((invert?" jl ":" jge ") + false_lbl + "\n");

    defer_code(false,
               function ()
               {
                   gen_label(op_generic_lbl);
                   gen(" pushl " + opnd + "\n");
                   gen(" pushl $" + encode_js_value(value) + "\n");
                   if (use_ctx_handlers)
                   {
                       gen(" call *" + mem(ctx_generic_op_handler_offs, ctx_reg) + "\n");
                   }
                   else
                   {
                       gen(" call generic_op_handler\n");
                   }
                   gen((!invert?" jl ":" jge ") + true_lbl + "\n");
                   gen(" jmp " + false_lbl + "\n");
               });

    gen_label(true_lbl);

    return { ctx: ctx, result: x.result };
}

function js86_compile(ast)
{
    use_arg_count            = true !== options["no-arg-count"];
    use_global_prop_handlers = true !== options["no-global-prop-handlers"];
    use_stack_limit_checks   = true !== options["no-stack-limit-checks"];
    use_ctx_handlers         = true === options["ctx-handlers"];
    use_ctx_constants        = true === options["ctx-constants"];
    use_distinguish_hot_cold = true === options["distinguish-hot-cold"];

    gen_statement(ast, null)
}

//=============================================================================
