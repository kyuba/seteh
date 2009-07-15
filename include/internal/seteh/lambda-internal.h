/*
 * This file is part of the kyuba.org Seteh project.
 * See the appropriate repository at http://git.kyuba.org/ for exact file
 * modification records.
*/

/*
 * Copyright (c) 2009, Kyuba Project Members
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
*/

#ifndef LIBSETEH_LAMBDA_INTERNAL_H
#define LIBSETEH_LAMBDA_INTERNAL_H

#include <curie/sexpr.h>

#ifdef __cplusplus
extern "C" {
#endif

#if 0
#define lambda_argument_base 0xf0000
#endif

#define lambda_argument_base 0x2801

enum primitive_ops
{
    op_lambda,
    op_mu,
    op_addition,
    op_subtraction,
    op_multiplication,
    op_division,
    op_modulo,
    op_dereference,
    op_unbound,
    op_equalp,
    op_if,
    op_gt,
    op_gte,
    op_lt,
    op_lte,
    op_equals,
    op_delay,
    op_force,
    op_eval
};

enum promise_type
{
    pt_automatic,
    pt_manual
};

struct lambda
{
    unsigned int type;
    sexpr arguments;
    sexpr code;
    sexpr environment;
};

struct environment
{
    unsigned int type;
    sexpr environment;
};

struct primitive
{
    unsigned int type;
    enum primitive_ops op;
};

struct promise
{
    unsigned int type;
    enum promise_type ptype;
    sexpr environment;
    sexpr code;
};

#define define_primitive(o,s,v) \
    static const struct primitive sexpr_payload_sx_p_ ## o\
       = { primitive_type_identifier, o };\
    static const sexpr sx_p_ ## o = ((const sexpr)&(sexpr_payload_sx_p_ ## o));\
    define_symbol(s,v)

define_symbol (sym_bad_primitive,        "bad-primitive");
define_symbol (sym_if_alt,               "if");

define_primitive (op_lambda,         sym_lambda,      "lambda");
define_primitive (op_mu,             sym_mu,          "mu");
define_primitive (op_addition,       sym_plus,        "+");
define_primitive (op_subtraction,    sym_minus,       "-");
define_primitive (op_multiplication, sym_multiply,    "*");
define_primitive (op_division,       sym_divide,      "/");
define_primitive (op_modulo,         sym_modulo,      "%");
define_primitive (op_dereference,    sym_dereference, "dereference");
define_primitive (op_unbound,        sym_unbound,     "unbound");
define_primitive (op_equalp,         sym_equalp,      "equal?");
define_primitive (op_if,             sym_if,          "?");
define_primitive (op_gt,             sym_gt,          ">?");
define_primitive (op_gte,            sym_gte,         ">=?");
define_primitive (op_lt,             sym_lt,          "<?");
define_primitive (op_lte,            sym_lte,         "<=?");
define_primitive (op_equals,         sym_equals,      "=?");
define_primitive (op_delay,          sym_delay,       "delay");
define_primitive (op_force,          sym_force,       "force");
define_primitive (op_eval,           sym_eval,        "eval");

/* struct sexpr_io *stdio;*/

#ifdef __cplusplus
}
#endif

#endif
