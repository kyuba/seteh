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
    op_unbound
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

define_symbol (sym_bad_primitive,        "bad-primitive");
define_symbol (sym_lambda,               "lambda");
define_symbol (sym_mu,                   "mu");
define_symbol (sym_plus,                 "+");
define_symbol (sym_minus,                "-");
define_symbol (sym_multiply,             "*");
define_symbol (sym_divide,               "/");
define_symbol (sym_modulo,               "%");
define_symbol (sym_dereference,          "dereference");
define_symbol (sym_unbound,              "unbound");

/* struct sexpr_io *stdio;*/

#ifdef __cplusplus
}
#endif

#endif
