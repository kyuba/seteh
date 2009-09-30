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
#include <curie/tree.h>

#ifdef __cplusplus
extern "C" {
#endif

enum primitive_ops
{
    op_lambda,
    op_mu,
    op_addition,
    op_subtraction,
    op_multiplication,
    op_division,
    op_modulo,
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

struct foreign_lambda
{
    unsigned int type;
    sexpr name;
    sexpr (*f) (sexpr, struct machine_state *);
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

struct sexpr_io *stdio;

void lx_sx_map_call (struct tree_node *node, void *u);
void initialise_seteh_environment ( void );
void initialise_seteh_lambda      ( void );
void initialise_seteh_promise     ( void );
void initialise_seteh_eval        ( void );
void initialise_seteh_state       ( void );

sexpr lx_make_automatic_promise (sexpr code, sexpr environment);

#ifdef __cplusplus
}
#endif

#endif
