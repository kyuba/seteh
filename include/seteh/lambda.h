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

#ifndef LIBSETEH_LAMBDA_H
#define LIBSETEH_LAMBDA_H

#include <curie/sexpr.h>

#ifdef __cplusplus
extern "C" {
#endif

#define lambda_type_identifier         0x03bb
#define mu_type_identifier             0x03bc
#define foreign_lambda_type_identifier 0x03be
#define foreign_mu_type_identifier     0x03bf
#define environment_type_identifier    0x03b5
#define primitive_type_identifier      0x03c6
#define promise_type_identifier        0x03c7

#define lambdap(sx)      sx_customp(sx,lambda_type_identifier)
#define mup(sx)          sx_customp(sx,mu_type_identifier)
#define flambdap(sx)     sx_customp(sx,foreign_lambda_type_identifier)
#define fmup(sx)         sx_customp(sx,foreign_mu_type_identifier)
#define environmentp(sx) sx_customp(sx,environment_type_identifier)
#define primitivep(sx)   sx_customp(sx,primitive_type_identifier)
#define promisep(sx)     sx_customp(sx,promise_type_identifier)

void initialise_seteh ( void );

sexpr lx_lambda             (sexpr sx, sexpr env);
sexpr lx_mu                 (sexpr sx, sexpr env);
sexpr lx_foreign_lambda     (sexpr name, sexpr (*f)(sexpr, sexpr *));
sexpr lx_foreign_mu         (sexpr name, sexpr (*f)(sexpr, sexpr *));
sexpr lx_apply              (sexpr sx, sexpr args, sexpr *env);
sexpr lx_eval               (sexpr sx, sexpr *env);
sexpr lx_make_environment   (sexpr env);
sexpr lx_environment_lookup (sexpr env, sexpr key);
sexpr lx_environment_unbind (sexpr env, sexpr key);
sexpr lx_environment_bind   (sexpr env, sexpr key, sexpr value);
sexpr lx_make_promise       (sexpr code, sexpr environment);

#ifdef __cplusplus
}
#endif

#endif
