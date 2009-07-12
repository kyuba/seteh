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

#include <seteh/lambda.h>
#include <seteh/lambda-internal.h>
#include <curie/memory.h>

static char initialised = 0;

static sexpr lambda_serialise        (sexpr lambda);
static sexpr lambda_unserialise      (sexpr lambda);
static void  lambda_tag              (sexpr lambda);
static void  lambda_destroy          (sexpr lambda);
static void  lambda_call             ( void );
static sexpr lambda_equalp           (sexpr a, sexpr b);

static sexpr mu_serialise            (sexpr mu);
static sexpr mu_unserialise          (sexpr mu);
static void  mu_tag                  (sexpr mu);
static void  mu_destroy              (sexpr mu);
static void  mu_call                 ( void );
static sexpr mu_equalp               (sexpr a, sexpr b);

static sexpr environment_serialise   (sexpr env);
static sexpr environment_unserialise (sexpr env);
static void  environment_tag         (sexpr env);
static void  environment_destroy     (sexpr env);
static void  environment_call        ( void );
static sexpr environment_equalp      (sexpr a, sexpr b);

static sexpr primitive_serialise     (sexpr op);
static sexpr primitive_unserialise   (sexpr op);
static void  primitive_tag           (sexpr op);
static void  primitive_destroy       (sexpr op);
static void  primitive_call          ( void );
static sexpr primitive_equalp        (sexpr a, sexpr b);

void initialise_seteh ( void )
{
    if (!initialised)
    {
        sx_register_type
                (lambda_type_identifier,
                 lambda_serialise, lambda_unserialise,
                 lambda_tag, lambda_destroy,
                 lambda_call, lambda_equalp);

        sx_register_type
                (mu_type_identifier,
                 mu_serialise, mu_unserialise,
                 mu_tag, mu_destroy,
                 mu_call, mu_equalp);

        sx_register_type
                (environment_type_identifier,
                 environment_serialise, environment_unserialise,
                 environment_tag, environment_destroy,
                 environment_call, environment_equalp);

        sx_register_type
                (primitive_type_identifier,
                 primitive_serialise, primitive_unserialise,
                 primitive_tag, primitive_destroy,
                 primitive_call, primitive_equalp);
    }
}

static sexpr lx_code (sexpr args, sexpr code)
{
    return code;
}

static sexpr make_lambda (unsigned int type, sexpr args)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct lambda));
    struct lambda *lx = get_pool_mem (&pool);
    sexpr arguments, code, t;

    if (lx == (struct lambda *)0)
    {
        return sx_nonexistent;
    }

    arguments = car (args);
    code      = cdr (args);

    lx->type      = type;
    lx->arguments = 0;

    if (consp (arguments))
    {
        t = arguments;

        while (consp (t))
        {
            lx->arguments++;
            t = cdr (arguments);
        }
    }
    else if (integerp (arguments))
    {
        lx->arguments = sx_integer (arguments);
    }

    lx->code = lx_code(args, code);

    return (sexpr)lx;
}

sexpr lx_lambda (sexpr expression)
{
    return lambda_unserialise (expression);
}

sexpr lx_mu (sexpr expression)
{
    return mu_unserialise (expression);
}

static sexpr lambda_serialise (sexpr lambda)
{
    struct lambda *lx = (struct lambda *)lambda;

    return cons (make_integer(lx->arguments), lx->code);
}

static sexpr lambda_unserialise (sexpr lambda)
{
    return make_lambda (lambda_type_identifier, lambda);
}

static void lambda_tag (sexpr lambda)
{
}

static void lambda_destroy (sexpr lambda)
{
}

static void lambda_call ( void )
{
}

static sexpr lambda_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

static sexpr mu_serialise (sexpr mu)
{
    return lambda_serialise (mu);
}

static sexpr mu_unserialise (sexpr mu)
{
    return make_lambda (mu_type_identifier, mu);
}

static void mu_tag (sexpr mu)
{
}

static void mu_destroy (sexpr mu)
{
}

static void mu_call ( void )
{
}

static sexpr mu_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

static sexpr environment_serialise (sexpr env)
{
    struct environment *t = (struct environment *)env;

    return t->environment;
}

static sexpr environment_unserialise (sexpr env)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct environment));
    struct environment *rv = get_pool_mem (&pool);

    if (rv == (struct environment *)0)
    {
        return sx_nonexistent;
    }

    rv->type        = environment_type_identifier;
    rv->environment = env;

    return (sexpr)rv;
}

static void environment_tag (sexpr env)
{
}

static void environment_destroy (sexpr env)
{
}

static void environment_call ( void )
{
}

static sexpr environment_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

static sexpr primitive_serialise (sexpr op)
{
    struct primitive *p = (struct primitive *)op;

    switch (p->op)
    {
        case op_lambda:
            return cons (sym_lambda, sx_end_of_list);
        case op_mu:
            return cons (sym_mu, sx_end_of_list);
        case op_addition:
            return cons (sym_plus, sx_end_of_list);
        case op_subtraction:
            return cons (sym_minus, sx_end_of_list);
        case op_multiplication:
            return cons (sym_multiply, sx_end_of_list);
        case op_division:
            return cons (sym_divide, sx_end_of_list);
        case op_modulo:
            return cons (sym_modulo, sx_end_of_list);
    }

    return cons (sym_bad_primitive, sx_end_of_list);
}

static sexpr primitive_unserialise (sexpr op)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct primitive));
    enum primitive_ops rop;
    struct primitive *p;

    if (truep(equalp(sym_lambda, op)))
    {
        rop = op_lambda;
    }
    else if (truep(equalp(sym_mu, op)))
    {
        rop = op_mu;
    }
    else if (truep(equalp(sym_plus, op)))
    {
        rop = op_addition;
    }
    else if (truep(equalp(sym_minus, op)))
    {
        rop = op_subtraction;
    }
    else if (truep(equalp(sym_multiply, op)))
    {
        rop = op_multiplication;
    }
    else if (truep(equalp(sym_divide, op)))
    {
        rop = op_division;
    }
    else if (truep(equalp(sym_modulo, op)))
    {
        rop = op_modulo;
    }
    else
    {
        return sx_nonexistent;
    }

    p = get_pool_mem (&pool);
    if (p == (struct primitive *)0)
    {
        return sx_nonexistent;
    }
    p->type = primitive_type_identifier;
    p->op   = rop;

    return (sexpr)p;
}

static void primitive_tag (sexpr op)
{
}

static void primitive_destroy (sexpr op)
{
}

static void primitive_call ( void )
{
}

static sexpr primitive_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

static sexpr lx_apply_primitive (enum primitive_ops op, sexpr args, sexpr env)
{
    switch (op)
    {
        case op_lambda:
            return lx_lambda (args);
        case op_mu:
            return lx_mu (args);
        case op_addition:
        {
            int_pointer_s i = 0;

            for (i = sx_integer(car (args)), args = cdr (args);
                 consp (args); args = cdr (args))
            {
                i += sx_integer(car (args));
            }

            return make_integer(i);
        }
        case op_subtraction:
        {
            int_pointer_s i = 0;

            for (i = sx_integer(car (args)), args = cdr (args);
                 consp (args); args = cdr (args))
            {
                i -= sx_integer(car (args));
            }

            return make_integer(i);
        }
        case op_multiplication:
        {
            int_pointer_s i = 0;

            for (i = sx_integer(car (args)), args = cdr (args);
                 consp (args); args = cdr (args))
            {
                i *= sx_integer(car (args));
            }

            return make_integer(i);
        }
        case op_division:
        {
            int_pointer_s i = 0;

            for (i = sx_integer(car (args)), args = cdr (args);
                 consp (args); args = cdr (args))
            {
                i /= sx_integer(car (args));
            }

            return make_integer(i);
        }
        case op_modulo:
        {
            int_pointer_s i = 0;

            for (i = sx_integer(car (args)), args = cdr (args);
                 consp (args); args = cdr (args))
            {
                i %= sx_integer(car (args));
            }

            return make_integer(i);
        }
    }

    return args;
}

sexpr lx_apply (sexpr sx, sexpr args, sexpr env)
{
    if (lambdap (sx))
    {
        return sx;
    }
    else if (mup (sx))
    {
        return sx;
    }
    else if (primitivep (sx))
    {
        struct primitive *p = (struct primitive *)sx;

        return lx_apply_primitive (p->op, args, env);
    }

    return sx_nonexistent;
}

sexpr lx_eval (sexpr sx, sexpr env)
{
    while (consp (sx))
    {
        sexpr sxcar = car (sx);
        sexpr sxcdr = cdr (sx);

        if (symbolp (sxcar))
        {
            sx = primitive_unserialise (sxcar);

            if (nexp (sx))
            {
                sx = cons (lx_environment_lookup (env, sxcar), sxcdr);
            }
            else
            {
                sx = cons (sx, sxcdr);
            }
        }
        else if (lambdap (sxcar) || mup (sxcar) || primitivep (sxcar))
        {
            sx = lx_apply (sxcar, sxcdr, env);
        }
        else if (consp (sxcar))
        {
            sx = cons (lx_eval (sxcar, env), sxcdr);
        }
        else
        {
            return sx;
        }
    }

    return sx;
}

sexpr lx_make_environment ()
{
    return environment_unserialise (sx_end_of_list);
}

sexpr lx_environment_lookup (sexpr env, sexpr key)
{
    if (environmentp (env))
    {
        struct environment *t = (struct environment *)env;
        sexpr sx = t->environment;

        while (consp (sx))
        {
            sexpr sxt = car (sx);

            if (truep (equalp (car (sxt), key)))
            {
                return cdr (sxt);
            }

            sx = cdr (sx);
        }
    }

    return sx_nonexistent;
}
