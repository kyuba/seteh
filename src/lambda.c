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
                 (void *)0, (void *)0, (void *)0, primitive_equalp);
    }
}

static sexpr lx_code (sexpr *environment, sexpr args, sexpr code)
{
    sexpr rv = sx_end_of_list, n = args;

    while (consp (n))
    {
        *environment = lx_environment_unbind (*environment, car (n));

        n = car (n);
    }

    while (consp (code))
    {
        rv = cons (lx_eval (car (code), environment), rv);

        code = cdr (code);
    }

    return sx_reverse (rv);
}

static sexpr make_lambda (unsigned int type, sexpr args, sexpr env)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct lambda));
    struct lambda *lx = get_pool_mem (&pool);
    sexpr arguments, code, t, environment;

    if (lx == (struct lambda *)0)
    {
        return sx_nonexistent;
    }

    t = car (args);
    if (nexp (env))
    {
        if (environmentp (t))
        {
            environment = t;
            args = cdr (args);
        }
        else
        {
            environment = lx_make_environment (sx_end_of_list);
        }
    }
    else
    {
        environment = env;
    }

    arguments = car (args);
    code      = cdr (args);

    lx->type      = type;
    lx->arguments = 0;

    lx->arguments = arguments;
    lx->code = lx_code (&environment, arguments, code);
    lx->environment = environment;

    return (sexpr)lx;
}

sexpr lx_lambda (sexpr expression, sexpr environment)
{
    return make_lambda (lambda_type_identifier, expression, environment);
}

sexpr lx_mu (sexpr expression, sexpr environment)
{
    return make_lambda (mu_type_identifier, expression, environment);
}

static sexpr lambda_serialise (sexpr lambda)
{
    struct lambda *lx = (struct lambda *)lambda;

    return cons (lx->environment, cons (lx->arguments, lx->code));
}

static sexpr lambda_unserialise (sexpr lambda)
{
    return make_lambda (lambda_type_identifier, lambda, sx_nonexistent);
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
    return make_lambda (mu_type_identifier, mu, sx_nonexistent);
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
    return lx_make_environment (env);
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

#define primitive_serialise_case(op,sym) case op: return sym

static sexpr primitive_serialise (sexpr op)
{
    struct primitive *p = (struct primitive *)op;

    switch (p->op)
    {
        primitive_serialise_case (op_lambda,         sym_lambda);
        primitive_serialise_case (op_mu,             sym_mu);
        primitive_serialise_case (op_addition,       sym_plus);
        primitive_serialise_case (op_subtraction,    sym_minus);
        primitive_serialise_case (op_multiplication, sym_multiply);
        primitive_serialise_case (op_division,       sym_divide);
        primitive_serialise_case (op_modulo,         sym_modulo);
        primitive_serialise_case (op_dereference,    sym_dereference);
        primitive_serialise_case (op_unbound,        sym_unbound);
        primitive_serialise_case (op_equalp,         sym_equalp);
        primitive_serialise_case (op_if,             sym_if);
        primitive_serialise_case (op_gt,             sym_gt);
        primitive_serialise_case (op_gte,            sym_gte);
        primitive_serialise_case (op_lt,             sym_lt);
        primitive_serialise_case (op_lte,            sym_lte);
        primitive_serialise_case (op_equals,         sym_equals);
    }

    return sym_bad_primitive;
}

#define make_primitive_case(op) case op: return sx_p_ ## op

static sexpr make_primitive (enum primitive_ops rop)
{
    switch (rop)
    {
        make_primitive_case (op_lambda);
        make_primitive_case (op_mu);
        make_primitive_case (op_addition);
        make_primitive_case (op_subtraction);
        make_primitive_case (op_multiplication);
        make_primitive_case (op_division);
        make_primitive_case (op_modulo);
        make_primitive_case (op_dereference);
        make_primitive_case (op_unbound);
        make_primitive_case (op_equalp);
        make_primitive_case (op_if);
        make_primitive_case (op_gt);
        make_primitive_case (op_gte);
        make_primitive_case (op_lt);
        make_primitive_case (op_lte);
        make_primitive_case (op_equals);
    }

     return sx_nonexistent;
}

#define primitive_unserialise_map(s,cop,op) \
    if (truep(equalp(s, cop)))\
    {\
        return make_primitive(op);\
    }\

static sexpr primitive_unserialise (sexpr op)
{
    primitive_unserialise_map (sym_lambda,      op, op_lambda);
    primitive_unserialise_map (sym_mu,          op, op_mu);
    primitive_unserialise_map (sym_plus,        op, op_addition);
    primitive_unserialise_map (sym_minus,       op, op_subtraction);
    primitive_unserialise_map (sym_multiply,    op, op_multiplication);
    primitive_unserialise_map (sym_divide,      op, op_division);
    primitive_unserialise_map (sym_modulo,      op, op_modulo);
    primitive_unserialise_map (sym_dereference, op, op_dereference);
    primitive_unserialise_map (sym_unbound,     op, op_unbound);
    primitive_unserialise_map (sym_equalp,      op, op_equalp);
    primitive_unserialise_map (sym_if,          op, op_if);
    primitive_unserialise_map (sym_gt,          op, op_gt);
    primitive_unserialise_map (sym_gte,         op, op_gte);
    primitive_unserialise_map (sym_lt,          op, op_lt);
    primitive_unserialise_map (sym_lte,         op, op_lte);
    primitive_unserialise_map (sym_equals,      op, op_equals);

    primitive_unserialise_map (sym_if_alt,      op, op_if);

    return sx_nonexistent;
}

static sexpr primitive_equalp (sexpr a, sexpr b)
{
    struct primitive *pa = (struct primitive *)a;
    struct primitive *pb = (struct primitive *)b;

    return (pa->op == pb->op) ? sx_true : sx_false;
}

static sexpr lx_apply_primitive (enum primitive_ops op, sexpr args, sexpr *env)
{
    switch (op)
    {
        case op_lambda:
            return lx_lambda (args, *env);
        case op_mu:
            return lx_mu (args, *env);
        case op_addition:
        case op_subtraction:
        case op_multiplication:
        case op_division:
        case op_modulo:
        {
            int_pointer_s i = 0;
            sexpr ta = args, ti = lx_eval (car (ta), env);

            if (!integerp (ti))
            {
                return cons (make_primitive (op), args);
            }

            i = sx_integer(ti);
            ta = cdr (ta);

            while (consp (ta))
            {
                ti = lx_eval (car (ta), env);

                if (!integerp (ti))
                {
                    return cons (make_primitive (op), args);
                }

                switch (op)
                {
                    case op_addition:
                        i += sx_integer(ti);
                        break;
                    case op_subtraction:
                        i -= sx_integer(ti);
                        break;
                    case op_multiplication:
                        i *= sx_integer(ti);
                        break;
                    case op_division:
                        i /= sx_integer(ti);
                        break;
                    case op_modulo:
                        i %= sx_integer(ti);
                        break;
                    default:
                        break;
                }

                ta = cdr (ta);
            }

            return make_integer(i);
        }
        case op_dereference:
        {
            sexpr r = lx_environment_lookup (*env, args);

            return nexp (r) ? cons (make_primitive (op_dereference), args) : r;
        }
        case op_unbound:
        {
            return make_primitive (op_unbound);
        }
        case op_if:
        {
            return lx_eval
                        ((truep(lx_eval (car (args), env)) ?
                            car (cdr (args)) :
                            car (cdr (cdr (args)))),
                        env);
        }
        case op_equalp:
        {
            return equalp (car (args), car (cdr (args)));
        }
        case op_gt:
        {
            sexpr a = car (args), b = car (cdr (args));

            return (sx_integer(a) > sx_integer(b)) ? sx_true : sx_false;
        }
        case op_gte:
        {
            sexpr a = car (args), b = car (cdr (args));

            return (sx_integer(a) >= sx_integer(b)) ? sx_true : sx_false;
        }
        case op_lt:
        {
            sexpr a = car (args), b = car (cdr (args));

            return (sx_integer(a) < sx_integer(b)) ? sx_true : sx_false;
        }
        case op_lte:
        {
            sexpr a = car (args), b = car (cdr (args));

            return (sx_integer(a) <= sx_integer(b)) ? sx_true : sx_false;
        }
        case op_equals:
        {
            sexpr a = car (args), b = car (cdr (args));

            return (sx_integer(a) == sx_integer(b)) ? sx_true : sx_false;
        }
    }

    return args;
}

static sexpr lx_apply_lambda (sexpr argspec, sexpr code, sexpr env, sexpr args)
{
    sexpr t, tb = args, rv = sx_nonexistent, n = argspec;

    while (consp (tb))
    {
        t   = car (n);

        env = lx_environment_unbind (env, t);
        env = lx_environment_bind   (env, t, car (tb));

        tb  = cdr (tb);
        n   = cdr (n);
    }

/*    sx_write (stdio, env);*/

    t = code;

    while (consp (t))
    {
        rv = lx_eval (car (t), &env);

        t = cdr (t);
    }

    if (consp (n))
    {
        return lx_lambda (cons (n, cons (rv, sx_end_of_list)), env);
    }

    return rv;
}

sexpr lx_apply (sexpr sx, sexpr args, sexpr *env)
{
    if (lambdap (sx))
    {
        struct lambda *p = (struct lambda *)sx;
        sexpr s = lx_apply_lambda (p->arguments, p->code, *env, args);

        return nexp (s) ? sx : s;
    }
    else if (mup (sx))
    {
        return sx;
    }
    else if (primitivep (sx))
    {
        struct primitive *p = (struct primitive *)sx;

/*        sx_write (stdio, cons (*env, cons (sx, args)));*/

        sx = lx_apply_primitive (p->op, args, env);

/*        sx_write (stdio, sx);*/

        return sx;
    }

    return sx_nonexistent;
}

sexpr lx_eval (sexpr sx, sexpr *env)
{
    if (symbolp (sx))
    {
        sexpr sxt = primitive_unserialise (sx);

        sx = nexp (sxt) ? cons (make_primitive (op_dereference), sx) : sxt;
    }

    while (consp (sx))
    {
        sexpr sxcar = car (sx);
        sexpr sxcdr = cdr (sx);

        if (symbolp (sxcar))
        {
            sx = cons (lx_eval (sxcar, env), sxcdr);
        }
        else if (lambdap (sxcar) || mup (sxcar) || primitivep (sxcar))
        {
            sexpr r = lx_apply (sxcar, sxcdr, env);

            if (truep (equalp (r, sx)))
            {
                return sx;
            }

            sx = r;
        }
        else if (consp (sxcar))
        {
            sexpr r = cons (lx_eval (sxcar, env), sxcdr);

            if (truep (equalp (r, sx)))
            {
                return sx;
            }

            sx = r;
        }
        else
        {
            return sx;
        }
    }

    return sx;
}

sexpr lx_make_environment (sexpr env)
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

sexpr lx_environment_unbind (sexpr env, sexpr key)
{
    sexpr r = sx_end_of_list;

    if (environmentp (env))
    {
        struct environment *t = (struct environment *)env;
        sexpr sx = t->environment;

        while (consp (sx))
        {
            sexpr sxt = car (sx);

            if (falsep (equalp (car (sxt), key)))
            {
                r = cons (sxt, r);
            }

            sx = cdr (sx);
        }
    }

    return lx_make_environment (sx_reverse (r));
}

sexpr lx_environment_bind (sexpr env, sexpr key, sexpr value)
{
    sexpr r = sx_end_of_list;

    if (environmentp (env))
    {
        struct environment *t = (struct environment *)env;

        r = cons (cons(key, value), t->environment);
    }

    return lx_make_environment (r);
}
