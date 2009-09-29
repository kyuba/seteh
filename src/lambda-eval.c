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
#include <curie/gc.h>
#include <curie/string.h>

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

static char initialised = 0;

static sexpr primitive_serialise        (sexpr op);
static sexpr primitive_unserialise      (sexpr op);
static sexpr primitive_equalp           (sexpr a, sexpr b);

void initialise_seteh_eval ( void )
{
    if (!initialised)
    {
        sx_register_type
                (primitive_type_identifier,
                 primitive_serialise, primitive_unserialise,
                 (void *)0, (void *)0, (void *)0, primitive_equalp);
    }
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
        primitive_serialise_case (op_delay,          sym_delay);
        primitive_serialise_case (op_force,          sym_force);
        primitive_serialise_case (op_eval,           sym_eval);
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
        make_primitive_case (op_delay);
        make_primitive_case (op_force);
        make_primitive_case (op_eval);
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
    primitive_unserialise_map (sym_delay,       op, op_delay);
    primitive_unserialise_map (sym_force,       op, op_force);
    primitive_unserialise_map (sym_eval,        op, op_eval);

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
            sexpr ta = args, ti = lx_eval (car (ta), env, sx_end_of_list);

            if (!integerp (ti))
            {
                return cons (make_primitive (op), args);
            }

            i = sx_integer(ti);
            ta = cdr (ta);

            while (consp (ta))
            {
                ti = lx_eval (car (ta), env, sx_end_of_list);

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
            sexpr cond = lx_eval (car (args), env, sx_end_of_list);

            if (!truep (cond) && !falsep (cond))
            {
                return cons (make_primitive (op_if), args);

            }

            return truep(cond) ?
                            car (cdr (args)) :
                            car (cdr (cdr (args)));
        }
        case op_equalp:
            return equalp (lx_eval (car (args), env, sx_end_of_list),
                           lx_eval (car (cdr (args)), env, sx_end_of_list));
        case op_gt:
        case op_gte:
        case op_lt:
        case op_lte:
        case op_equals:
        {
            sexpr a = lx_eval (car (args), env, sx_end_of_list),
                  b = lx_eval (car (cdr (args)), env, sx_end_of_list);

            if (!integerp(a) || !integerp(b))
            {
                return cons (make_primitive (op),
                             cons (a,
                                   cons (b, sx_end_of_list)));
            }

            switch (op)
            {
                case op_gt:
                    return (sx_integer(a) > sx_integer(b))? sx_true : sx_false;
                case op_gte:
                    return (sx_integer(a) >= sx_integer(b))? sx_true : sx_false;
                case op_lt:
                    return (sx_integer(a) < sx_integer(b))? sx_true : sx_false;
                case op_lte:
                    return (sx_integer(a) <= sx_integer(b))? sx_true : sx_false;
                case op_equals:
                    return (sx_integer(a) == sx_integer(b))? sx_true : sx_false;
                default:
                    return sx_nonexistent;
            }
        }
        case op_delay:
            return lx_make_promise (args, *env);
        case op_force:
            if (promisep(args))
            {
                struct promise *p = (struct promise *)args;

                sexpr e = p->environment;
                return lx_eval (p->code, &e, sx_end_of_list);
            }
        case op_eval:
            return lx_eval (args, env, sx_end_of_list);
    }

    return args;
}

static sexpr lx_apply_lambda (sexpr argspec, sexpr code, sexpr env, sexpr args)
{
    sexpr t, e, tb = args, rv = sx_nonexistent, n = argspec;

    while (consp (tb))
    {
        t   = car (n);
        e   = lx_eval (car (tb), &env, sx_end_of_list);

        env = lx_environment_unbind (env, t);
        env = lx_environment_bind   (env, t, e);

        tb  = cdr (tb);
        n   = cdr (n);
    }

/*    sx_write (stdio, env);*/

    t = code;

    while (e = cdr (t), consp (e))
    {
        t = e;
    }

    if (consp (n))
    {
        return lx_lambda (cons (n, cons (rv, sx_end_of_list)), env);
    }
    else
    {
/*        rv = lx_eval (car (t), &env, sx_end_of_list);*/
        rv = lx_make_automatic_promise (car (t), env);
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
        struct lambda *p = (struct lambda *)sx;
        sexpr s = lx_apply_lambda (p->arguments, p->code, *env, args);

        return nexp (s) ? sx : s;
    }
    else if (flambdap (sx))
    {
        struct foreign_lambda *p = (struct foreign_lambda *)sx;

        return p->f (args, env);
    }
    else if (fmup (sx))
    {
        struct foreign_lambda *p = (struct foreign_lambda *)sx;

        return p->f (args, env);
    }
    else if (primitivep (sx))
    {
        struct primitive *p = (struct primitive *)sx;

/*        sx_write (stdio, cons (*env, cons (sx, args)));*/

        sx = lx_apply_primitive (p->op, args, env);

/*        sx_write (stdio, sx);*/

        return sx;
    }
    else if (promisep (sx))
    {
        struct promise *p = (struct promise *)sx;

        if (p->ptype == pt_automatic)
        {
            sexpr e = p->environment;
            return lx_eval (p->code, &e, sx_end_of_list);
        }

        return sx;
    }

    return sx_nonexistent;
}

sexpr lx_eval (sexpr sx, sexpr *env, sexpr cont)
{
    sexpr sxcar, sxcdr, r, e;

    while (consp (sx) || promisep (sx) || symbolp (sx))
    {
        if (symbolp (sx))
        {
            sexpr sxt = primitive_unserialise (sx);

            sx = nexp (sxt) ? cons (make_primitive (op_dereference), sx) : sxt;

            continue;
        }
        else if (promisep (sx))
        {
            struct promise *p = (struct promise *)sx;

            if (p->ptype == pt_automatic)
            {
                e = p->environment;
                env = &e;
                sx = p->code;
                continue;
            }

            return sx;
        }

        sxcar = car (sx);
        sxcdr = cdr (sx);

        if (symbolp (sxcar))
        {
            sx = cons (lx_eval (sxcar, env, cont), sxcdr);
        }
        else if (lambdap (sxcar)  || mup (sxcar)      || primitivep (sxcar) ||
                 promisep (sxcar))
        {
            r = lx_apply (sxcar, sxcdr, env);

            if (truep (equalp (r, sx)))
            {
                return sx;
            }

            sx = r;
        }
        else if (flambdap (sxcar) || fmup (sxcar))
        {
            return lx_apply (sxcar, sxcdr, env);
        }
        else if (consp (sxcar))
        {
            r = cons (lx_eval (sxcar, env, cont), sxcdr);

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
