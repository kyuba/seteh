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

#include <seteh/lambda-internal.h>
#include <curie/memory.h>
#include <curie/math.h>

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
define_primitive (op_equalp,         sym_equalp,      "equal?");
define_primitive (op_if,             sym_if,          "?");
define_primitive (op_gt,             sym_gt,          ">?");
define_primitive (op_gte,            sym_gte,         ">=?");
define_primitive (op_lt,             sym_lt,          "<?");
define_primitive (op_lte,            sym_lte,         "<=?");
define_primitive (op_equals,         sym_equals,      "=?");
define_primitive (op_eval,           sym_eval,        "eval");
define_primitive (op_quote,          sym_quote,       "quote");
define_primitive (op_list,           sym_list,        "list");
define_primitive (op_car,            sym_car,         "car");
define_primitive (op_cdr,            sym_cdr,         "cdr");

static sexpr primitive_serialise        (sexpr op);
static sexpr primitive_unserialise      (sexpr op);
static sexpr primitive_equalp           (sexpr a, sexpr b);

void initialise_seteh_eval ( void )
{
    static char initialised = 0;

    if (!initialised)
    {
        sx_register_type
                (primitive_type_identifier,
                 primitive_serialise, primitive_unserialise,
                 (void *)0, (void *)0, (void *)0, primitive_equalp);

        initialised = (char)1;
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
        primitive_serialise_case (op_equalp,         sym_equalp);
        primitive_serialise_case (op_if,             sym_if);
        primitive_serialise_case (op_gt,             sym_gt);
        primitive_serialise_case (op_gte,            sym_gte);
        primitive_serialise_case (op_lt,             sym_lt);
        primitive_serialise_case (op_lte,            sym_lte);
        primitive_serialise_case (op_equals,         sym_equals);
        primitive_serialise_case (op_eval,           sym_eval);
        primitive_serialise_case (op_quote,          sym_quote);
        primitive_serialise_case (op_list,           sym_list);
        primitive_serialise_case (op_car,            sym_car);
        primitive_serialise_case (op_cdr,            sym_cdr);
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
        make_primitive_case (op_equalp);
        make_primitive_case (op_if);
        make_primitive_case (op_gt);
        make_primitive_case (op_gte);
        make_primitive_case (op_lt);
        make_primitive_case (op_lte);
        make_primitive_case (op_equals);
        make_primitive_case (op_eval);
        make_primitive_case (op_quote);
        make_primitive_case (op_list);
        make_primitive_case (op_car);
        make_primitive_case (op_cdr);
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
    primitive_unserialise_map (sym_equalp,      op, op_equalp);
    primitive_unserialise_map (sym_if,          op, op_if);
    primitive_unserialise_map (sym_gt,          op, op_gt);
    primitive_unserialise_map (sym_gte,         op, op_gte);
    primitive_unserialise_map (sym_lt,          op, op_lt);
    primitive_unserialise_map (sym_lte,         op, op_lte);
    primitive_unserialise_map (sym_equals,      op, op_equals);
    primitive_unserialise_map (sym_eval,        op, op_eval);
    primitive_unserialise_map (sym_quote,       op, op_quote);
    primitive_unserialise_map (sym_list,        op, op_list);
    primitive_unserialise_map (sym_car,         op, op_car);
    primitive_unserialise_map (sym_cdr,         op, op_cdr);

    primitive_unserialise_map (sym_if_alt,      op, op_if);

    return sx_nonexistent;
}

static sexpr primitive_equalp (sexpr a, sexpr b)
{
    struct primitive *pa = (struct primitive *)a;
    struct primitive *pb = (struct primitive *)b;

    return (pa->op == pb->op) ? sx_true : sx_false;
}

static sexpr eval_atom (sexpr sx, sexpr environment)
{
    sexpr b;

    if (symbolp (sx))
    {
        b = lx_environment_lookup (environment, sx);
        if (nexp (b))
        {
            b = primitive_unserialise (sx);

            if (!nexp (b))
            {
                return b;
            }
        }
        else
        {
            return b;
        }
    }

    return sx;
}

static sexpr lx_apply_primitive
        (enum primitive_ops op, sexpr args, struct machine_state *s)
{
    switch (op)
    {
        case op_lambda:
            return lx_lambda (args, s->environment);
        case op_mu:
            return lx_mu (args, s->environment);
        case op_addition:
        case op_subtraction:
        case op_multiplication:
        case op_division:
        case op_modulo:
        {
            int_pointer   in = 0, n, g;
            int_pointer_s id = 1, d;
            sexpr ta = args, ti = car (ta);

            if (ninfp (ti) || pinfp (ti))
            {
                return ti;
            }
            else if (integerp (ti))
            {
                in = sx_integer (ti);
            }
            else if (rationalp (ti))
            {
                in = sx_numerator   (ti);
                id = sx_denominator (ti);
            }
            else
            {
                s->stack = cons (make_primitive (op), s->stack);
                return sx_nonexistent;
            }

            ta = cdr (ta);

            while (consp (ta))
            {
                ti = car (ta);

                if (ninfp (ti))
                {
                    switch (op)
                    {
                        case op_subtraction:
                            return sx_positive_infinity;
                        default:
                            return ti;
                    }
                }
                else if (pinfp (ti))
                {
                    switch (op)
                    {
                        case op_subtraction:
                            return sx_negative_infinity;
                        default:
                            return ti;
                    }
                }
                else if (integerp (ti))
                {
                    n = sx_integer (ti);
                    d = 1;
                }
                else if (rationalp (ti))
                {
                    n = sx_numerator   (ti);
                    d = sx_denominator (ti);
                }
                else
                {
                    s->stack = cons (make_primitive (op), s->stack);
                    return sx_nonexistent;
                }

                switch (op)
                {
                    case op_addition:
                        if (id == d)
                        {
                            in += n;
                        }
                        else
                        {
                            in *= d;
                            n  *= id;
                            id *= d;
                            in += n;
                        }
                        break;
                    case op_subtraction:
                        if (id == d)
                        {
                            in -= n;
                        }
                        else
                        {
                            in *= d;
                            n  *= id;
                            id *= d;
                            in -= n;
                        }
                        break;
                    case op_multiplication:
                        in *= n;
                        id *= d;
                        break;
                    case op_division:
                        in *= d;
                        id *= n;
                        break;
                    case op_modulo:
                        if (id == d)
                        {
                            in %= n;
                        }
                        else
                        {
                            in = 0;
                        }
                        break;
                    default:
                        break;
                }

                g = gcd (in, id);
                if (g > 1)
                {
                    in /= g;
                    id /= g;
                }

                ta = cdr (ta);
            }

            return make_rational (in, id);
        }
        case op_if:
        {
//            sexpr cond = lx_eval (car(args), s->environment);
            sexpr cond = car (args);

            if (!truep (cond) && !falsep (cond))
            {
                if (eolp (s->stack))
                {
                    s->stack = cons (make_primitive(op_if), s->stack);
                    s->code = cons (cond, cons (sx_quote, cons (car (cdr(args)),
                                    cons (sx_quote, cons (car (cdr (cdr(args))),
                                    sx_end_of_list)))));
                    return sx_nonexistent;
                }

                return cons (make_primitive (op_if), args);
            }

            s->stack = sx_end_of_list;
            s->code = truep(cond) ?
                            car (cdr (args)) :
                            car (cdr (cdr (args)));
            return sx_unquote;
        }
        case op_equalp:
            if (eolp (s->stack))
            {
                s->stack = cons (make_primitive(op_equalp), s->stack);
                return sx_nonexistent;
            }
            else
            {
//                sx_write (stdio, s);
//                sx_write (stdio, args);
                return equalp (car (args), car (cdr (args)));
            }
        case op_gt:
        case op_gte:
        case op_lt:
        case op_lte:
        case op_equals:
        {
            sexpr a = car (args), b = car (cdr (args));

            if (!integerp(a) || !integerp(b))
            {
                if (eolp (s->stack))
                {
                    s->stack = cons (make_primitive(op), s->stack);
                    return sx_nonexistent;
                }
                else
                {
                    return cons (make_primitive (op),
                                 cons (a, cons (b, sx_end_of_list)));
                }
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
                    return sx_nil;
            }
        }
        case op_eval:
            s->code = args;
            s->stack = sx_end_of_list;
            return sx_nonexistent;
        case op_quote:
            return car (args);
        case op_list:
            return args;
        case op_car:
        case op_cdr:
            if (eolp (s->stack))
            {
                s->stack = cons (make_primitive(op), s->stack);
                return sx_nonexistent;
            }

            if (op == op_car)
            {
                return car (car (args));
            }
            else
            {
                return cdr (car (args));
            }
    }

    return args;
}

static sexpr lx_simulate (struct machine_state *st)
{
    sexpr a, b;
    char reset;

//    gc_add_root ((sexpr*)st);

    do
    {
        reset = 0;

        if (consp (st->code))
        {
            if (eolp (st->stack))
            {
                a = eval_atom (car (st->code), st->environment);
                if (primitivep (a))
                {
                    struct primitive *p = (struct primitive *)a;
                    st->code = cdr (st->code);
                    b = lx_apply_primitive (p->op, st->code, st);
                  done_primitive_foreign:
                    if (nexp (b) || unquotep(b))
                    {
                        reset = 1;
                        continue;
                    }
                    else
                    {
                        st->stack = cons (b, sx_end_of_list);
                    }
                    goto done;
                }
                else if (consp (a))
                {
                  sub_application:
                    b = lx_make_state (st->stack, st->environment,
                                       cdr (st->code), st->dump);
                    st->dump = cons (b, st->dump);
                    st->stack = sx_end_of_list;
                    st->code = a;
                    reset = 1;
                    continue;
                }
                else if (flambdap (a) || fmup (a))
                {
                    struct foreign_lambda *l = (struct foreign_lambda *)a;
                    st->code = cdr (st->code);
                    if (l->f)
                    {
                        b = l->f (st->code, st);
                        goto done_primitive_foreign;
                    }
                }
                else if (quotep (a))
                {
                    st->code = cdr (st->code);
                    a = cons (st->code, sx_end_of_list);
                }

                st->stack = cons (a, st->stack);
                st->code = cdr (st->code);
            }

            while (consp (st->code))
            {
                a = car (st->code);

                if (symbolp (a))
                {
                    st->stack = cons (eval_atom (a, st->environment),st->stack);
                }
                else if (consp (a))
                {
                    goto sub_application;
                }
                else if (quotep (a))
                {
                    st->code = cdr (st->code);
                    st->stack = cons (car (st->code), st->stack);
                }
                else
                {
                    st->stack = cons (a, st->stack);
                }

                st->code = cdr (st->code);
            }
        }
        else if (!eolp (st->code))
        {
            st->stack = cons (eval_atom (st->code, st->environment), st->stack);
        }

        if (!eolp (st->stack))
        {
            a = sx_reverse (st->stack);
            b = car (a);

            if (primitivep (b))
            {
                struct primitive *p = (struct primitive *)b;
                b = lx_apply_primitive (p->op, cdr (a), st);
                if (unquotep (b))
                {
                    reset = 1;
                    continue;
                }
                else if (nexp (b))
                {
                    b = a;
                }

                st->stack = cons (b, sx_end_of_list);
            }
            else if (lambdap (b) || mup (b))
            {
                struct lambda *l = (struct lambda *)b;
                sexpr t, tb = cdr (a), n = l->arguments;

                st->environment =
                        lx_environment_join (st->environment, l->environment);

                while (consp (tb))
                {
                    t  = car (n);

                    st->environment =
                            lx_environment_unbind (st->environment, t);
                    st->environment =
                            lx_environment_bind   (st->environment, t, car(tb));

                    tb = cdr (tb);
                    n  = cdr (n);
                }

                if (consp (n))
                {
                    st->stack =
                            cons (lx_lambda (cons (n, l->code),st->environment),
                                  sx_end_of_list);
                }
                else
                {
                    st->code  = l->code;
                    st->stack = sx_end_of_list;

                    reset = 1;
                    continue;
                }
            }
            else if (flambdap (b) || fmup (b))
            {
                struct foreign_lambda *l = (struct foreign_lambda *)b;
                if (l->f)
                {
                    b = l->f (cdr (a), st);
                    if (unquotep (b))
                    {
                        reset = 1;
                        continue;
                    }
                    else if (!nexp (b))
                    {
                        st->stack = cons (b, sx_end_of_list);
                    }
                }
            }
        }

      done:

        if (consp (st->dump))
        {
            sexpr new_state = car (st->dump);
            if (mstatep (new_state))
            {
                struct machine_state *m = (struct machine_state *)new_state;
                st->stack = cons (car (st->stack), m->stack);
                st->environment = m->environment;
                st->code = m->code;
                st->dump = m->dump;

                reset = 1;
            }
        }
    }
    while (reset == (char)1);

//    gc_remove_root ((sexpr*)st);

    return car (st->stack);
}

sexpr lx_eval (sexpr sx, sexpr env)
{
    struct machine_state st =
     { machine_state_type_identifier, sx_end_of_list, env, sx, sx_end_of_list };

    return lx_simulate (&st);
}

sexpr lx_apply (sexpr sx, sexpr args, sexpr env)
{
    struct machine_state st =
    { machine_state_type_identifier, sx_end_of_list, env, cons (sx, args),
      sx_end_of_list };

    return lx_simulate (&st);
}

sexpr lx_continue (sexpr continuation)
{
    if (mstatep (continuation))
    {
        struct machine_state *stc = (struct machine_state *)continuation;
        struct machine_state st =
          { machine_state_type_identifier, stc->stack, stc->environment,
            stc->code, stc->dump };

        return lx_simulate (&st);
    }

    return sx_nonexistent;
}
