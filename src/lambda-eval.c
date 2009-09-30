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
            int_pointer_s i = 0;
            sexpr ta = args, ti = car (ta);

            if (!integerp (ti))
            {
                s->stack = cons (make_primitive (op), s->stack);
                return sx_nonexistent;
            }

            i = sx_integer(ti);
            ta = cdr (ta);

            while (consp (ta))
            {
                ti = car (ta);

                if (!integerp (ti))
                {
                    s->stack = cons (make_primitive (op), s->stack);
                    return sx_nonexistent;
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
        case op_if:
        {
            sexpr cond = lx_eval (car(args), &(s->environment), sx_end_of_list);

            if (!truep (cond) && !falsep (cond))
            {
                return cons (make_primitive (op_if), args);

            }

            return truep(cond) ?
                            car (cdr (args)) :
                            car (cdr (cdr (args)));
        }
        case op_equalp:
            return equalp (lx_eval (car (args), &(s->environment), sx_end_of_list),
                           lx_eval (car (cdr (args)), &(s->environment), sx_end_of_list));
        case op_gt:
        case op_gte:
        case op_lt:
        case op_lte:
        case op_equals:
        {
            sexpr a = lx_eval (car (args), &(s->environment), sx_end_of_list),
                  b = lx_eval (car (cdr (args)), &(s->environment), sx_end_of_list);

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
                    return sx_nil;
            }
        }
        case op_delay:
            return lx_make_promise (args, s->environment);
        case op_force:
            if (promisep(args))
            {
                struct promise *p = (struct promise *)args;

                sexpr e = p->environment;
                return lx_eval (p->code, &e, sx_end_of_list);
            }
        case op_eval:
            return lx_eval (args, &(s->environment), sx_end_of_list);
    }

    return args;
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

sexpr lx_eval (sexpr sx, sexpr *env, sexpr cont)
{
    struct machine_state st =
        { machine_state_type_identifier, sx_end_of_list, *env, sx, cont };
    sexpr a, b;
    char reset;

//    sx_write (stdio, &st);

    do
    {
        reset = 0;
//        sx_write (stdio, &st);

        if (consp (st.code))
        {
            if (eolp (st.stack))
            {
                a = eval_atom (car (st.code), st.environment);
                if (primitivep (a))
                {
                    struct primitive *p = (struct primitive *)a;
                    st.code = cdr (st.code);
//                    sx_write (stdio, &st);
                    b = lx_apply_primitive (p->op, st.code, &st);
                  done_primitive_foreign:
                    if (nexp (b))
                    {
//                        sx_write (stdio, &st);
                        reset = 1;
                        continue;
                    }
                    else
                    {
                        st.stack = cons (b, sx_end_of_list);
                    }
                    goto done;
                }
                else if (consp (a))
                {
                  sub_application:
                    b = lx_make_state (st.stack, st.environment, cdr (st.code), st.dump);
                    st.dump = cons (b, st.dump);
                    st.stack = sx_end_of_list;
                    st.code = a;
                    reset = 1;
                    continue;
                }
                else if (flambdap (a) || fmup (a))
                {
                    struct foreign_lambda *l = (struct foreign_lambda *)a;
                    st.code = cdr (st.code);
                    if (l->f)
                    {
                        b = l->f (st.code, &st);
                        goto done_primitive_foreign;
                    }
                }
                else
                {
                    st.stack = cons (a, st.stack);
                    st.code = cdr (st.code);
                }
            }

            while (consp (st.code))
            {
                a = car (st.code);
                st.code = cdr (st.code);

                if (symbolp (a))
                {
                    st.stack = cons (eval_atom (a, st.environment), st.stack);
                }
                else if (consp (a))
                {
                    goto sub_application;
                }
                else
                {
                    st.stack = cons (a, st.stack);
                }
            }
        }
        else if (!eolp (st.code))
        {
            st.stack = cons (eval_atom (st.code, st.environment), st.stack);
        }

        if (!eolp (st.stack))
        {
            a = sx_reverse (st.stack);
            b = car (a);

            if (primitivep (b))
            {
                struct primitive *p = (struct primitive *)b;
                b = lx_apply_primitive (p->op, cdr (a), &st);
                if (nexp (b))
                {
                    b = a; 
                }

                st.stack = cons (b, sx_end_of_list);
            }
            else if (lambdap (b) || mup (b))
            {
                struct lambda *l = (struct lambda *)b;
                sexpr t, tb = cdr (a), n = l->arguments;

                st.environment =
                        lx_environment_join (st.environment, l->environment);

//                sx_write (stdio, &st);

                while (consp (tb))
                {
                    t  = car (n);

                    st.environment =
                            lx_environment_unbind (st.environment, t);
                    st.environment =
                            lx_environment_bind   (st.environment, t, car (tb));

                    tb = cdr (tb);
                    n  = cdr (n);
                }

//                sx_write (stdio, &st);

                if (consp (n))
                {
                    st.stack =
                            cons (lx_lambda (cons (n, l->code), st.environment),
                                  sx_end_of_list);

//                    sx_write (stdio, &st);
                }
                else
                {
                    st.code  = l->code;
                    st.stack = sx_end_of_list;

                    reset = 1;
//                    sx_write (stdio, &st);
                    continue;
                }
            }
            else if (flambdap (b) || fmup (b))
            {
                struct foreign_lambda *l = (struct foreign_lambda *)b;
                if (l->f)
                {
                    b = l->f (cdr (a), &st);
                    if (!nexp (b))
                    {
                        st.stack = cons (b, sx_end_of_list);
                    }
                }
            }
        }

      done:

        if (consp (st.dump))
        {
            sexpr new_state = car (st.dump);
            if (mstatep (new_state))
            {
                struct machine_state *m = (struct machine_state *)new_state;
                st.stack = cons (car (st.stack), m->stack);
                st.environment = m->environment;
                st.code = m->code;
                st.dump = m->dump;

                reset = 1;
            }
        }
    }
    while (reset == (char)1);

//    sx_write (stdio, &st);

    return car (st.stack);
}
