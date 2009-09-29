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

static struct tree environment_tree    = TREE_INITIALISER;

static char initialised = 0;

static sexpr environment_serialise      (sexpr env);
static sexpr environment_unserialise    (sexpr env);
static void  environment_tag            (sexpr env);
static void  environment_destroy        (sexpr env);
static void  environment_call           ( void );
static sexpr environment_equalp         (sexpr a, sexpr b);

void initialise_seteh_environment ( void )
{
    if (!initialised)
    {
        sx_register_type
                (environment_type_identifier,
                 environment_serialise, environment_unserialise,
                 environment_tag, environment_destroy,
                 environment_call, environment_equalp);
    }
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
    struct environment *t = (struct environment *)env;

    gc_tag (t->environment);
}

static void environment_destroy (sexpr env)
{
    struct environment *t = (struct environment *)env;
    sexpr tn = t->environment;

    free_pool_mem ((void *)env);

    tree_remove_node(&environment_tree, (int_pointer)tn);
}

static void environment_call ( void )
{
    tree_map (&environment_tree, lx_sx_map_call, (void *)0);
}

static sexpr environment_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

sexpr lx_make_environment (sexpr env)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct environment));
    struct environment *rv;
    struct tree_node *n;

    if ((n = tree_get_node (&environment_tree, (int_pointer)env)))
    {
        return (sexpr)node_get_value (n);
    }

    rv = get_pool_mem (&pool);

    if (rv == (struct environment *)0)
    {
        return sx_nonexistent;
    }

    rv->type        = environment_type_identifier;
    rv->environment = env;

    tree_add_node_value (&environment_tree, (int_pointer)env, (void *)rv);

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

    return lx_make_environment (r);
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

sexpr lx_environment_join (sexpr a, sexpr b)
{
    sexpr r = sx_end_of_list;

    if (environmentp (a) && environmentp (b))
    {
        struct environment *ta = (struct environment *)a;
        struct environment *tb = (struct environment *)b;
        sexpr c = tb->environment;

        while (consp (c))
        {
            sexpr d  = car (c);
            sexpr da = car (d);
            ta = (struct environment *)lx_environment_unbind ((sexpr)ta, da);
            ta = (struct environment *)lx_environment_bind ((sexpr)ta, da, cdr (d));
            c = cdr (c);
        }

        return (sexpr)ta;
    }

    return lx_make_environment (r);
}

sexpr lx_environment_alist (sexpr env)
{
    if (environmentp (env))
    {
        struct environment *tenv = (struct environment *)env;

        return tenv->environment;
    }

    return sx_end_of_list;
}

