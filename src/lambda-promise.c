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

static struct tree promise_tree        = TREE_INITIALISER;

static char initialised = 0;

static sexpr promise_serialise          (sexpr env);
static sexpr promise_unserialise        (sexpr env);
static void  promise_tag                (sexpr env);
static void  promise_destroy            (sexpr env);
static void  promise_call               ( void );
static sexpr promise_equalp             (sexpr a, sexpr b);

void initialise_seteh_promise ( void )
{
    if (!initialised)
    {
        sx_register_type
                (promise_type_identifier,
                 promise_serialise, promise_unserialise,
                 promise_tag, promise_destroy,
                 promise_call, promise_equalp);
    }
}

sexpr lx_make_promise (sexpr code, sexpr environment)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct promise));
    struct promise *p;
    struct tree_node *n;
    sexpr pt = cons (environment, code);

    if ((n = tree_get_node (&promise_tree, (int_pointer)pt)))
    {
        return (sexpr)node_get_value (n);
    }

    p = get_pool_mem (&pool);

    if (p == (struct promise *)0)
    {
        return sx_nonexistent;
    }

    p->type        = promise_type_identifier;
    p->ptype       = pt_manual;
    p->environment = environment;
    p->code        = code;

    tree_add_node_value (&promise_tree, (int_pointer)pt, (void *)p);

    return (sexpr)p;
}

sexpr lx_make_automatic_promise (sexpr code, sexpr environment)
{
    struct promise *p = (struct promise *)lx_make_promise (code, environment);

    if (p == (struct promise *)0)
    {
        return sx_nonexistent;
    }

    p->ptype       = pt_automatic;

    return (sexpr)p;
}

static sexpr promise_serialise (sexpr promise)
{
    struct promise *p = (struct promise *)promise;

    return cons (p->environment, p->code);
}

static sexpr promise_unserialise (sexpr promise)
{
    return lx_make_promise (car (promise), cdr (promise));
}

static void promise_tag (sexpr promise)
{
    struct promise *p = (struct promise *)promise;

    gc_tag (p->environment);
    gc_tag (p->code);
}

static void promise_destroy (sexpr promise)
{
    sexpr pt = promise_serialise (promise);

    free_pool_mem ((void *)promise);

    tree_remove_node(&promise_tree, (int_pointer)pt);
}

static void promise_call ( void )
{
    tree_map (&promise_tree, lx_sx_map_call, (void *)0);
}

static sexpr promise_equalp (sexpr a, sexpr b)
{
    return sx_false;
}
