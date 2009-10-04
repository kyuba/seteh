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
#include <curie/gc.h>

static struct tree state_tree          = TREE_INITIALISER;

static sexpr state_serialise            (sexpr secd);
static sexpr state_unserialise          (sexpr secd);
static void  state_tag                  (sexpr secd);
static void  state_destroy              (sexpr secd);
static void  state_call                 ( void );
static sexpr state_equalp               (sexpr a, sexpr b);

void initialise_seteh_state ( void )
{
    static char initialised = 0;

    if (!initialised)
    {
        sx_register_type
                (machine_state_type_identifier,
                 state_serialise, state_unserialise,
                 state_tag, state_destroy,
                 state_call, state_equalp);

        initialised = (char)1;
    }
}

static sexpr state_serialise (sexpr secd)
{
    struct machine_state *t = (struct machine_state *)secd;
    return cons (t->stack,
                 cons (t->environment,
                       cons(t->code,
                            cons(t->dump, sx_end_of_list))));
}

static sexpr state_unserialise (sexpr secd)
{
    sexpr s   = car (secd);
    sexpr sd  = cdr (secd);
    sexpr e   = car (sd);
    sexpr sdd = cdr (sd);
    sexpr c   = car (sdd);
    sexpr d   = car (cdr (sdd));

    return lx_make_state (s, e, c, d);
}

static void state_tag (sexpr secd)
{
    gc_tag (state_serialise (secd));
}

static void state_destroy (sexpr secd)
{
    sexpr tn = state_serialise (secd);

    free_pool_mem ((void *)secd);

    tree_remove_node(&state_tree, (int_pointer)tn);
}

static void state_call ( void )
{
    tree_map (&state_tree, lx_sx_map_call, (void *)0);
}

static sexpr state_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

sexpr lx_make_state (sexpr s, sexpr e, sexpr c, sexpr d)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct machine_state));
    struct machine_state *rv;
    struct tree_node *n;

    sexpr t = cons (s, cons (e, cons (c, cons (d, sx_end_of_list))));

    if ((n = tree_get_node (&state_tree, (int_pointer)t)))
    {
        return (sexpr)node_get_value (n);
    }

    rv = get_pool_mem (&pool);

    if (rv == (struct machine_state *)0)
    {
        return sx_nonexistent;
    }

    rv->type        = machine_state_type_identifier;
    rv->stack       = s;
    rv->environment = e;
    rv->code        = c;
    rv->dump        = d;

    tree_add_node_value (&state_tree, (int_pointer)t, (void *)rv);

    return (sexpr)rv;
}
