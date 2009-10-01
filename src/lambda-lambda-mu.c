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
#include <curie/string.h>

static struct tree lambda_tree         = TREE_INITIALISER;
static struct tree mu_tree             = TREE_INITIALISER;
static struct tree foreign_lambda_tree = TREE_INITIALISER;
static struct tree foreign_mu_tree     = TREE_INITIALISER;

static char initialised = 0;

static sexpr lambda_serialise           (sexpr lambda);
static sexpr lambda_unserialise         (sexpr lambda);
static void  lambda_tag                 (sexpr lambda);
static void  lambda_destroy             (sexpr lambda);
static void  lambda_call                ( void );
static sexpr lambda_equalp              (sexpr a, sexpr b);

static sexpr mu_serialise               (sexpr mu);
static sexpr mu_unserialise             (sexpr mu);
static void  mu_tag                     (sexpr mu);
static void  mu_destroy                 (sexpr mu);
static void  mu_call                    ( void );
static sexpr mu_equalp                  (sexpr a, sexpr b);

static sexpr foreign_lambda_serialise   (sexpr lambda);
static sexpr foreign_lambda_unserialise (sexpr lambda);
static void  foreign_lambda_tag         (sexpr lambda);
static void  foreign_lambda_destroy     (sexpr lambda);
static void  foreign_lambda_call        ( void );
static sexpr foreign_lambda_equalp      (sexpr a, sexpr b);

static sexpr foreign_mu_serialise       (sexpr mu);
static sexpr foreign_mu_unserialise     (sexpr mu);
static void  foreign_mu_tag             (sexpr mu);
static void  foreign_mu_destroy         (sexpr mu);
static void  foreign_mu_call            ( void );
static sexpr foreign_mu_equalp          (sexpr a, sexpr b);

void initialise_seteh_lambda ( void )
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
                (foreign_lambda_type_identifier,
                 foreign_lambda_serialise, foreign_lambda_unserialise,
                 foreign_lambda_tag, foreign_lambda_destroy,
                 foreign_lambda_call, foreign_lambda_equalp);

        sx_register_type
                (foreign_mu_type_identifier,
                 foreign_mu_serialise, foreign_mu_unserialise,
                 foreign_mu_tag, foreign_mu_destroy,
                 foreign_mu_call, foreign_mu_equalp);
    }
}

static sexpr lx_code (sexpr *environment, sexpr args, sexpr code)
{
    sexpr n = args, t, e;

    while (consp (n))
    {
        *environment = lx_environment_unbind (*environment, car (n));

        n = car (n);
    }

    t = code;

    while (e = cdr (t), consp (e))
    {
        t = e;
    }

//    return cons (lx_eval (car (t), environment, sx_end_of_list),sx_end_of_list);
//    return cons (car(t), sx_end_of_list);
    return code;
}

static sexpr make_lambda (unsigned int type, sexpr args, sexpr env)
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct lambda));
    struct lambda *lx;
    sexpr arguments, code, t, environment, na, nc, ne, u;
    struct tree_node *n;

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

    na = arguments;
    nc = lx_code (&environment, arguments, code);
    ne = environment;

    u = cons (ne, cons (na, nc));

    if ((n = tree_get_node (((type == lambda_type_identifier) ? &lambda_tree
                                                              : &mu_tree),
                            (int_pointer)u)))
    {
        return (sexpr)node_get_value (n);
    }

    lx = get_pool_mem (&pool);

    if (lx == (struct lambda *)0)
    {
        return sx_nonexistent;
    }

    lx->type      = type;
    lx->arguments = 0;

    lx->arguments   = na;
    lx->code        = nc;
    lx->environment = ne;

    tree_add_node_value
            (((type == lambda_type_identifier) ? &lambda_tree : &mu_tree),
             (int_pointer)u, (void *)lx);

    return (sexpr)lx;
}

sexpr lx_foreign_lambda (sexpr name, sexpr (*f)(sexpr, struct machine_state *))
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct foreign_lambda));
    struct foreign_lambda *lx;
    unsigned long len;
    int_32 hash = str_hash (sx_symbol (name), &len);
    struct tree_node *n;

    if ((n = tree_get_node (&foreign_lambda_tree, (int_pointer)hash)))
    {
        lx = (struct foreign_lambda *)node_get_value (n);

        if (f != (void *)0)
        {
            lx->f = f;
        }

        return (sexpr)lx;
    }

    lx = get_pool_mem (&pool);

    lx->type = foreign_lambda_type_identifier;
    lx->name = name;
    lx->f    = f;

    tree_add_node_value (&foreign_lambda_tree, (int_pointer)hash, (void *)lx);

    return (sexpr)lx;
}

sexpr lx_foreign_mu (sexpr name, sexpr (*f)(sexpr, struct machine_state *))
{
    static struct memory_pool pool
            = MEMORY_POOL_INITIALISER(sizeof (struct foreign_lambda));
    struct foreign_lambda *lx;
    unsigned long len;
    int_32 hash = str_hash (sx_symbol (name), &len);
    struct tree_node *n;

    if ((n = tree_get_node (&foreign_mu_tree, (int_pointer)hash)))
    {
        lx = (struct foreign_lambda *)node_get_value (n);

        if (f != (void *)0)
        {
            lx->f = f;
        }

        return (sexpr)lx;
    }

    lx = get_pool_mem (&pool);

    lx->type = foreign_mu_type_identifier;
    lx->name = name;
    lx->f    = f;

    tree_add_node_value (&foreign_mu_tree, (int_pointer)hash, (void *)lx);

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
    gc_tag (lambda_serialise (lambda));
}

static void lambda_destroy (sexpr lambda)
{
    sexpr t = lambda_serialise (lambda);

    free_pool_mem ((void *)lambda);

    tree_remove_node(&lambda_tree, (int_pointer)t);
}

static void lambda_call ( void )
{
    tree_map (&lambda_tree, lx_sx_map_call, (void *)0);
}

static sexpr lambda_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

static sexpr foreign_lambda_serialise (sexpr lambda)
{
    struct foreign_lambda *lx = (struct foreign_lambda *)lambda;

    return lx->name;
}

static sexpr foreign_lambda_unserialise (sexpr lambda)
{
    return lx_foreign_lambda (lambda, (void *)0);
}

static void foreign_lambda_tag (sexpr lambda)
{
    struct foreign_lambda *lx = (struct foreign_lambda *)lambda;

    gc_tag (lx->name);
}

static void foreign_lambda_destroy (sexpr lambda)
{
    struct foreign_lambda *lx = (struct foreign_lambda *)lambda;
    sexpr t = lx->name;

    free_pool_mem ((void *)lambda);

    tree_remove_node(&foreign_lambda_tree, (int_pointer)t);
}

static void foreign_lambda_call ( void )
{
    tree_map (&foreign_lambda_tree, lx_sx_map_call, (void *)0);
}

static sexpr foreign_lambda_equalp (sexpr a, sexpr b)
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
    struct lambda *sx = (struct lambda *)mu;

    gc_tag (sx->arguments);
    gc_tag (sx->code);
    gc_tag (sx->environment);
}

static void mu_destroy (sexpr mu)
{
    sexpr t = lambda_serialise (mu);

    free_pool_mem ((void *)mu);

    tree_remove_node(&mu_tree, (int_pointer)t);
}

static void mu_call ( void )
{
    tree_map (&mu_tree, lx_sx_map_call, (void *)0);
}

static sexpr mu_equalp (sexpr a, sexpr b)
{
    return sx_false;
}

static sexpr foreign_mu_serialise (sexpr lambda)
{
    struct foreign_lambda *lx = (struct foreign_lambda *)lambda;

    return lx->name;
}

static sexpr foreign_mu_unserialise (sexpr lambda)
{
    return lx_foreign_mu (lambda, (void *)0);
}

static void foreign_mu_tag (sexpr lambda)
{
    struct foreign_lambda *lx = (struct foreign_lambda *)lambda;

    gc_tag (lx->name);
}

static void foreign_mu_destroy (sexpr lambda)
{
    struct foreign_lambda *lx = (struct foreign_lambda *)lambda;
    sexpr t = lx->name;

    free_pool_mem ((void *)lambda);

    tree_remove_node(&foreign_mu_tree, (int_pointer)t);
}

static void foreign_mu_call ( void )
{
    tree_map (&foreign_mu_tree, lx_sx_map_call, (void *)0);
}

static sexpr foreign_mu_equalp (sexpr a, sexpr b)
{
    return sx_false;
}
