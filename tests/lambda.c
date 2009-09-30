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

struct sexpr_io *stdio;

define_symbol (sym_f, "f");

sexpr f (sexpr args, struct machine_state *environment)
{
    return cons (make_string ("wheeeeoooo"), args);
}

int cmain ()
{
    struct io *in       = io_open_read        ("tests/data/lambda-1.sx");
    struct sexpr_io *io = sx_open_io          (in, io_open_null);
    sexpr sx, env       = lx_make_environment (sx_end_of_list);
    int rv = 1;

    stdio               = sx_open_stdout      ();

    initialise_seteh ();

    env = lx_environment_bind (env, make_symbol ("f"), make_integer (2));
    env = lx_environment_bind (env, make_symbol ("fx"),
                               lx_foreign_lambda (sym_f, f));

    while (!eofp(sx = sx_read (io)))
    {
        if (!nexp (sx))
        {
            sexpr eval = lx_eval (sx, &env, sx_end_of_list);

            if (truep(equalp (eval, make_integer (42))))
            {
                rv = 0;
            }

/*            sx_write (stdio, sx);*/
            sx_write (stdio, eval);
/*            sx_write (stdio, env);*/
        }
    }

    return rv;
}
